module Core where

import Clash.Prelude
import Data.Maybe (isJust)

type Address = Unsigned 16
type NumOfNodesToStore = Unsigned 3
type NumOfEdgesToStore = Unsigned 3
type IdOfPort = Unsigned 3

data NodeType = Dup | Erasure
  deriving (Generic, NFDataX)

data ReducerStatus = Work | Finished | Error
  deriving (Generic, NFDataX)

data ReducerState = Initial | Empty | OneNodeToLoad
  deriving (Generic, NFDataX)

data Port = Port {
    _targetAddress :: Address
    , _edgeIsVisited :: Bit
  }
  deriving (Generic, NFDataX)

data MaybePort = Connected Port | NotConnected | PNothing
  deriving (Generic, NFDataX)

data Node number_of_ports = Node {
    _ports :: Vec number_of_ports (Maybe Port)
    , _nodeType :: NodeType
  }
  deriving (Generic, NFDataX)

data LoadedNode number_of_ports = LoadedNode {
    _node :: Node number_of_ports
    , _originalAddress :: Address
  }
  deriving (Generic, NFDataX)

data EdgeEnd = EdgeEnd{
    _addressOfVertex :: Address
    , _idOfPort :: IdOfPort
  }
  deriving (Generic, NFDataX)

data Edge = Edge{
    _leftEnd :: EdgeEnd
    , _rightEnd :: EdgeEnd
  }
  deriving (Generic, NFDataX)

data DataToStore max_num_of_nodes_to_store max_num_of_edges_to_store number_of_ports = DataToStore{
    _nodes :: Vec max_num_of_nodes_to_store (Maybe (LoadedNode number_of_ports))
    , _edges :: Vec max_num_of_edges_to_store (Maybe Edge)
  }
  deriving (Generic, NFDataX)

loader ::
 (KnownDomain dom
 , HiddenClockResetEnable dom)
 => (Signal dom Address -> Signal dom Node)
 -> Signal dom (Maybe Address)
 -> Signal dom (Maybe LoadedNode)
loader ram addressToLoad =
  mkLoadedNode <$> bundle (ram addressToLoad) registerForAddressToLoad
  where
    mkLoadedNode (node, address) = fmap (LoadedNode node) address
    registerForAddressToLoad = register Nothing addressToLoad

reducer ::
 (KnownDomain dom
 , HiddenClockResetEnable dom
 , KnownNat max_num_of_nodes_to_store
 , KnownNat max_num_of_edges_to_store
 , KnownNat number_of_ports)
 => Address
 -> (Signal dom Maybe(Address) -> Signal dom Maybe(LoadedNode number_of_ports))
 -> Signal dom (DataToStore max_num_of_nodes_to_store max_num_of_edges_to_store number_of_ports, ReducerStatus)
reducer addressOfFirstNodeToLoad nodeLoader =
  dataToStore
  where
    (dataToStore, nextLeftNode, addressOfNextNodeToLoad) = 
      unbundle 
      $ mealy mealyF Initial 
      $ bundle leftNode (nodeLoader addressOfNextNodeToLoad)
    
    leftNode = register Nothing nextLeftNode    
    
    handle 
      :: Maybe (LoadedNode number_of_ports)
      -> Maybe (LoadedNode number_of_ports)
      -> (DataToStore max_num_of_nodes_to_store max_num_of_edges_to_store number_of_ports)
    handle leftNode rightNode = 
      let emptyNodes = repeat Nothing :: Vec max_num_of_nodes_to_store _
      in 
      let emptyEdges = repeat Nothing :: Vec max_num_of_edges_to_store _
      in
      let nodes = 
        if isActive leftNode loadedNode
          then 
            case leftNode of
              Nothing -> DataToStore emptyNodes emptyEdges -- Error must be here
              Just n1 -> rightNode +>> leftNode +>> emptyNodes -- reduction rules applied here                  
          else rightNode +>> leftNode +>> emptyNodes
      in
      DataToStore emptyEdges (markAllInnerEdges nodes)

    isPortToLoad
      :: Maybe Port 
      -> Bool
    isPortToLoad port = 
      case port of 
        Nothing -> False
        Just port -> (not $ _edgeIsVisited port) 
          && (_originalAddress loadedNode /= _targetAddress port) 

    selectAddressToLoad 
      :: LoadedNode number_of_ports
      -> Maybe Address
    selectAddressToLoad loadedNode = 
      let ports = _ports $ _node loadedNode in
      if not $ _edgeIsVisited $ ports !! 0 -- 0 is a main port
        then Just $ _targetAddress $ ports !! 0
        else foldl (\ s p -> if isPortToLoad p then Just p else s) Nothing $ tail ports

    selectNextLeftNode
      :: DataToStore max_num_of_nodes_to_store max_num_of_edges_to_store number_of_ports 
      -> (Maybe (LoadedNode number_of_ports, Address), DataToStore max_num_of_nodes_to_store max_num_of_edges_to_store number_of_ports)
    selectNextLeftNode  preDataToStore =
      let handleNode s i node =
        case selectAddressToLoad node of
          Nothing -> s
          Just a -> Just (i, node, a)
      let selectedNode = ifoldl handleNode Nothing $ _nodes preDataToStore
      case selectedNode of
        Nothing -> Nothing, preDataToStore
        Just (i, node, addressOfNodeToLoad) -> 
            Just (node, addressOfNodeToLoad), DataToStore (replace i Nothing $ _nodes preDataToStore) (_edges preDataToStore)

    markAllInnerEdges
      :: Vec (max_num_of_nodes_to_store + 1) (Maybe (LoadedNode number_of_ports))
      -> Vec (max_num_of_nodes_to_store + 1) (Maybe (LoadedNode number_of_ports))
    markAllInnerEdges nodes =
      let addressesOfLoadedNodes = map (fmap _originalAddress) nodes in
      let markPorts node =
        let targetAddress = _targetAddress port
        Node (_nodeType node) (map (\port -> Port targetAddress (_edgeIsVisited port || isJust $ elemIndex (Just targetAddress) addressesOfLoadedNodes)) $ _ports node)
      map (fmap markPorts) nodes

    isActive
      :: Maybe (LoadedNode number_of_ports)
      -> Maybe (LoadedNode number_of_ports)
      -> Bool
    isActive leftNode rightNode =
      case leftNode of
        Nothing -> False
        Just n1 -> 
          case rightNode of 
            Nothing -> False 
            Just n2 -> 
              _targetAddress ((_ports $ _node n1) !! 0) == _originalAddress n2
              && _targetAddress ((_ports $ _node n2) !! 0) == _originalAddress n1

    mealyF
      :: ReducerState 
      -> (Maybe (LoadedNode number_of_ports), Maybe (LoadedNode number_of_ports)) 
      -> (ReducerState, (Maybe (DataToStore max_num_of_nodes_to_store max_num_of_edges_to_store number_of_ports), Maybe (LoadedNode number_of_ports), Maybe Address))
    mealyF state (leftNode, loadedNode) =
      case state of
        Initial -> (Empty, (Nothing, Nothing, Just addressOfFirstNodeToLoad))
        Empty ->
          let addressToLoad = selectAddressToLoad loadedNode in
          case addressToLoad of
            Just x -> (OneNodeToLoad, (Nothing, loadedNode, addressToLoad))
            Nothing -> reduce loadedNode Nothing  -- !!HANDLE!!
        OneNodeToLoad -> 
          let preDataToStore = handle leftNode loadedNode in 
          let nextData, dataToStore = selectNextLeftNode preDataToStore in
          case nextData of
            Just (nextLeftNode, nextAddressToLoad) -> (OneNodeToLoad, (dataToStore, Just nextLeftNode, Just nextAddressToLoad))
            Nothing -> (Empty, (dataToStore, Nothing, Just addressOfNodeWithUnvisitedEdges))
