{-# LANGUAGE PartialTypeSignatures #-}
{-# OPTIONS_GHC -Wno-partial-type-signatures #-}

module Core where

import Clash.Prelude
import Data.Maybe (isJust)

type Address = Unsigned 16
type NumOfNodesToStore = Unsigned 3
type NumOfEdgesToStore = Unsigned 3
type IdOfPort = Unsigned 3

data NodeType = Dup | Erasure
  deriving (Generic, NFDataX)

data ReducerStatus = Work | Finished | ErrorHandleSingleNode | ErrorEmptyLeftNode
  deriving (Generic, NFDataX)

data ReducerState = Initial | Empty | OneNodeToLoad | Done | Failed 
  deriving (Generic, NFDataX)

data Port = Port {
    _targetAddress :: Address
    , _edgeIsVisited :: Bit
  }
  deriving (Generic, NFDataX)

data MaybePort = Connected Port | NotConnected | PNothing
  deriving (Generic, NFDataX)

data Node number_of_ports = Node {
    _primaryPort :: Port
    , _secondaryPorts :: Vec number_of_ports (Maybe Port)
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
 , HiddenClockResetEnable dom
 , KnownNat number_of_ports)
 => (Signal dom Address -> Signal dom (Node number_of_ports))
 -> Signal dom (Maybe Address)
 -> Signal dom (Maybe (LoadedNode number_of_ports))
loader ram addressToLoad =
  mkLoadedNode <$> bundle ((ram $ (\n -> case n of 
                                          Nothing -> 0 
                                          Just x -> x) <$> addressToLoad), registerForAddressToLoad)
  where
    mkLoadedNode (node, address) = fmap (LoadedNode node) address
    registerForAddressToLoad = register Nothing addressToLoad

--memoryManager 
--  :: (KnownNat number_of_cells) 
--  => Vec number_of_cells Bit
--  -> Vec number_of_cells Bit
--  -> ()
--memoryManager isFreeMap isFullyVisitedMap = 


reducer ::
 (KnownDomain dom
 , HiddenClockResetEnable dom
 , KnownNat max_num_of_nodes_to_store
 , KnownNat max_num_of_edges_to_store
 , KnownNat number_of_ports)
 => Address
 -> (Signal dom (Maybe Address) -> Signal dom (Maybe (LoadedNode number_of_ports)))
 -> (Signal dom Bit -> Signal dom (Maybe Address))
 -> Signal dom (Maybe (DataToStore max_num_of_nodes_to_store max_num_of_edges_to_store number_of_ports), ReducerStatus)
reducer addressOfFirstNodeToLoad nodeLoader nonvisitedNodesProvider =
  bundle (dataToStore, reducerStatus)
  where
    (dataToStore, reducerStatus, nextLeftNode, addressOfNextNodeToLoad, isNonvisitedNodeUsed) = 
      unbundle 
      $ mealy mealyF Initial 
      $ bundle (leftNode, (nodeLoader addressOfNextNodeToLoad), status, (nonvisitedNodesProvider isNonvisitedNodeUsed))
    
    leftNode = register Nothing selectNextLeftNode

    status = register Work reducerStatus
    
    handle 
      :: LoadedNode number_of_ports
      -> Maybe (LoadedNode number_of_ports)
      -> (Maybe (LoadedNode number_of_ports, Address),DataToStore max_num_of_nodes_to_store max_num_of_edges_to_store number_of_ports)
    handle leftNode rightNode = 
      let emptyNodes = repeat Nothing :: Vec max_num_of_nodes_to_store _
      in 
      let emptyEdges = repeat Nothing :: Vec max_num_of_edges_to_store _
      in
      let nodes = case rightNode of
                    Nothing -> Just leftNode +>> emptyNodes
                    Just n -> 
                      if isActive leftNode n
                        then rightNode +>> (Just leftNode) +>> emptyNodes -- reduction rules applied here
                        else rightNode +>> (Just leftNode) +>> emptyNodes 
      in
      selectNextLeftNode $ DataToStore (markAllInnerEdges nodes) emptyEdges

    isPortToLoad 
      :: (KnownNat number_of_ports)
      => LoadedNode number_of_ports
      -> Port 
      -> Bool
    isPortToLoad loadedNode port =
      (not $ bitToBool $ _edgeIsVisited port) 
      && (_originalAddress loadedNode /= _targetAddress port)

    selectAddressToLoad 
      :: LoadedNode number_of_ports
      -> Maybe Address
    selectAddressToLoad loadedNode = 
      let node = _node loadedNode in
      let addressToLoad s port = if isPortToLoad loadedNode port then Just $ _targetAddress port else s       
      in
      if isPortToLoad loadedNode $ _primaryPort node
        then Just $ _targetAddress $ _primaryPort node
        else 
          foldl (\s mbPort -> mbPort >>= addressToLoad s) Nothing 
          $ _secondaryPorts node

    selectNextLeftNode
      :: DataToStore max_num_of_nodes_to_store max_num_of_edges_to_store number_of_ports 
      -> (Maybe (LoadedNode number_of_ports, Address), DataToStore max_num_of_nodes_to_store max_num_of_edges_to_store number_of_ports)
    selectNextLeftNode  preDataToStore =
      let handleNode s i mbNode = mbNode >>= (\node -> case selectAddressToLoad node of
                                                        Nothing -> s
                                                        Just a -> Just (i, node, a))
      in
      let selectedNode = ifoldl handleNode Nothing $ _nodes preDataToStore
      in
      case selectedNode of
        Nothing -> (Nothing, preDataToStore)
        Just (i, node, addressOfNodeToLoad) -> 
            (Just (node, addressOfNodeToLoad), DataToStore (replace i Nothing $ _nodes preDataToStore) (_edges preDataToStore))

    markAllInnerEdges
      :: Vec max_num_of_nodes_to_store (Maybe (LoadedNode number_of_ports))
      -> Vec max_num_of_nodes_to_store (Maybe (LoadedNode number_of_ports))
    markAllInnerEdges nodes =
      let addressesOfLoadedNodes = map (fmap _originalAddress) nodes in
      let markPort port = Port (_targetAddress port) 
                                (boolToBit ((bitToBool $ _edgeIsVisited port) || (isJust $ elemIndex (Just (_targetAddress port)) addressesOfLoadedNodes)))
      in
      let markPorts loadedNode = LoadedNode (Node (markPort $ _primaryPort $ _node loadedNode)
                                                  (map (fmap markPort) $ _secondaryPorts $ _node loadedNode)
                                                  (_nodeType $ _node loadedNode)) 
                                            (_originalAddress loadedNode)        
      in
      map (fmap markPorts) nodes

    isActive
      :: LoadedNode number_of_ports
      -> LoadedNode number_of_ports
      -> Bool
    isActive leftNode rightNode = 
      (_targetAddress $ _primaryPort $ _node leftNode) == (_originalAddress rightNode)
      && (_targetAddress $ _primaryPort $ _node rightNode) == (_originalAddress leftNode)

    mealyF
      :: ReducerState 
      -> (Maybe (LoadedNode number_of_ports), Maybe (LoadedNode number_of_ports))
      -> ReducerStatus
      -> (Maybe Address) 
      -> (ReducerState, (Maybe (DataToStore max_num_of_nodes_to_store max_num_of_edges_to_store number_of_ports), ReducerStatus, Maybe (LoadedNode number_of_ports), Maybe Address, Bit))
    mealyF state (leftNode, loadedNode) status addressOfNodeWithUnvisitedEdges =
      case state of
        Initial -> (Empty, (Nothing, Work, Nothing, Just addressOfFirstNodeToLoad))
        Empty ->
          case loadedNode of 
            Nothing -> (Failed, (Nothing, Error, Nothing, Nothing))
            Just loadedNode ->
              let addressToLoad = selectAddressToLoad loadedNode in
              case addressToLoad of
                Just x -> (OneNodeToLoad, (Nothing, Work, Just loadedNode, addressToLoad))
                Nothing -> 
                  case leftNode of
                    Just leftNode -> 
                      let (infoAboutNextNodes, dataToStore) = handle leftNode Nothing in
                      case infoAboutNextNodes of
                        Just (nextLeftNode, nextAddressToLoad) -> (Failed, (Nothing, ErrorHandleSingleNode, Nothing, Nothing))
                        Nothing -> (Empty, (dataToStore, Work, Nothing, Just addressOfNodeWithUnvisitedEdges))
                    Nothing -> (Failed, (Nothing, ErrorEmptyLeftNode, Nothing, Nothing))
        OneNodeToLoad -> 
          case leftNode of
            Just leftNode -> 
              let (infoAboutNextNodes, dataToStore) = handle leftNode loadedNode in
              case infoAboutNextNodes of
                Just (nextLeftNode, nextAddressToLoad) -> (OneNodeToLoad, (dataToStore, Work, Just nextLeftNode, Just nextAddressToLoad))
                Nothing -> (Empty, (dataToStore, Work, Nothing, Just addressOfNodeWithUnvisitedEdges))
            Nothing -> (Failed, (Nothing, ErrorEmptyLeftNode, Nothing, Nothing))
        Failed -> (Failed, (Nothing, status, Nothing, Nothing))
