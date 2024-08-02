module Core where

import Clash.Prelude

type Address = Unsigned 16
type NumOfNodesToStore = Unsigned 3
type NumOfEdgesToStore = Unsigned 3
type IdOfPort = Unsigned 2

data NodeType = Dup | Erasure
  deriving (Generic, NFDataX)

data CoreState = Empty | OneNodeToLoad
  deriving (Generic, NFDataX)

data Port = Port {
    _targetAddress :: Address
    , _edgeIsVisited :: Bit
  }
  deriving (Generic, NFDataX)

data Node ports = Node {
    _ports :: Vec ports Port
    , _nodeType :: NodeType
  }
  deriving (Generic, NFDataX)

data LoadedNode ports = LoadedNode {
    _node :: Node ports
    , _originalAddress :: Address
  }
  deriving (Generic, NFDataX)

data Edge = Edge{
    _leftPortValue :: Port
    , _addressOfLeftPort :: Address
    , _idOfLeftPort :: IdOfPort
    , _rightPortValue :: Port
    , _addressOfRightPort :: Address
    , _idOfRightPort :: IdOfPort
  }
  deriving (Generic, NFDataX)

data DataToStore = DataToStore{
    _nodes :: Vec max_num_of_nodes_to_store LoadedNode
    , _edges :: Vec max_num_of_edges_to_store Edge
    , _actualNumOfNodesToStore :: NumOfNodesToStore
    , _actualNumOfEdgesToStore :: NumOfEdgesToStore
  }
  deriving (Generic, NFDataX)

reducer ::
 (KnownDomain dom
 , HiddenClockResetEnable dom)
 => Signal dom LoadedNode
 -> Signal dom DataToStore
reducer node =
  where
    leftNode = register None node
    handle leftNode rightNode =

      let newLeftNode = Node

      let selectEdgeToLoad node =
        let ports = _node $ _ports node in
        if not $ _edgeIsVisited $ ports !! 0
          then ports !! 0
          else find !!! $ tail ports

    mealyF state (leftNode, rightNode) =
      case state of
        Empty -> (OneNodeToLoad, Nothing)
        OneNodeToLoad -> (OneNodeToLoad, Just $ handle leftNode rightNode)
