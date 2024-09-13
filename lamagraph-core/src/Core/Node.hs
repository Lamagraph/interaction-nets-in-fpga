module Core.Node where

import Clash.Prelude
import Data.Maybe (isJust)

type Address = Unsigned 16

-- | Pointer on port of the `Node` with visited flag.
data Port = Port
  { _targetAddress :: Address
  , _edgeIsVisited :: Bit
  }
  deriving (NFDataX, Generic)

-- | Node in the RAM.
data Node numberOfPorts = Node
  { _primaryPort :: Port
  , _secondaryPorts :: Vec numberOfPorts (Maybe Port)
  -- _nodeType :: INNode looks like we need some kind of node label. Info about and reduction rules contained IN
  }
  deriving (NFDataX, Generic)

-- | `Node` with info about his`Address`.
data LoadedNode numberOfPorts = LoadedNode
  { _node :: Node numberOfPorts
  , _originalAddress :: Address
  }
  deriving (NFDataX, Generic)

-- | Check if `Port` is not visited yet and there is no collision with `Node` address.
isPortToLoad ::
  (KnownNat numberOfPorts) =>
  LoadedNode numberOfPorts ->
  Port ->
  Bool
isPortToLoad loadedNode port =
  not (bitToBool $ _edgeIsVisited port)
    && (_originalAddress loadedNode /= _targetAddress port)

-- | Mark all `Port` in nodes as visited, if it has pointer at given nodes.
markAllInnerEdges ::
  (KnownNat maxNumOfNodesToStore) =>
  -- | Starting vector of nodes.
  Vec maxNumOfNodesToStore (Maybe (LoadedNode numberOfPorts)) ->
  -- | Marked vector of nodes.
  Vec maxNumOfNodesToStore (Maybe (LoadedNode numberOfPorts))
markAllInnerEdges nodes =
  let addressesOfLoadedNodes = map (fmap _originalAddress) nodes
      markPort port =
        Port
          (_targetAddress port)
          ( boolToBit
              $ bitToBool (_edgeIsVisited port)
              || isJust (elemIndex (Just (_targetAddress port)) addressesOfLoadedNodes)
          )
      markPorts loadedNode =
        LoadedNode
          ( Node
              (markPort $ _primaryPort $ _node loadedNode)
              (map (fmap markPort) $ _secondaryPorts $ _node loadedNode)
          )
          (_originalAddress loadedNode)
   in map (fmap markPorts) nodes

-- | Check if pair of `LoadedNode` are active, i.e. they are connected by primary ports.
isActive ::
  LoadedNode numberOfPorts ->
  LoadedNode numberOfPorts ->
  Bool
isActive leftNode rightNode =
  leftNodePrimaryPortAddress
    == _originalAddress rightNode
    && rightNodePrimaryPortAddress
    == _originalAddress leftNode
 where
  leftNodePrimaryPortAddress = _targetAddress (_primaryPort $ _node leftNode)
  rightNodePrimaryPortAddress = _targetAddress (_primaryPort $ _node rightNode)

{- | Select an `Address` among those node's ports that can be loaded.
It is always check primary port first.
-}
selectAddressToLoad ::
  (KnownNat numberOfPorts) =>
  LoadedNode numberOfPorts ->
  Maybe Address
selectAddressToLoad loadedNode =
  if isPortToLoad loadedNode primaryPort
    then Just $ _targetAddress primaryPort
    else
      foldl (\s mbPort -> mbPort >>= addressToLoad s) Nothing
        $ _secondaryPorts node
 where
  node = _node loadedNode
  primaryPort = _primaryPort node
  addressToLoad s port = if isPortToLoad loadedNode port then Just $ _targetAddress port else s
