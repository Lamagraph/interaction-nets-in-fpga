{-# LANGUAGE TemplateHaskell #-}

module Core.Node where

import Clash.Prelude
import Control.Lens (makeLenses, (^.))
import Data.Maybe (isJust)

type Address = Unsigned 16

data Port = Port
  { _targetAddress :: Address
  , _edgeIsVisited :: Bool
  }
  deriving (NFDataX, Generic)

$(makeLenses ''Port)

-- | Node in the RAM.
data Node numberOfPorts = Node
  { _primaryPort :: Port
  , _secondaryPorts :: Vec numberOfPorts (Maybe Port)
  -- _nodeType :: INNode looks like we need some kind of node label. Info about and reduction rules contained IN
  }
  deriving (NFDataX, Generic)

$(makeLenses ''Node)

{- | `Node` with info about his`Address`.
Original address can be useful when reducer working.
For example, if this `Node` has reduced then his `Addres  s` is become free
and info about this should be passed to the memory manager.
-}
data LoadedNode numberOfPorts = LoadedNode
  { _containedNode :: Node numberOfPorts
  , _originalAddress :: Address
  }
  deriving (NFDataX, Generic)

$(makeLenses ''LoadedNode)

-- | Check if `Port` is not visited yet and there is no collision with `Node` address.
isPortToLoad ::
  (KnownNat numberOfPorts) =>
  LoadedNode numberOfPorts ->
  Port ->
  Bool
isPortToLoad loadedNode port =
  not (port ^. edgeIsVisited)
    && (loadedNode ^. originalAddress /= port ^. targetAddress)

-- | Mark all `Port` in nodes as visited, if it has pointer at given nodes.
markAllInnerEdges ::
  (KnownNat maxNumOfNodesToStore) =>
  -- | Starting vector of nodes.
  Vec maxNumOfNodesToStore (Maybe (LoadedNode numberOfPorts)) ->
  -- | Marked vector of nodes.
  Vec maxNumOfNodesToStore (Maybe (LoadedNode numberOfPorts))
markAllInnerEdges nodes =
  let addressesOfLoadedNodes = map (fmap (^. originalAddress)) nodes
      markPort port =
        Port
          (port ^. targetAddress)
          ((port ^. edgeIsVisited) || isJust (elemIndex (Just (port ^. targetAddress)) addressesOfLoadedNodes))
      markPorts loadedNode =
        LoadedNode
          ( Node
              (markPort $ loadedNode ^. containedNode . primaryPort)
              (map (fmap markPort) $ loadedNode ^. containedNode . secondaryPorts)
          )
          (loadedNode ^. originalAddress)
   in map (fmap markPorts) nodes

-- | Check if pair of `LoadedNode` are active, i.e. they are connected by primary ports.
isActive ::
  LoadedNode numberOfPorts ->
  LoadedNode numberOfPorts ->
  Bool
isActive leftNode rightNode =
  leftNodePrimaryPortAddress == rightNode ^. originalAddress
    && rightNodePrimaryPortAddress == leftNode ^. originalAddress
 where
  leftNodePrimaryPortAddress = leftNode ^. containedNode . primaryPort . targetAddress
  rightNodePrimaryPortAddress = rightNode ^. containedNode . primaryPort . targetAddress

{- | Select an `Address` among those node's ports that can be loaded.
It is always check primary port first.
-}
selectAddressToLoad ::
  (KnownNat numberOfPorts) =>
  LoadedNode numberOfPorts ->
  Maybe Address
selectAddressToLoad loadedNode =
  if isPortToLoad loadedNode primPort
    then Just $ primPort ^. targetAddress
    else
      foldl (\s mbPort -> mbPort >>= addressToLoad s) Nothing $ node ^. secondaryPorts
 where
  node = loadedNode ^. containedNode
  primPort = node ^. primaryPort
  addressToLoad s port = if isPortToLoad loadedNode port then Just $ port ^. targetAddress else s
