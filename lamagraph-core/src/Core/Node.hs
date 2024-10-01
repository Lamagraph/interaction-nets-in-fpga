{-# LANGUAGE TemplateHaskell #-}

module Core.Node where

import Clash.Prelude
import Control.Lens (makeLenses, (^.))

type AddressNumber = Unsigned 16

data Address = ActualAddress AddressNumber | LocalAddress AddressNumber
  deriving (NFDataX, Generic, Show, Eq)

newtype Port = Port (Maybe Address)
  deriving (NFDataX, Generic, Show, Eq)

-- | Node in the RAM.
data Node numberOfPorts = Node
  { _primaryPort :: Port
  , _secondaryPorts :: Vec numberOfPorts (Maybe Port)
  -- _nodeType :: INNode looks like we need some kind of node label. Info about and reduction rules contained IN
  }
  deriving (NFDataX, Generic, Show, Eq)

$(makeLenses ''Node)

{- | `Node` with info about his`Address`.
Original address can be useful when reducer working.
For example, if this `Node` has reduced then his `Address` is become free
and info about this should be passed to the memory manager.
-}
data LoadedNode (portsNumber :: Nat) = LoadedNode
  { _containedNode :: Node portsNumber
  , _originalAddress :: Address
  }
  deriving (NFDataX, Generic, Show, Eq)

$(makeLenses ''LoadedNode)

-- | Analog of `LoadedNode` with local address. Redundant, just for simplification of signatures.
data LocalNode (portsNumber :: Nat) = LocalNode
  { _localAddress :: Address
  , _numberedNode :: Node portsNumber
  }
  deriving (NFDataX, Generic, Show, Eq)

$(makeLenses ''LocalNode)

-- | Check if pair of `LoadedNode` are active, i.e. they are connected by primary ports.
isActive ::
  LoadedNode numberOfPorts ->
  LoadedNode numberOfPorts ->
  Bool
isActive leftNode rightNode =
  leftNodePrimaryPortAddress == Just (rightNode ^. originalAddress)
    && rightNodePrimaryPortAddress == Just (leftNode ^. originalAddress)
 where
  Port leftNodePrimaryPortAddress = leftNode ^. containedNode . primaryPort
  Port rightNodePrimaryPortAddress = rightNode ^. containedNode . primaryPort
