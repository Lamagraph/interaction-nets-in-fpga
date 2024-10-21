{-# LANGUAGE TemplateHaskell #-}

module Core.Node where

import Clash.Prelude
import Control.Lens (makeLenses, (^.))

type AddressNumber = Unsigned 16

data IdOfPort (portsNumber :: Nat) = Id (Index portsNumber) | Primary
  deriving (Generic, Show, Eq, NFDataX) -- Index numberOfPorts

data Address = ActualAddress AddressNumber | LocalAddress AddressNumber
  deriving (NFDataX, Generic, Show, Eq)

instance Default Address where
  def :: Address
  def = ActualAddress (def :: AddressNumber)

data Port (portsNumber :: Nat) = Port
  { _nodeAddress :: Maybe Address
  , _portConnectedToId :: IdOfPort portsNumber
  }
  deriving (NFDataX, Generic, Show, Eq)

$(makeLenses ''Port)

-- | Node in the RAM.
data Node portsNumber = Node
  { _primaryPort :: Port portsNumber
  , _secondaryPorts :: Vec portsNumber (Maybe (Port portsNumber))
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
  Port leftNodePrimaryPortAddress _ = leftNode ^. containedNode . primaryPort
  Port rightNodePrimaryPortAddress _ = rightNode ^. containedNode . primaryPort
