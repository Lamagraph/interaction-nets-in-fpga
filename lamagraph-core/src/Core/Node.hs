{-# LANGUAGE GADTs #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE TemplateHaskell #-}

{- |
Module      : Core.Node
Description : Net data types shared by every layer

The vocabulary types of the interaction net stored in RAM: @Node@, @Edge@,
@Port@ and the address aliases. Everything above the memory level —
"Core.MemoryManager.MemoryManager", "Core.Reducer", "Core.CPU" — manipulates
these types and nothing else, so this module depends only on "Clash.Prelude"
and lens.

Fixed sizes: a cell address is `AddressBitSize` = 4 bits wide
(`AddressNumber`), so the whole net holds `CellsNumber` = 16 nodes; the
number of secondary ports is a type-level parameter of every type here.
-}
module Core.Node where

import Clash.Prelude
import Control.Lens (makeLenses, (^.))

{- | Type alias for `Maybe Port` with aliased constructors too.
This can be useful for distinguish connected to something @Port@, free @Port@ that is not connected and void @Port@.
The last one come from situation when max number of ports (i.e. max arity) greater than arity of the given @Node@.
-}
type Connection (portsNumber :: Nat) = Maybe (Port portsNumber)

pattern Connected :: Port portsNumber -> Connection portsNumber
pattern Connected x = Just x

pattern NotConnected :: Connection portsNumber
pattern NotConnected = Nothing
{-# COMPLETE Connected, NotConnected #-}

type AddressBitSize = 4
type AddressNumber = Unsigned AddressBitSize
type CellsNumber = 2 ^ AddressBitSize

-- | Identifies one port of a node: the principal one (`Primary`) or a secondary one by index (`Id i`).
data IdOfPort (portsNumber :: Nat) = Id (Index portsNumber) | Primary
  deriving (Generic, Show, Eq, NFDataX, ShowX)

{- | Reference to a concrete port of a concrete node: address plus which port of that node.
Wiring is symmetric by convention: if @Port a (Id j)@ points somewhere, the node at @a@
keeps the mirror reference @... (Id (i - j))@ back — see "Core.Concrete.Initial" for
the fixture encoding.
-}
data Port (portsNumber :: Nat) = Port
  { _nodeAddress :: AddressNumber
  , _portConnectedToId :: IdOfPort portsNumber
  }
  deriving (NFDataX, Generic, Show, Eq, ShowX)

$(makeLenses ''Port)

{- | Node in the RAM, without its address.

The secondary ports carry two layers of `Maybe`: the outer one marks whether
the slot exists at all (an agent whose arity is lower than @portsNumber@ keeps
@Nothing@ padding slots), the inner `Connection` says whether an existing slot
is wired (@Connected@) or free (@NotConnected@). A node is active iff its
principal port is wired to another principal port (@nodeIsActive@).
-}
data Node portsNumber agentType = Node
  { _primaryPort :: Connection portsNumber
  , _secondaryPorts :: Vec portsNumber (Maybe (Connection portsNumber))
  , _nodeType :: agentType -- ^ Agent label; the reduction rules are selected by it
  }
  deriving (NFDataX, Generic, Show, Eq, ShowX)

$(makeLenses ''Node)

{- | @Node@ with info about his@Address@.
Original address can be useful when reducer working.
For example, if this @Node@ has reduced then his @Address@ is become free
and info about this should be passed to the memory manager.
-}
data LoadedNode (portsNumber :: Nat) agentType = LoadedNode
  { _containedNode :: Node portsNumber agentType
  , _originalAddress :: AddressNumber
  }
  deriving (NFDataX, Generic, Show, Eq, ShowX)

$(makeLenses ''LoadedNode)

-- | A wire between two ends. Either end may be `NotConnected` (a dangling end left by a reduction step).
data Edge (portsNumber :: Nat) = Edge
  { _leftEnd :: Connection portsNumber
  , _rightEnd :: Connection portsNumber
  }
  deriving (Generic, NFDataX, Show, Eq)

$(makeLenses ''Edge)

-- | Check if @Node@ is active: its principal port is connected to a port that is itself principal (`Primary`).
nodeIsActive ::
  (KnownNat portsNumber) => Node portsNumber agentType -> Bool
nodeIsActive node =
  case node ^. primaryPort of
    Just port -> case port ^. portConnectedToId of
      Primary -> True
      _ -> False
    _ -> False

-- | All `Connection`s of a @Node@: the principal one first, then the secondary slots in order. Length is @portsNumber + 1@.
getAllConnections ::
  (KnownNat portsNumber) =>
  Node portsNumber agentType ->
  Vec (portsNumber + 1) (Maybe (Connection portsNumber))
getAllConnections node = Just (node ^. primaryPort) :> (node ^. secondaryPorts)

{- | Look up the port a given `Connection` points at and return the connection stored there.

`NotConnected` input yields `Nothing`. An out-of-range `Id` index is a programming
error (bounds error of the underlying `Vec`), not a runtime check.
-}
findConnection ::
  (KnownNat portsNumber) =>
  Node portsNumber agentType ->
  Connection portsNumber ->
  Maybe (Connection portsNumber)
findConnection (Node primPort secPorts _) = \case
  NotConnected -> Nothing
  Connected (Port _ portId) ->
    case portId of
      Primary -> Just primPort
      Id i -> secPorts !! i
