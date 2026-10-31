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
import qualified Prelude as P

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
  , _nodeType :: agentType
  -- ^ Agent label; the reduction rules are selected by it
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

-- | The connection a node stores at one of its ports; @Nothing@ is an absent slot.
connectionAt ::
  (KnownNat portsNumber) =>
  Node portsNumber agentType ->
  IdOfPort portsNumber ->
  Maybe (Connection portsNumber)
connectionAt (Node primPort secPorts _) = \case
  Primary -> Just primPort
  Id i -> secPorts !! i

{- | Look up the port a given `Connection` points at and return the connection stored there.

`NotConnected` input yields `Nothing`. An out-of-range `Id` index is a programming
error (bounds error of the underlying `Vec`), not a runtime check.
-}
findConnection ::
  (KnownNat portsNumber) =>
  Node portsNumber agentType ->
  Connection portsNumber ->
  Maybe (Connection portsNumber)
findConnection node = \case
  NotConnected -> Nothing
  Connected (Port _ portId) -> connectionAt node portId

{- | One side of a wire that is not mirrored back.

`_errorSourcePort` holds the referencing port, `_errorTargetPort` the port it
points at. `_errorTargetConnection` is what that target port actually contains:
`Nothing` when the target is not a live cell or the slot is padding,
`Just connection` when it holds a different connection.
-}
data SymmetryError (portsNumber :: Nat) = SymmetryError
  { _errorSourcePort :: Port portsNumber
  , _errorTargetPort :: Port portsNumber
  , _errorTargetConnection :: Maybe (Connection portsNumber)
  }
  deriving (Show, Eq, Generic)

{- | Look a referenced address up in the set being checked: @Nothing@ = outside the
set (external, skipped), @Just Nothing@ = inside the universe but holds no node.
-}
type TargetLookup portsNumber agentType =
  AddressNumber -> Maybe (Maybe (Node portsNumber agentType))

-- | Check every connection of one node against the mirror stored on the other side.
checkNodeSymmetry ::
  forall portsNumber agentType.
  (KnownNat portsNumber) =>
  TargetLookup portsNumber agentType ->
  AddressNumber ->
  Node portsNumber agentType ->
  [SymmetryError portsNumber]
checkNodeSymmetry lookupTarget sourceAddress node = primaryErrors P.++ secondaryErrors
 where
  checkConnection sourcePortId = \case
    NotConnected -> []
    Connected targetPort -> case lookupTarget (targetPort ^. nodeAddress) of
      Nothing -> [] -- external to the checked set
      Just maybeTargetNode ->
        let mirror = maybeTargetNode >>= (`connectionAt` (targetPort ^. portConnectedToId))
            expected = Connected $ Port sourceAddress sourcePortId
            sourcePort = Port sourceAddress sourcePortId
         in [SymmetryError sourcePort targetPort mirror | mirror /= Just expected]
  primaryErrors = checkConnection Primary (node ^. primaryPort)
  secondaryErrors =
    ifoldl
      ( \errors i maybeConnection -> case maybeConnection of
          Nothing -> errors
          Just connection -> checkConnection (Id i) connection P.++ errors
      )
      []
      (node ^. secondaryPorts)

{- | Wiring-symmetry violations of a whole net: every @Connected@ reference from a
live cell (busy bit set) must point at a live cell, and the target port must hold
exactly the mirror connection back.

The busy map is authoritative for liveness: freed cells may still contain stale
nodes in RAM, and those are not part of the net.
-}
netSymmetryErrors ::
  forall portsNumber agentType.
  (KnownNat portsNumber) =>
  Vec CellsNumber Bool ->
  Vec CellsNumber (Maybe (Node portsNumber agentType)) ->
  [SymmetryError portsNumber]
netSymmetryErrors busyBitMap net =
  ifoldl
    ( \errors i isBusy ->
        if isBusy
          then case net !! i of
            Nothing -> errors -- busy without payload: occupancy defect, not wiring
            Just node -> checkNodeSymmetry lookupTarget (fromIntegral i) node P.++ errors
          else errors
    )
    []
    busyBitMap
 where
  lookupTarget address =
    if busyBitMap !! address
      then Just (net !! address)
      else Just Nothing -- every address is inside a whole net; a freed one is dead

{- | Wiring-symmetry violations of a subnet given as `LoadedNode`s. References to
addresses outside the set are external and skipped, so a pair, an interface or a
rule result can be checked against its own members only.
-}
subnetSymmetryErrors ::
  forall portsNumber agentType.
  (KnownNat portsNumber) =>
  [LoadedNode portsNumber agentType] ->
  [SymmetryError portsNumber]
subnetSymmetryErrors nodes = P.concatMap checkNode nodes
 where
  checkNode (LoadedNode node address) = checkNodeSymmetry lookupTarget address node
  lookupTarget address = go nodes
   where
    go [] = Nothing
    go (LoadedNode node nodeAddress : rest)
      | nodeAddress == address = Just (Just node)
      | otherwise = go rest
