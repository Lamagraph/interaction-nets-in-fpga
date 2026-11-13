{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Eta reduce" #-}
{-# HLINT ignore "Functor law" #-}
{-# OPTIONS_HADDOCK ignore-exports #-}

{- |
Module      : Core.MemoryManager.ChangesAccumulator
Description : Compute the per-neighbour updates implied by a @Delta@

After a reduction rule has fired, every external neighbour of the dying pair
still points its port at a dead address. This module computes, for each
external neighbour in the `Interface`, which of its ports must be redirected
and to what:

- a port of a fresh @Node@ that points into the `Interface` yields the update
  @external.port := Port newNode portId@ (the neighbour now attaches to the
  fresh node);
- an @Edge@ between two external ends yields two mirrored updates (the two
  neighbours now attach to each other);
- several updates concerning one external node are merged into a single
  @Changes@ value for that node.

The result — a `Map` from external address to @Changes@ — is written back to
RAM by the @WriteChange@ phase of the CPU FSM ("Core.CPU"), which iterates
the `Interface` and looks each address up in the map.

`OPTIONS_HADDOCK ignore-exports` is set so the accumulator pipeline, hidden
by the explicit export list, is fully documented; the export list itself is
unchanged.
-}
module Core.MemoryManager.ChangesAccumulator (getAllChangesByDelta) where

import Clash.Prelude
import Control.Lens hiding (ifoldl)
import Core.Map
import Core.MemoryManager.NodeChanges
import Core.Node

-- | Type alias for triple (the address of the external node to update, the id of the port to update, the connection to update to)
type UpdateInfo (portsNumber :: Nat) = (AddressNumber, IdOfPort portsNumber, Connection portsNumber)

{- | Generate update information for the @Node@ attached to @port@ on the
@addressPointTo@ side:

- `Nothing` when the port is `NotConnected` (nothing to redirect) or when the
  attached node's address is not a member of the `Interface` — an update would
  never be read back, so it is dropped here rather than accumulated.
- @Just (attachedAddress, attachedPortId, newConnection)@ otherwise, where
  @newConnection@ points at @addressPointTo.portId@ — the port of the node
  being written.
-}
getUpdateInfoByPort ::
  forall portsNumber externalNodesNumber.
  (KnownNat portsNumber, KnownNat externalNodesNumber) =>
  AddressNumber ->
  Interface externalNodesNumber ->
  Connection portsNumber ->
  IdOfPort portsNumber ->
  Maybe (UpdateInfo portsNumber)
getUpdateInfoByPort addressPointTo interface port portId =
  case port of
    Connected (Port addressNumber connectedToPortId) -> if Just addressNumber `elem` interface then update addressNumber else def
     where
      newPort = Connected $ Port addressPointTo portId
      update addressNum = Just (addressNum, connectedToPortId, newPort)
    NotConnected -> Nothing

{- | The same as `getUpdateInfoByPort` by @Edge@: the two mirrored updates that
re-point both external ends of the edge at each other. A @NotConnected@ end
yields no update for the opposite side (the still-connected end is dropped to
free, the other side keeps nothing to point at).

Invariant: unlike `getUpdateInfoByPort`, this does NOT check membership in the
`Interface`. It is guaranteed by construction — every @Edge@ of a @Delta@ comes
from a rule result, whose ends are the neighbours of the reduced pair that
@getInterface@ collected (the EDSL builds edges only for external-to-external
wiring, `INet.Define`; hand-written rules zip the pair's secondary ports,
"Core.Concrete.ReduceRulesLambda"). Should a malformed rule still produce an
edge towards a non-interface address, the stray update is harmless: the
@WriteChange@ phase of "Core.CPU" iterates the `Interface` and never reads
map entries under other keys.
-}
getUpdateInfoByEdge ::
  forall portsNumber.
  (KnownNat portsNumber) =>
  Edge portsNumber ->
  ( Maybe (UpdateInfo portsNumber)
  , Maybe (UpdateInfo portsNumber)
  )
getUpdateInfoByEdge (Edge maybeLeftEdge maybeRightEdge) =
  (leftUpdate, rightUpdate)
 where
  leftUpdate = case maybeRightEdge of
    Connected (Port address portId) -> Just (address, portId, maybeLeftEdge)
    NotConnected -> Nothing
  rightUpdate = case maybeLeftEdge of
    Connected (Port address portId) -> Just (address, portId, maybeRightEdge)
    NotConnected -> Nothing

{- | Merge one `UpdateInfo` into the changes `Map` via `Core.Map.insertWith`.

The update overwrites exactly one slot of the addressed node's @Changes@: the
principal connection for `Primary`, the @i@-th secondary slot for @Id i@; all
other slots of that node keep their previous values, so successive updates to
different ports of one node accumulate into one @Changes@. If the same port of
the same node were updated twice, the later write would win (the last
`Just` for that slot replaces the earlier one).

Overflow: a full map aborts inside `Core.Map.insert` with an explicit error;
see `getAllChangesByDelta` for why a well-formed @Delta@ never fills it.
-}
insertInInfoVec ::
  (KnownNat size, KnownNat portsNumber) =>
  Map size (Changes portsNumber) ->
  Maybe (UpdateInfo portsNumber) ->
  Map size (Changes portsNumber)
insertInInfoVec infoVec maybeInsertionInfo =
  case maybeInsertionInfo of
    Nothing -> infoVec
    Just (address, portId, newPort) ->
      insertWith
        infoVec
        (insertFunction portId newPort)
        address
 where
  insertFunction portId newPort maybeInfo = Just $ maybe (constructNewInfo def) constructNewInfo maybeInfo
   where
    constructNewInfo info@(Changes secPortsInfo _) = case portId of
      Primary -> set primePort (Just newPort) info
      Id i -> set secPorts (replace i (Just newPort) secPortsInfo) info

-- | Accumulate the updates induced by all ports of one new @LoadedNode@ into the `Map` (principal port first, then secondaries in order).
accumulatePortsChangesByLoadedNode ::
  forall portsNumber externalNodesNumber agentType.
  (KnownNat portsNumber, KnownNat externalNodesNumber) =>
  Map externalNodesNumber (Changes portsNumber) ->
  Interface externalNodesNumber ->
  LoadedNode portsNumber agentType ->
  Map externalNodesNumber (Changes portsNumber)
accumulatePortsChangesByLoadedNode infoVec interface signalLoadedNode =
  let updateToConcreteNode = getUpdateInfoByPort signalAddress interface
      signalAddress = view originalAddress signalLoadedNode
      infoVecByPrimary =
        insertInInfoVec
          infoVec
          ( updateToConcreteNode
              (view (containedNode . primaryPort) signalLoadedNode)
              Primary
          )
   in ifoldl
        ( \oldInfoVec i signalMaybePort -> case signalMaybePort of
            Nothing -> oldInfoVec
            Just signalPort -> insertInInfoVec oldInfoVec (updateToConcreteNode signalPort (Id i))
        )
        infoVecByPrimary
        (view (containedNode . secondaryPorts) signalLoadedNode)

-- | Accumulate the updates of all new @Node@s of the @Delta@ (slot order).
accumulateUpdatesByNodes ::
  (KnownNat nodesNumber, KnownNat portsNumber, KnownNat externalNodesNumber) =>
  Map externalNodesNumber (Changes portsNumber) ->
  Interface externalNodesNumber ->
  Vec nodesNumber (Maybe (LoadedNode portsNumber agentType)) ->
  Map externalNodesNumber (Changes portsNumber)
accumulateUpdatesByNodes infoVec interface loadedNodes =
  foldl
    ( \oldInfoVec maybeLoadedNode -> case maybeLoadedNode of
        Nothing -> oldInfoVec
        Just loadedNode -> accumulatePortsChangesByLoadedNode oldInfoVec interface loadedNode
    )
    infoVec
    loadedNodes

-- | Merge the two mirrored updates of one @Edge@ into the `Map` (left end first, then right).
insertPairInInfoVec ::
  (KnownNat size, KnownNat portsNumber) =>
  Map size (Changes portsNumber) ->
  (Maybe (UpdateInfo portsNumber), Maybe (UpdateInfo portsNumber)) ->
  Map size (Changes portsNumber)
insertPairInInfoVec oldInfoVec (maybeLeftUpdateInfo, maybeRightUpdateInfo) =
  insertInInfoVec (insertInInfoVec oldInfoVec maybeLeftUpdateInfo) maybeRightUpdateInfo

-- | Accumulate the updates of all @Edge@s of the @Delta@ (slot order).
accumulateUpdatesByEdges ::
  (KnownNat edgesNumber, KnownNat portsNumber, KnownNat externalNodesNumber) =>
  Map externalNodesNumber (Changes portsNumber) ->
  Vec edgesNumber (Maybe (Edge portsNumber)) ->
  Map externalNodesNumber (Changes portsNumber)
accumulateUpdatesByEdges infoVec edgesForUpdate =
  foldl
    ( \oldInfoVec maybeEdge -> case maybeEdge of
        Nothing -> oldInfoVec
        Just edge -> insertPairInInfoVec oldInfoVec (getUpdateInfoByEdge edge)
    )
    infoVec
    edgesForUpdate

{- | All port updates implied by the @Delta@, as a map from external address to
@Changes@.

Size: the result is a `Map` of @externalNodesNumber@ — the size of the
`Interface` — slots, i.e. exactly one slot per external neighbour; it can hold
every well-formed delta, in which every external node is updated at most once
per port. There is no truncation: a full map aborts inside `Core.Map.insert`.

Order: edges are applied first (inner fold), new nodes second (outer fold).
The two passes normally touch disjoint ports of disjoint nodes — a neighbour's
dead-pointing port is redirected either to another neighbour (edge) or to a
fresh node (node port) — so the order is not observable on well-formed deltas.
If a malformed rule updated the same port twice, the node-pass value would win
because it is applied later and overwrites that single slot.
-}
getAllChangesByDelta ::
  (KnownNat edgesNumber, KnownNat nodesNumber, KnownNat externalNodesNumber, KnownNat portsNumber) =>
  Delta nodesNumber edgesNumber portsNumber agentType ->
  Interface externalNodesNumber ->
  Map externalNodesNumber (Changes portsNumber)
getAllChangesByDelta Delta{..} interface = accumulateUpdatesByNodes (accumulateUpdatesByEdges def _newEdges) interface _newNodes
