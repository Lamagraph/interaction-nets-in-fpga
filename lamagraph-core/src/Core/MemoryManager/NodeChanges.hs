{- |
Module      : Core.MemoryManager.NodeChanges
Description : Change records a reduction step applies to the rest of the net

The data a reduction step communicates to memory: @Delta@ — the full result
of one reduction (fresh nodes, new wires, the consumed pair), `Interface` —
the addresses of all external neighbours of the pair, and @Changes@ — a
per-node patch of port connections. `applyChangesToNode` and
`updateLoadedNodesByChanges` apply such patches to nodes fetched from RAM;
`getActiveAddresses` \/ `writeNewActives` mark the pairs created by the step
in the @MemoryManager@ (used by "Core.Reducer.reduce").
-}
module Core.MemoryManager.NodeChanges where

import Clash.Prelude
import Control.Lens hiding (Index, ifoldl, imap, (:>))
import Core.Map
import Core.MemoryManager.MemoryManager
import Core.Node
import Data.Maybe

{- | The full effect of one reduction step, produced by "Core.Reducer.toDelta".

`_newNodes` are the fresh nodes (an entry may reuse an address of the
consumed pair); `_newEdges` are the new wires with connections already
redirected past the pair; `_activePair` is the pair that was reduced.
-}
data Delta (nodesNumber :: Nat) (edgesNumber :: Nat) (portsNumber :: Nat) agentType = Delta
  { _newNodes :: Vec nodesNumber (Maybe (LoadedNode portsNumber agentType))
  , _newEdges :: Vec edgesNumber (Maybe (Edge portsNumber))
  , _activePair :: ActivePair portsNumber agentType
  }
  deriving (Show, Eq, Generic, NFDataX)

$(makeLenses ''Delta)

-- | Addresses of the external neighbours of an active pair, fixed-size (at most @externalNodesNumber@ slots, `Nothing`-padded).
type Interface externalNodesNumber = Vec externalNodesNumber (Maybe AddressNumber)

{- | Data to accumulate all @Port@ changes of the @Node@.

A partial patch: a `Nothing` in the outer slot means \"leave this port as it
is\", a `Just` connection replaces the current one. Default (`def`) patches
nothing.
-}
data Changes (portsNumber :: Nat)
  = Changes
  { _secPorts :: Vec portsNumber (Maybe (Connection portsNumber))
  , _primePort :: Maybe (Connection portsNumber)
  }
  deriving (Show, Eq, Generic, NFDataX, Default)

$(makeLenses ''Changes)

{- | Update @Port@s of @Node@ by @Changes@.

Only the slots patched by the @Changes@ move: for secondary ports the patch
wins slot-wise via `zipWith (<|>)` (first `Just` — the patch — takes
precedence), for the principal port a `Just` patch replaces the connection
outright.
-}
applyChangesToNode ::
  (KnownNat portsNumber) =>
  Node portsNumber agentType ->
  Changes portsNumber ->
  Node portsNumber agentType
applyChangesToNode oldNode (Changes maybeSecPortsAddr maybePrimaryPort) =
  set primaryPort newPrimaryPort (set secondaryPorts newSecondaryPorts oldNode)
 where
  newSecondaryPorts =
    zipWith
      (<|>)
      maybeSecPortsAddr
      (oldNode ^. secondaryPorts)
  newPrimaryPort = case maybePrimaryPort of
    Nothing -> oldNode ^. primaryPort
    Just primPort -> primPort

{- | Update external @LoadedNode@s by accumulated @Changes@.

Every loaded node is looked up by its `_originalAddress` in the map; nodes
without a changes entry are a contract violation and abort with
`errorX "External node has no changes"` — by construction every external
neighbour of a reduced pair has at least one redirected port, so a well-formed
@Delta@ never triggers this. Map entries whose key matches no node are ignored.
-}
updateLoadedNodesByChanges ::
  (KnownNat portsNumber, KnownNat externalNodesNumber) =>
  Vec externalNodesNumber (Maybe (LoadedNode portsNumber agentType)) ->
  Map externalNodesNumber (Changes portsNumber) ->
  Vec externalNodesNumber (Maybe (LoadedNode portsNumber agentType))
updateLoadedNodesByChanges externalLoadedNodes mapOfChanges =
  map
    (\maybeLoadedNode -> applyChangesToLoadedNode <$> maybeLoadedNode <*> (getChangesByLoadedNode <$> maybeLoadedNode))
    externalLoadedNodes
 where
  getChangesByAddress address = fromMaybe (errorX "External node has no changes") (find mapOfChanges address)
  getChangesByLoadedNode loadedNode = getChangesByAddress $ loadedNode ^. originalAddress
  applyChangesToLoadedNode loadedNode change = over containedNode (`applyChangesToNode` change) loadedNode

{- | Addresses of the pairs created by the reduction, sized
@Div nodesNumber 2 + Div edgesNumber 2@: a fresh node whose principal port
wires to an external principal port heads a new pair (the external address is
skipped — it is marked via its partner), and so does an edge whose both ends
are principal ports (the left one is taken).
-}
getActiveAddresses ::
  forall nodesNumber edgesNumber portsNumber agentType.
  (KnownNat portsNumber, KnownNat nodesNumber, KnownNat edgesNumber) =>
  Delta nodesNumber edgesNumber portsNumber agentType ->
  Vec (Div nodesNumber 2 + Div edgesNumber 2) (Maybe AddressNumber)
getActiveAddresses (Delta nodes edges _) = newActivesByEdges
 where
  getPrimaryAddressByLoadedNode (LoadedNode (Node (Connected (Port adjacentAddress Primary)) _ _) address) = Just (adjacentAddress, address)
  getPrimaryAddressByLoadedNode _ = Nothing
  newActivesByNodes =
    foldl
      ( \actives maybeLoadedNode ->
          maybe
            actives
            (\(adjacentAddress, address) -> if Just adjacentAddress `notElem` actives then Just address +>> actives else actives)
            (getPrimaryAddressByLoadedNode =<< maybeLoadedNode)
      )
      (def :: Vec (Div nodesNumber 2 + Div edgesNumber 2) (Maybe AddressNumber))
      nodes
  newActivesByEdges =
    foldl
      ( \actives -> \case
          Just (Edge (Connected (Port address Primary)) (Connected (Port _ Primary))) -> Just address +>> actives
          _ -> actives
      )
      newActivesByNodes
      edges

-- | Mark every new active `AddressNumber` of the @Delta@ (`getActiveAddresses`) in the active-pairs bit map.
writeNewActives ::
  forall nodesNumber edgesNumber portsNumber agentType.
  (KnownNat nodesNumber, KnownNat edgesNumber, KnownNat portsNumber) =>
  Delta nodesNumber edgesNumber portsNumber agentType ->
  MemoryManager ->
  MemoryManager
writeNewActives delta memoryManager = set activePairs newActivePairs memoryManager
 where
  newActivePairs =
    foldl
      (\acPairs maybeAddress -> maybe acPairs (markAddress acPairs True) maybeAddress)
      (memoryManager ^. activePairs)
      (getActiveAddresses delta)
