{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

{- | Shared wiring-symmetry assertions for the test suite.

Whole nets are checked against the live cells they declare (fixtures) or an
explicit busy mask; subnets — active pairs, interface vectors and rule results —
are checked against their own members only (`Core.Node.subnetSymmetryErrors`
treats references outside the subset as external).
-}
module Tests.Core.Unit.Common.Symmetry (
  assertSymmetricNet,
  assertSymmetricFixtureNet,
  assertSymmetricSubnet,
  assertSymmetricActivePair,
  assertSymmetricAfterDelta,
  assertSymmetricReductions,
  applyDeltaToNet,
  fixtureLive,
  loadedNet,
  loadedNodes,
) where

import Clash.Prelude
import Control.Lens ((^.))
import Core.MemoryManager.ChangesAccumulator (getAllChangesByDelta)
import Core.MemoryManager.MemoryManager (
  ActivePair (ActivePair),
  MemoryManager,
  busyBitMap,
  giveActiveAddressNumber,
  leftNode,
  removeActivePair,
  rightNode,
 )
import Core.MemoryManager.NodeChanges (Delta (..), activePair, updateLoadedNodesByChanges)
import Core.Node
import Core.Reducer (getInterface, reduce)
import qualified Data.Maybe as DM
import INet.Net (INet)
import Test.Tasty.HUnit (Assertion, assertBool)
import qualified Prelude as P

-- | Live cells of a fixture: every occupied cell is part of the net.
fixtureLive :: Vec CellsNumber (Maybe a) -> Vec CellsNumber Bool
fixtureLive = imap (P.const DM.isJust)

-- | Whole-net check against an explicit live set (the busy map of a running net).
assertSymmetricNet ::
  (KnownNat portsNumber) =>
  String ->
  Vec CellsNumber Bool ->
  Vec CellsNumber (Maybe (Node portsNumber agentType)) ->
  Assertion
assertSymmetricNet label busy net =
  assertBool (label P.++ ": " P.++ P.show errors) (P.null errors)
 where
  errors = netSymmetryErrors busy net

-- | Whole-net check for a static fixture (all occupied cells are live).
assertSymmetricFixtureNet ::
  (KnownNat portsNumber) =>
  String ->
  Vec CellsNumber (Maybe (Node portsNumber agentType)) ->
  Assertion
assertSymmetricFixtureNet label net = assertSymmetricNet label (fixtureLive net) net

-- | Subnet check: references outside the given nodes are external.
assertSymmetricSubnet ::
  (KnownNat portsNumber) =>
  String ->
  [LoadedNode portsNumber agentType] ->
  Assertion
assertSymmetricSubnet label nodes =
  assertBool (label P.++ ": " P.++ P.show errors) (P.null errors)
 where
  errors = subnetSymmetryErrors nodes

-- | Nodes of a `LoadedNode` vector, dropping free cells.
loadedNodes ::
  Vec n (Maybe (LoadedNode portsNumber agentType)) ->
  [LoadedNode portsNumber agentType]
loadedNodes = DM.catMaybes . toList

-- | Subnet check for the two nodes of an active pair.
assertSymmetricActivePair ::
  (KnownNat portsNumber) =>
  String ->
  ActivePair portsNumber agentType ->
  Assertion
assertSymmetricActivePair label pair =
  assertSymmetricSubnet label [pair ^. leftNode, pair ^. rightNode]

-- | Raw node view of a `LoadedNode` vector, for feeding it to `applyDeltaToNet`.
loadedNet ::
  Vec n (Maybe (LoadedNode portsNumber agentType)) ->
  Vec n (Maybe (Node portsNumber agentType))
loadedNet = fmap (fmap (^. containedNode))

{- | Reference application of one `Delta` to the net it was produced from.
Test-only model of the CPU reduce phase: patch the interface neighbours, clear
the consumed pair and write the fresh nodes. Order matters because a fresh node
may reuse an address of the pair. Returns the live set of the resulting net.
-}
applyDeltaToNet ::
  forall portsNumber nodesNumber edgesNumber agentType.
  ( KnownNat portsNumber
  , KnownNat nodesNumber
  , KnownNat edgesNumber
  ) =>
  Vec CellsNumber Bool ->
  Vec CellsNumber (Maybe (Node portsNumber agentType)) ->
  Delta nodesNumber edgesNumber portsNumber agentType ->
  (Vec CellsNumber Bool, Vec CellsNumber (Maybe (Node portsNumber agentType)))
applyDeltaToNet busy net delta@(Delta newNodes _ _) =
  (newBusy, newNet)
 where
  pair = delta ^. activePair
  interface = getInterface @portsNumber @(2 * portsNumber) pair
  changes = getAllChangesByDelta delta interface
  interfaceNodes =
    fmap
      ( \maybeAddress -> do
          address <- maybeAddress
          LoadedNode <$> (net !! address) <*> pure address
      )
      interface
  patchedInterface = updateLoadedNodesByChanges interfaceNodes changes
  leftAddress = pair ^. leftNode . originalAddress
  rightAddress = pair ^. rightNode . originalAddress
  netWithoutPair = replace leftAddress Nothing (replace rightAddress Nothing net)
  netAfterInterface = writeLoadedNodes netWithoutPair (DM.catMaybes $ toList patchedInterface)
  newNodesList = DM.catMaybes $ toList newNodes
  (newBusy, newNet) =
    P.foldl
      ( \(mask, currentNet) (LoadedNode node address) ->
          (replace address True mask, replace address (Just node) currentNet)
      )
      (replace leftAddress False (replace rightAddress False busy), netAfterInterface)
      newNodesList
  writeLoadedNodes =
    P.foldl (\currentNet (LoadedNode node address) -> replace address (Just node) currentNet)

-- | Materialize a reduction result and assert that the resulting net is symmetric.
assertSymmetricAfterDelta ::
  (KnownNat portsNumber, KnownNat nodesNumber, KnownNat edgesNumber) =>
  String ->
  Vec CellsNumber (Maybe (Node portsNumber agentType)) ->
  Delta nodesNumber edgesNumber portsNumber agentType ->
  Assertion
assertSymmetricAfterDelta label net delta =
  assertSymmetricNet label busy netAfterDelta
 where
  (busy, netAfterDelta) = applyDeltaToNet (fixtureLive net) net delta

{- | Reduce a net to quiescence with the real reducer and assert symmetry after
every step. Uses the CPU order: free the pair in the memory manager before
asking the reducer for fresh addresses.
-}
assertSymmetricReductions ::
  forall portsNumber nodesNumber edgesNumber agentType.
  ( KnownNat portsNumber
  , KnownNat nodesNumber
  , KnownNat edgesNumber
  , INet agentType nodesNumber edgesNumber portsNumber
  ) =>
  String ->
  Vec CellsNumber (Maybe (Node portsNumber agentType)) ->
  MemoryManager ->
  Int ->
  Assertion
assertSymmetricReductions label initialNet initialMemoryManager maxSteps = do
  let initialBusy = initialMemoryManager ^. busyBitMap
  assertBool
    (label P.++ ": input busy mask differs from the MemoryManager")
    (initialBusy == fixtureLive initialNet)
  assertSymmetricNet (label P.++ " initial") initialBusy initialNet
  go 0 initialBusy initialMemoryManager initialNet
 where
  go stepNo busy memoryManager net =
    case giveActiveAddressNumber memoryManager of
      Nothing -> assertBool (label P.++ ": no active pair was reduced") (stepNo > 0)
      Just leftAddress
        | stepNo >= maxSteps ->
            assertBool
              (label P.++ ": no quiescence after " P.++ P.show maxSteps P.++ " reductions")
              False
        | otherwise -> case net !! leftAddress of
            Nothing -> assertBool (label P.++ ": active address holds no node") False
            Just leftNode -> case leftNode ^. primaryPort of
              Just (Port rightAddress Primary) -> case net !! rightAddress of
                Nothing -> assertBool (label P.++ ": active partner holds no node") False
                Just rightNode ->
                  let pair :: ActivePair portsNumber agentType
                      pair = ActivePair (LoadedNode leftNode leftAddress) (LoadedNode rightNode rightAddress)
                      freedMemoryManager = removeActivePair pair memoryManager
                      (delta, nextMemoryManager) =
                        reduce freedMemoryManager pair ::
                          (Delta nodesNumber edgesNumber portsNumber agentType, MemoryManager)
                      (nextBusy, nextNet) = applyDeltaToNet busy net delta
                      memoryManagerBusy = nextMemoryManager ^. busyBitMap
                   in do
                        assertBool
                          (label P.++ " step " P.++ P.show stepNo P.++ ": busy mask differs from the MemoryManager")
                          (nextBusy == memoryManagerBusy)
                        assertSymmetricNet
                          (label P.++ " step " P.++ P.show stepNo)
                          nextBusy
                          nextNet
                        go (stepNo + 1) nextBusy nextMemoryManager nextNet
              _ -> assertBool (label P.++ ": active pair is not a principal-principal wire") False
