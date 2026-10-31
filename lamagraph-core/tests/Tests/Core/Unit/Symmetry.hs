{- | Unit tests: wiring symmetry of the hand-built fixtures.

Every exported net in "Core.Concrete.Initial" is checked with the shared
assertions from "Tests.Core.Unit.Common.Symmetry" (whole net for fixtures,
subnets for the `LoadedNode` views). Negative cases rebuild the one-sided
wiring that used to be in `initialEpsAppToIdNode` and a reference into a free
cell, so the checker cannot pass vacuously.
-}
module Tests.Core.Unit.Symmetry (symmetryTests) where

import Clash.Prelude
import Core.Concrete.Initial
import Core.Concrete.ReduceRulesLambda (AgentSimpleLambda (..))
import Core.Node
import Test.Tasty
import Test.Tasty.HUnit
import Tests.Core.Unit.Common.Symmetry (
  assertSymmetricFixtureNet,
  assertSymmetricReductions,
  assertSymmetricSubnet,
  loadedNodes,
 )
import qualified Prelude as P

symmetryTests :: TestTree
symmetryTests =
  testGroup
    "wiring symmetry"
    [ testCase "initialIdApplyToIdNode" $
        assertSymmetricFixtureNet "initialIdApplyToIdNode" initialIdApplyToIdNode
    , testCase "initialEpsAppToIdNode" $
        assertSymmetricFixtureNet "initialEpsAppToIdNode" initialEpsAppToIdNode
    , testCase "initialEpsAppToIdSimpleNode" $
        assertSymmetricFixtureNet "initialEpsAppToIdSimpleNode" initialEpsAppToIdSimpleNode
    , testCase "initialIdEraseNode" $
        assertSymmetricFixtureNet "initialIdEraseNode" initialIdEraseNode
    , testCase "initialLoopEdge" $
        assertSubnet "initialLoopEdge" initialLoopEdge
    , testCase "loaded view: initialIdApplyToId" $
        assertSubnet "initialIdApplyToId" initialIdApplyToId
    , testCase "loaded view: initialEpsAppToId" $
        assertSubnet "initialEpsAppToId" initialEpsAppToId
    , testCase "loaded view: initialEpsAppToIdSimple" $
        assertSubnet "initialEpsAppToIdSimple" initialEpsAppToIdSimple
    , testCase "loaded view: initialIdErase" $
        assertSubnet "initialIdErase" initialIdErase
    , testCase "detects a one-sided wire" $
        assertSource (Port 0 Primary) "detects a one-sided wire" brokenErrors
    , testCase "detects a reference into a free cell" $
        assertSource (Port 0 Primary) "detects a reference into a free cell" danglingErrors
    , testCase "initialEpsAppToIdNode reduces symmetrically" $
        assertSymmetricReductions @2 @2 @2
          "eps fixture"
          initialEpsAppToIdNode
          initialEpsAppToIdMM
          16
    ]
 where
  brokenErrors = netSymmetryErrors brokenBusy brokenNet
  danglingErrors = netSymmetryErrors danglingBusy danglingNet

assertSubnet ::
  String ->
  Vec CellsNumber (Maybe (LoadedNode 2 AgentSimpleLambda)) ->
  Assertion
assertSubnet label = assertSymmetricSubnet label . loadedNodes

assertSource :: Port 2 -> String -> [SymmetryError 2] -> Assertion
assertSource port label errors =
  assertBool
    ( label
        P.++ ": expected a violation at "
        P.++ P.show port
        P.++ ", got: "
        P.++ P.show (P.map _errorSourcePort errors)
    )
    (port `elem` P.map _errorSourcePort errors)

{- | The pre-fix shape of `initialEpsAppToIdNode`: `absId@0.Primary` claims
`(1, Id1)` while `apply@1.sec1` is free and the argument actually sits on
`sec0`.
-}
brokenNet :: Vec CellsNumber (Maybe (Node 2 AgentSimpleLambda))
brokenNet =
  replace 0 (Just absNode) $
    replace 1 (Just applyNode) $
      repeat Nothing
 where
  absNode = Node (Connected $ Port 1 (Id 1)) def Abstract
  applyNode =
    Node
      (Connected $ Port 0 Primary)
      (Just (Connected $ Port 0 Primary) :> Just NotConnected :> Nil)
      Apply

brokenBusy :: Vec CellsNumber Bool
brokenBusy = replace 0 True $ replace 1 True $ repeat False

-- | One live node whose principal port references a freed cell.
danglingNet :: Vec CellsNumber (Maybe (Node 2 AgentSimpleLambda))
danglingNet = replace 0 (Just node) $ repeat Nothing
 where
  node = Node (Connected $ Port 1 Primary) def Abstract

danglingBusy :: Vec CellsNumber Bool
danglingBusy = replace 0 True $ repeat False
