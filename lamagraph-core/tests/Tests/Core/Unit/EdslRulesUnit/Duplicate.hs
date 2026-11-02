{- | Unit tests: the EDSL-only `Duplicate` rules on fixed fixtures.

`Duplicate` has no hand-written counterpart, so everything here is asserted
against hand-derived literals. A `Duplicate` actor has arity 2 but no arguments,
so both its slots are padding `Nothing`; a `Duplicate >< X` rule copies @X@ to
the first two pool addresses and gives each secondary slot of @X@ a `Duplicate`
fan. The fan's primary absorbs the @X@ slot and its secondaries reach the
copies' matching slots, so no external edges are emitted.
-}
module Tests.Core.Unit.EdslRulesUnit.Duplicate (
  duplicateTests,
) where

import qualified Clash.Prelude as C
import Core.Node
import qualified INet.Define as Def
import INet.Net (INet (makeReduction), ReduceRuleResult)
import Test.Tasty
import Test.Tasty.HUnit (Assertion, testCase, (@?=))
import Prelude

import Tests.Core.Unit.Common.Normalize (freeVec4, freeVec4Full, mkNode, normEdges, normNodesDef)
import Tests.Core.Unit.Common.Symmetry (assertSymmetricSubnet)

-- | Compare a rule result and assert that its fresh-node subnet is symmetric.
assertRuleResult ::
  [LoadedNode 2 Def.AgentSimpleLambda] ->
  [LoadedNode 2 Def.AgentSimpleLambda] ->
  [Edge 2] ->
  [Edge 2] ->
  Assertion
assertRuleResult actNodes expectedNodes actEdges expectedEdges = do
  assertSymmetricSubnet "rule result" actNodes
  actNodes @?= expectedNodes
  actEdges @?= expectedEdges

{- | Fixtures for the `Duplicate >< X` rules. The `Duplicate` actor has arity 2
but no arguments, so both its slots are padding `Nothing` (see
"Core.Concrete.Initial".@initialIdErase@); `dupTargetSecs` points the partner's
slots at ports of node @5@, i.e. outside the pair and disjoint from the pool.
-}

-- | A `Duplicate` actor: both secondary slots are padding.
dupNode :: AddressNumber -> AddressNumber -> LoadedNode 2 Def.AgentSimpleLambda
dupNode = mkNode Def.DuplicateLabel emptySecs

-- | The other member of a `Duplicate` pair, with the given secondary connections.
dupPartner ::
  Def.AgentSimpleLambda ->
  AddressNumber ->
  AddressNumber ->
  C.Vec 2 (Maybe (Connection 2)) ->
  LoadedNode 2 Def.AgentSimpleLambda
dupPartner lbl addr partnerAddr secs = mkNode lbl secs addr partnerAddr

-- | An `Erase` actor: arity 0, so both slots are padding `Nothing`.
eraseNode :: AddressNumber -> AddressNumber -> LoadedNode 2 Def.AgentSimpleLambda
eraseNode = mkNode Def.EraseLabel emptySecs

-- | No secondary connection in either slot.
emptySecs :: C.Vec 2 (Maybe (Connection 2))
emptySecs = Nothing C.:> Nothing C.:> C.Nil

-- | Secondary connections of a `Duplicate` partner, both targeting node 5.
dupTargetSecs :: C.Vec 2 (Maybe (Connection 2))
dupTargetSecs =
  Just (Connected (Port 5 (Id 0))) C.:> Just (Connected (Port 5 (Id 1))) C.:> C.Nil

{- | The four nodes a `Duplicate >< X` rule materializes, given the label of the
two copies: @X@ is copied to the first two pool addresses and each of @X@'s
secondary slots gets a `Duplicate` fan, whose primary absorbs that slot and
whose two secondaries reach the copies' matching slots.
-}
dupExpectation :: Def.AgentSimpleLambda -> [LoadedNode 2 Def.AgentSimpleLambda]
dupExpectation copyLbl =
  [ LoadedNode
      ( Node
          NotConnected
          (Just (Connected (Port 12 (Id 0))) C.:> Just (Connected (Port 13 (Id 0))) C.:> C.Nil)
          copyLbl
      )
      10
  , LoadedNode
      ( Node
          NotConnected
          (Just (Connected (Port 12 (Id 1))) C.:> Just (Connected (Port 13 (Id 1))) C.:> C.Nil)
          copyLbl
      )
      11
  , LoadedNode
      ( Node
          (Connected (Port 5 (Id 0)))
          (Just (Connected (Port 10 (Id 0))) C.:> Just (Connected (Port 11 (Id 0))) C.:> C.Nil)
          Def.DuplicateLabel
      )
      12
  , LoadedNode
      ( Node
          (Connected (Port 5 (Id 1)))
          (Just (Connected (Port 10 (Id 1))) C.:> Just (Connected (Port 11 (Id 1))) C.:> C.Nil)
          Def.DuplicateLabel
      )
      13
  ]

{- | Same, for a partner whose secondary slots are all empty: the copies are
still wired to the fans, but a fan has nothing to absorb, so its primary stays
`NotConnected`. This is the `EndFree, EndFresh` branch of `wire`
("INet.Edsl".@writeAcc@) reached through the `Duplicate` path.
-}
dupExpectationEmpty :: Def.AgentSimpleLambda -> [LoadedNode 2 Def.AgentSimpleLambda]
dupExpectationEmpty copyLbl =
  [ LoadedNode
      ( Node
          NotConnected
          (Just (Connected (Port 12 (Id 0))) C.:> Just (Connected (Port 13 (Id 0))) C.:> C.Nil)
          copyLbl
      )
      10
  , LoadedNode
      ( Node
          NotConnected
          (Just (Connected (Port 12 (Id 1))) C.:> Just (Connected (Port 13 (Id 1))) C.:> C.Nil)
          copyLbl
      )
      11
  , LoadedNode
      ( Node
          NotConnected
          (Just (Connected (Port 10 (Id 0))) C.:> Just (Connected (Port 11 (Id 0))) C.:> C.Nil)
          Def.DuplicateLabel
      )
      12
  , LoadedNode
      ( Node
          NotConnected
          (Just (Connected (Port 10 (Id 1))) C.:> Just (Connected (Port 11 (Id 1))) C.:> C.Nil)
          Def.DuplicateLabel
      )
      13
  ]

{- | `Duplicate >< X` (EDSL-only): two `Abstract` copies plus one `Duplicate` fan
per secondary of `X`. The fan's primary eats the `X` slot and its secondaries
reach the copies' secondaries; all these ends are new nodes (`EndFresh`), so the
connectivity lands inside the copy/fan slots (`writeAcc`) and no external edges
are emitted.
-}
reduceDuplicateAbstract :: TestTree
reduceDuplicateAbstract =
  testCase "Duplicate >< Abstract: two copies, two fans" $
    assertRuleResult actNodes expectedNodes actEdges expectedEdges
 where
  res :: ReduceRuleResult 4 2 2 Def.AgentSimpleLambda
  res = makeReduction (freeVec4Full 10 11 12 13) (dupNode 0 1) (dupPartner Def.AbstractLabel 1 0 dupTargetSecs)
  actNodes = normNodesDef res
  actEdges = normEdges res
  expectedNodes = dupExpectation Def.AbstractLabel
  expectedEdges = []

{- | `Duplicate >< Apply`: same shape as the `Abstract` case — the copy agent is
chosen by the pair's right-hand label and both `Apply` and `Abstract` have arity
2 — with `Apply` copies instead.
-}
reduceDuplicateApply :: TestTree
reduceDuplicateApply =
  testCase "Duplicate >< Apply: two apply copies, two fans" $
    assertRuleResult actNodes expectedNodes actEdges expectedEdges
 where
  res :: ReduceRuleResult 4 2 2 Def.AgentSimpleLambda
  res = makeReduction (freeVec4Full 10 11 12 13) (dupNode 0 1) (dupPartner Def.ApplyLabel 1 0 dupTargetSecs)
  actNodes = normNodesDef res
  actEdges = normEdges res
  expectedNodes = dupExpectation Def.ApplyLabel
  expectedEdges = []

{- | Mirrored dispatch (`Apply >< Duplicate`): the rule body is called with the
arguments swapped, so the `Duplicate` remains the branching actor and the result
is unchanged.
-}
reduceApplyDuplicate :: TestTree
reduceApplyDuplicate =
  testCase "Apply >< Duplicate: two apply copies, two fans" $
    assertRuleResult actNodes expectedNodes actEdges expectedEdges
 where
  res :: ReduceRuleResult 4 2 2 Def.AgentSimpleLambda
  res = makeReduction (freeVec4Full 10 11 12 13) (dupPartner Def.ApplyLabel 0 1 dupTargetSecs) (dupNode 1 0)
  actNodes = normNodesDef res
  actEdges = normEdges res
  expectedNodes = dupExpectation Def.ApplyLabel
  expectedEdges = []

-- | Mirrored dispatch (`Abstract >< Duplicate`), otherwise as `reduceDuplicateAbstract`.
reduceAbstractDuplicate :: TestTree
reduceAbstractDuplicate =
  testCase "Abstract >< Duplicate: two copies, two fans" $
    assertRuleResult actNodes expectedNodes actEdges expectedEdges
 where
  res :: ReduceRuleResult 4 2 2 Def.AgentSimpleLambda
  res = makeReduction (freeVec4Full 10 11 12 13) (dupPartner Def.AbstractLabel 0 1 dupTargetSecs) (dupNode 1 0)
  actNodes = normNodesDef res
  actEdges = normEdges res
  expectedNodes = dupExpectation Def.AbstractLabel
  expectedEdges = []

{- | `Duplicate >< Abstract` with an empty partner: both copies and both fans
still get wired to each other, but nothing exists for a fan's primary to absorb,
so every primary stays `NotConnected` and the copies become unreachable.
-}
reduceDuplicateAbstractEmpty :: TestTree
reduceDuplicateAbstractEmpty =
  testCase "Duplicate >< Abstract with no arguments: unreachable copies and fans" $
    assertRuleResult actNodes expectedNodes actEdges expectedEdges
 where
  res :: ReduceRuleResult 4 2 2 Def.AgentSimpleLambda
  res = makeReduction (freeVec4Full 10 11 12 13) (dupNode 0 1) (dupPartner Def.AbstractLabel 1 0 emptySecs)
  actNodes = normNodesDef res
  actEdges = normEdges res
  expectedNodes = dupExpectationEmpty Def.AbstractLabel
  expectedEdges = []

-- | Erase rules fired by a `Duplicate`: both its slots are padding, so two fresh erasers are allocated with unconnected primaries.
reduceEraseDuplicate :: TestTree
reduceEraseDuplicate =
  testCase "Erase >< Duplicate: two unreachable fresh erasers" $ do
    -- Both results are free erasers with padding slots: no internal wire exists
    -- to check, so only the literal comparison applies.
    actNodes @?= expectedNodes
    actEdges @?= expectedEdges
 where
  res :: ReduceRuleResult 4 2 2 Def.AgentSimpleLambda
  res = makeReduction (freeVec4 10 11) (dupNode 0 1) (eraseNode 1 0)
  actNodes = normNodesDef res
  actEdges = normEdges res
  expectedNodes =
    [ LoadedNode (Node NotConnected (Nothing C.:> Nothing C.:> C.Nil) Def.EraseLabel) 10
    , LoadedNode (Node NotConnected (Nothing C.:> Nothing C.:> C.Nil) Def.EraseLabel) 11
    ]
  expectedEdges = []

-- | Mirrored dispatch (`Duplicate >< Erase`).
reduceDuplicateErase :: TestTree
reduceDuplicateErase =
  testCase "Duplicate >< Erase: two unreachable fresh erasers" $ do
    -- Both results are free erasers with padding slots: no internal wire exists
    -- to check, so only the literal comparison applies.
    actNodes @?= expectedNodes
    actEdges @?= expectedEdges
 where
  res :: ReduceRuleResult 4 2 2 Def.AgentSimpleLambda
  res = makeReduction (freeVec4 10 11) (eraseNode 0 1) (dupNode 1 0)
  actNodes = normNodesDef res
  actEdges = normEdges res
  expectedNodes =
    [ LoadedNode (Node NotConnected (Nothing C.:> Nothing C.:> C.Nil) Def.EraseLabel) 10
    , LoadedNode (Node NotConnected (Nothing C.:> Nothing C.:> C.Nil) Def.EraseLabel) 11
    ]
  expectedEdges = []

duplicateTests :: TestTree
duplicateTests =
  testGroup
    "Duplicate"
    [ reduceDuplicateAbstract
    , reduceDuplicateApply
    , reduceApplyDuplicate
    , reduceAbstractDuplicate
    , reduceDuplicateAbstractEmpty
    , reduceEraseDuplicate
    , reduceDuplicateErase
    ]
