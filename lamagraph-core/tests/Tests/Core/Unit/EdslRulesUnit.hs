{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE TypeApplications #-}

{- | Unit tests: EDSL rules (INet.Define) on fixed, hand-written fixtures.

Mirrors the hand-written-rule unit tests (`Tests.Core.Unit.Reducer`): identical
concrete nets from `Core.Concrete.Initial`, but instead of driving the full
`reduce` pipeline the EDSL `makeReduction` is called directly on the active
pair. Expected values are baked in as literals, so a regression shows up as a
plain HUnit diff.

Where the hand-written rules express the same semantics at rule level, the
expected value is additionally pinned against `Core.Concrete.ReduceRulesLambda`
so both sides of the EDSL-vs-handwritten contract are checked in one place.

Three deliberate divergences from the raw hand-written rule output:

* A wire whose pair-slot end is `NotConnected` (e.g. an abstract whose secondary
  is empty) is elided by the EDSL (`EndFree, EndExternal -> pure ()`) instead of
  producing `Edge NotConnected _`. See `reduceIdAppIdBeta` below.
* `Duplicate >< X` has no hand-written counterpart — it is asserted against a
  hand-derived literal only.
* A non-Erase actor with an /empty/ secondary slot makes the EDSL materialize a
  node for it anyway (`EndFree, EndFresh -> writeAcc _ NotConnected`), often
  leaving a permanently unreachable node whose primary stays `NotConnected`,
  where the hand-written rules raise `error "Agent type of the node is
  incorrect"`. This is why `reduceEpsAppToId`, `reduceEpsAppToIdSimple` and
  `reduceIdErase` are pinned against neither implementation.

The `errorX` guards in `INet.Define` (`No rule for this pair`, reached by
`Apply >< Apply`, `Abstract >< Abstract` and `Duplicate >< Duplicate`) and in
`INet.Edsl` (`free address pool exhausted`, `too many wires`, `no fresh agent at
slot`) are deliberately untested: `error` cannot be caught in pure Haskell, and
covering them would mean making the rule interface surface failures as data.
-}
module Tests.Core.Unit.EdslRulesUnit where

import qualified Clash.Prelude as C
import qualified Core.Concrete.Initial as Init
import qualified Core.Concrete.ReduceRulesLambda as RW
import Core.Node
import Data.Maybe (mapMaybe)
import qualified INet.Define as Def
import INet.Net (INet (getAddressesAllocationCount, makeReduction), ReduceRuleResult (..))
import Test.Tasty
import Test.Tasty.HUnit (testCase, (@?=))
import Prelude

-- ====================
-- Normalization (list projections, as in EdslRules.hs)
-- ====================

normEdges :: ReduceRuleResult n e p l -> [Edge p]
normEdges = mapMaybe id . C.toList . _edges

normNodesDef :: ReduceRuleResult n e p Def.AgentSimpleLambda -> [LoadedNode p Def.AgentSimpleLambda]
normNodesDef = mapMaybe id . C.toList . _nodes

-- | Relabel a hand-written node to the EDSL label set
toDef :: RW.AgentSimpleLambda -> Def.AgentSimpleLambda
toDef = \case
  RW.Apply -> Def.ApplyLabel
  RW.Abstract -> Def.AbstractLabel
  RW.Erase -> Def.EraseLabel

-- | Free-address pools for the EDSL reductions below
freeVec4 :: AddressNumber -> AddressNumber -> C.Vec 4 (Maybe AddressNumber)
freeVec4 a0 a1 = Just a0 C.:> Just a1 C.:> Nothing C.:> Nothing C.:> C.Nil

freeVec4Full :: AddressNumber -> AddressNumber -> AddressNumber -> AddressNumber -> C.Vec 4 (Maybe AddressNumber)
freeVec4Full a0 a1 a2 a3 = Just a0 C.:> Just a1 C.:> Just a2 C.:> Just a3 C.:> C.Nil

freeVec2 :: AddressNumber -> AddressNumber -> C.Vec 2 (Maybe AddressNumber)
freeVec2 a0 a1 = Just a0 C.:> Just a1 C.:> C.Nil

-- ====================
-- Fixtures (re-using Core.Concrete.Initial where possible)
-- ====================

-- | Active pair extracted from a fixture, relabeled to the EDSL label set
rwPair ::
  (LoadedNode 2 RW.AgentSimpleLambda, LoadedNode 2 RW.AgentSimpleLambda) ->
  (LoadedNode 2 Def.AgentSimpleLambda, LoadedNode 2 Def.AgentSimpleLambda)
rwPair (LoadedNode (Node pr secs lbl) a, LoadedNode (Node pr' secs' lbl') a') =
  ( LoadedNode (Node pr secs (toDef lbl)) a
  , LoadedNode (Node pr' secs' (toDef lbl')) a'
  )

-- | \((\lambda x.x)(\lambda y.y)\): pair = Apply@0 and the inner Abstract@2
fixtureIdApplyToId :: (LoadedNode 2 RW.AgentSimpleLambda, LoadedNode 2 RW.AgentSimpleLambda)
fixtureIdApplyToId =
  let (Just p0 C.:> _ C.:> Just p2 C.:> _) = Init.initialIdApplyToId
   in (p0, p2)

-- | Self-looped apply/abstract: every secondary references its own node
fixtureLoopEdge :: (LoadedNode 2 RW.AgentSimpleLambda, LoadedNode 2 RW.AgentSimpleLambda)
fixtureLoopEdge =
  let (Just p0 C.:> Just p1 C.:> _) = Init.initialLoopEdge
   in (p0, p1)

-- | \((\lambda x.(\varepsilon)(x))(\lambda y.y)\): pair = Apply@3 and Erase@4
fixtureEpsAppToId :: (LoadedNode 2 RW.AgentSimpleLambda, LoadedNode 2 RW.AgentSimpleLambda)
fixtureEpsAppToId =
  let (_ C.:> _ C.:> _ C.:> Just p3 C.:> Just p4 C.:> _) = Init.initialEpsAppToId
   in (p3, p4)

-- | \((\varepsilon)(\lambda y.y)\): pair = Apply@1 and Erase@2
fixtureEpsAppToIdSimple :: (LoadedNode 2 RW.AgentSimpleLambda, LoadedNode 2 RW.AgentSimpleLambda)
fixtureEpsAppToIdSimple =
  let (_ C.:> Just p1 C.:> Just p2 C.:> _) = Init.initialEpsAppToIdSimple
   in (p1, p2)

-- | \(\lambda x.x\) connected to an eraser: pair = Abstract@0 and Erase@1
fixtureIdErase :: (LoadedNode 2 RW.AgentSimpleLambda, LoadedNode 2 RW.AgentSimpleLambda)
fixtureIdErase =
  let (Just p0 C.:> Just p1 C.:> _) = Init.initialIdErase
   in (p0, p1)

-- | A fully-connected β-pair: all four secondaries hold real connections
fixtureBetaFull :: (LoadedNode 2 RW.AgentSimpleLambda, LoadedNode 2 RW.AgentSimpleLambda)
fixtureBetaFull =
  ( LoadedNode (Node (Connected (Port 1 Primary)) lSecs RW.Apply) 2
  , LoadedNode (Node (Connected (Port 2 Primary)) rSecs RW.Abstract) 1
  )
 where
  lSecs = Just (Connected (Port 4 Primary)) C.:> Just (Connected (Port 5 (Id 1))) C.:> C.Nil
  rSecs = Just (Connected (Port 6 (Id 1))) C.:> Just (Connected (Port 7 (Id 0))) C.:> C.Nil

-- ====================
-- Duplicate fixtures
-- ====================

{- | Fixtures for the `Duplicate >< X` rules. The `Duplicate` actor has arity 2
but no arguments, so both its slots are padding `Nothing` (see
`Core.Concrete.Initial.initialIdErase`); `dupTargetSecs` points the partner's
slots at ports of node @5@, i.e. outside the pair and disjoint from the pool.
-}

-- | A `Duplicate` actor: both secondary slots are padding
dupNode :: AddressNumber -> AddressNumber -> LoadedNode 2 Def.AgentSimpleLambda
dupNode addr partnerAddr =
  LoadedNode
    (Node (Connected (Port partnerAddr Primary)) (Nothing C.:> Nothing C.:> C.Nil) Def.DuplicateLabel)
    addr

-- | The other member of a `Duplicate` pair, with the given secondary connections
dupPartner ::
  Def.AgentSimpleLambda ->
  AddressNumber ->
  AddressNumber ->
  C.Vec 2 (Maybe (Connection 2)) ->
  LoadedNode 2 Def.AgentSimpleLambda
dupPartner lbl addr partnerAddr secs =
  LoadedNode (Node (Connected (Port partnerAddr Primary)) secs lbl) addr

-- | Secondary connections of a `Duplicate` partner, both targeting node 5
dupTargetSecs :: C.Vec 2 (Maybe (Connection 2))
dupTargetSecs =
  Just (Connected (Port 5 (Id 0))) C.:> Just (Connected (Port 5 (Id 1))) C.:> C.Nil

-- | An `Erase` actor: arity 0, so both slots are padding `Nothing`
eraseNode :: AddressNumber -> AddressNumber -> LoadedNode 2 Def.AgentSimpleLambda
eraseNode addr partnerAddr =
  LoadedNode
    (Node (Connected (Port partnerAddr Primary)) (Nothing C.:> Nothing C.:> C.Nil) Def.EraseLabel)
    addr

-- | No secondary connection in either slot
emptySecs :: C.Vec 2 (Maybe (Connection 2))
emptySecs = Nothing C.:> Nothing C.:> C.Nil

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
(`INet.Edsl.writeAcc`) reached through the `Duplicate` path.
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

-- ====================
-- Helpers
-- ====================

-- | Hand-written rule result on the same raw pair, edges only (labels don't appear in `Edge`)
rwEdgesOf ::
  C.Vec 2 (Maybe AddressNumber) -> LoadedNode 2 RW.AgentSimpleLambda -> LoadedNode 2 RW.AgentSimpleLambda -> [Edge 2]
rwEdgesOf free lN rN = normEdges res
 where
  res :: ReduceRuleResult 2 2 2 RW.AgentSimpleLambda
  res = makeReduction free lN rN

-- | EDSL rule result on a fixture pair with the given pool
edsOf ::
  C.Vec 4 (Maybe AddressNumber) ->
  (LoadedNode 2 RW.AgentSimpleLambda, LoadedNode 2 RW.AgentSimpleLambda) ->
  ([Edge 2], [LoadedNode 2 Def.AgentSimpleLambda])
edsOf freePool (lN, rN) = (edges, nodes)
 where
  (lN', rN') = rwPair (lN, rN)
  res :: ReduceRuleResult 4 2 2 Def.AgentSimpleLambda
  res = makeReduction freePool lN' rN'
  edges = normEdges res
  nodes = normNodesDef res

-- ====================
-- Allocation count
-- ====================

{- | `Core.Reducer` sizes the free-address pool from `getAddressesAllocationCount`
(`Core/Reducer.hs`), so a drift between the declared count and the number of
`fresh` calls a rule body actually performs is a memory-manager bug: too few
addresses ⇒ @fresh: free address pool exhausted@, too many ⇒ leaked cells.

The invariant is checked per label pair and is contents-independent: no EDSL
rule body inspects a secondary port's /content/, only wires it. A single
fixture shape therefore covers every pair. Its secondaries target ports of
nodes @7@ and @8@, i.e. outside the pair (@0@ and @1@) and disjoint from the
pool (@10@–@13@), so no wire is ever resolved to a fresh node.
-}

-- | Active pair whose members each have both secondaries connected outside the pair
countFixture ::
  Def.AgentSimpleLambda ->
  Def.AgentSimpleLambda ->
  (LoadedNode 2 Def.AgentSimpleLambda, LoadedNode 2 Def.AgentSimpleLambda)
countFixture lLbl rLbl =
  ( LoadedNode (Node (Connected (Port 1 Primary)) lSecs lLbl) 0
  , LoadedNode (Node (Connected (Port 0 Primary)) rSecs rLbl) 1
  )
 where
  lSecs = Just (Connected (Port 7 Primary)) C.:> Just (Connected (Port 7 (Id 1))) C.:> C.Nil
  rSecs = Just (Connected (Port 8 Primary)) C.:> Just (Connected (Port 8 (Id 1))) C.:> C.Nil

{- | Free-address pool holding exactly as many addresses as the rule under test
declares. Only the counts the four rules actually produce are handled: a rule
that started allocating 1 or 3 addresses fails loudly here rather than
silently testing a mis-sized pool.
-}
poolWith :: Int -> C.Vec 4 (Maybe AddressNumber)
poolWith 0 = Nothing C.:> Nothing C.:> Nothing C.:> Nothing C.:> C.Nil
poolWith 2 = freeVec4 10 11
poolWith 4 = freeVec4Full 10 11 12 13
poolWith _ = error "poolWith: no EDSL rule allocates 1 or 3 addresses"

{- | The index parameters do not appear in the argument types, so the instance
has to be named explicitly for each of the two `INet`s under test
-}
edsCount :: Def.AgentSimpleLambda -> Def.AgentSimpleLambda -> Int
edsCount l r = fromIntegral (getAddressesAllocationCount @Def.AgentSimpleLambda @4 @2 @2 l r)

rwCount :: RW.AgentSimpleLambda -> RW.AgentSimpleLambda -> Int
rwCount l r = fromIntegral (getAddressesAllocationCount @RW.AgentSimpleLambda @2 @2 @2 l r)

{- | One matrix cell: the declared count must equal the number of materialized
nodes. The pool is sized from the declared count, so a rule body that asks for
more raises @fresh: free address pool exhausted@ and one that asks for fewer
materializes fewer nodes than declared; either drift fails the assertion.
-}
allocationCountCase ::
  String ->
  Def.AgentSimpleLambda ->
  Def.AgentSimpleLambda ->
  TestTree
allocationCountCase name lLbl rLbl =
  testCase (name ++ ": " ++ show declared ++ " allocated") $
    length (normNodesDef res) @?= declared
 where
  declared = edsCount lLbl rLbl
  res :: ReduceRuleResult 4 2 2 Def.AgentSimpleLambda
  res = makeReduction (poolWith declared) lN rN
  (lN, rN) = countFixture lLbl rLbl

allocationCountTests :: TestTree
allocationCountTests =
  testGroup
    "EDSL allocation count"
    [ testGroup
        "declared count == materialized nodes"
        [ allocationCountCase "Apply >< Abstract" Def.ApplyLabel Def.AbstractLabel
        , allocationCountCase "Abstract >< Apply" Def.AbstractLabel Def.ApplyLabel
        , allocationCountCase "Erase >< Erase" Def.EraseLabel Def.EraseLabel
        , allocationCountCase "Erase >< Apply" Def.EraseLabel Def.ApplyLabel
        , allocationCountCase "Erase >< Abstract" Def.EraseLabel Def.AbstractLabel
        , allocationCountCase "Erase >< Duplicate" Def.EraseLabel Def.DuplicateLabel
        , allocationCountCase "Apply >< Erase" Def.ApplyLabel Def.EraseLabel
        , allocationCountCase "Abstract >< Erase" Def.AbstractLabel Def.EraseLabel
        , allocationCountCase "Duplicate >< Erase" Def.DuplicateLabel Def.EraseLabel
        , allocationCountCase "Apply >< Duplicate" Def.ApplyLabel Def.DuplicateLabel
        , allocationCountCase "Abstract >< Duplicate" Def.AbstractLabel Def.DuplicateLabel
        , allocationCountCase "Duplicate >< Apply" Def.DuplicateLabel Def.ApplyLabel
        , allocationCountCase "Duplicate >< Abstract" Def.DuplicateLabel Def.AbstractLabel
        ]
    , testGroup
        "declared count == hand-written count"
        [ testCase (show l ++ " >< " ++ show r) $
            edsCount (toDef l) (toDef r) @?= rwCount l r
        | l <- rwLabels
        , r <- rwLabels
        , not (unRuleablePair l r)
        ]
    ]
 where
  rwLabels = [RW.Apply, RW.Abstract, RW.Erase]
  -- the hand-written rules raise `errorX` on these pairs, so there is no count to compare
  unRuleablePair RW.Apply RW.Apply = True
  unRuleablePair RW.Abstract RW.Abstract = True
  unRuleablePair _ _ = False

-- ====================
-- Tests
-- ====================

{- | β on \((\lambda x.x)(\lambda y.y)\): only the argument wire leaves the pair;
the empty `Apply` secondary is elided (see module header)
-}
reduceIdAppIdBeta :: TestTree
reduceIdAppIdBeta =
  testCase "beta on id-apply-to-id: single argument wire" $
    (actEdges @?= expectedEdges) >> (actNodes @?= [])
 where
  (actEdges, actNodes) = edsOf (freeVec4 3 4) fixtureIdApplyToId
  expectedEdges = [Edge (Connected (Port 1 Primary)) (Connected (Port 2 (Id 0)))]

{- | Same net mirrored: `Abstract >< Apply` dispatch path. The dispatch
normalizes pair orientation (`betaBody rp lp`), so the resulting wire list is
identical to the un-mirrored case.
-}
reduceIdAppIdBetaMirrored :: TestTree
reduceIdAppIdBetaMirrored =
  testCase "beta on id-apply-to-id mirrored (Abstract >< Apply)" $
    (actEdges @?= expectedEdges) >> (actNodes @?= [])
 where
  (rN, lN) = fixtureIdApplyToId
  (actEdges, actNodes) = edsOf (freeVec4 3 4) (lN, rN)
  expectedEdges = [Edge (Connected (Port 1 Primary)) (Connected (Port 2 (Id 0)))]

{- | Both secondaries self-looped: both pairs become external wires.
Pinned against the hand-written rule: identical result.
-}
reduceLoopEdgeBeta :: TestTree
reduceLoopEdgeBeta =
  testCase "beta on self-looped apply/abstract: two wires" $
    (actEdges @?= expectedEdges) >> (rwEdges @?= actEdges) >> (actNodes @?= [])
 where
  (lN, rN) = fixtureLoopEdge
  (actEdges, actNodes) = edsOf (freeVec4 2 3) (lN, rN)
  expectedEdges =
    [ Edge (Connected (Port 0 (Id 1))) (Connected (Port 1 (Id 1)))
    , Edge (Connected (Port 0 (Id 0))) (Connected (Port 1 (Id 0)))
    ]
  rwEdges = rwEdgesOf (freeVec2 2 3) lN rN

-- | Fully-connected β-pair: all four secondaries become wires; pinned against the hand-written rule
reduceBetaFull :: TestTree
reduceBetaFull =
  testCase "beta on fully-connected pair: both argument wires" $
    (actEdges @?= expectedEdges) >> (rwEdges @?= actEdges) >> (actNodes @?= [])
 where
  (lN, rN) = fixtureBetaFull
  (actEdges, actNodes) = edsOf (freeVec4 8 9) (lN, rN)
  expectedEdges =
    [ Edge (Connected (Port 4 Primary)) (Connected (Port 6 (Id 1)))
    , Edge (Connected (Port 5 (Id 1))) (Connected (Port 7 (Id 0)))
    ]
  rwEdges = rwEdgesOf (freeVec2 8 9) lN rN

{- | \((\lambda x.(\varepsilon)(x))(\lambda y.y)\): the apply's secondaries become
fresh `Erase` nodes at the first two pool addresses
-}
reduceEpsAppToId :: TestTree
reduceEpsAppToId =
  testCase "erase on (λx.(ε x))(λy.y): two fresh erasers" $
    (actNodes @?= expectedNodes) >> (actEdges @?= [])
 where
  (lN, rN) = fixtureEpsAppToId
  (actEdges, actNodes) = edsOf (freeVec4 6 5) (lN, rN)
  expectedNodes =
    [ LoadedNode (Node (Connected (Port 2 (Id 1))) (void C.:> void C.:> C.Nil) Def.EraseLabel) 6
    , LoadedNode (Node (Connected (Port 2 (Id 0))) (void C.:> void C.:> C.Nil) Def.EraseLabel) 5
    ]
  void = Nothing

{- | \((\varepsilon)(\lambda y.y)\): the first apply secondary is empty, so the
first fresh eraser's primary stays `NotConnected`
-}
reduceEpsAppToIdSimple :: TestTree
reduceEpsAppToIdSimple =
  testCase "erase on (ε)(λy.y): one free eraser primary" $
    (actNodes @?= expectedNodes) >> (actEdges @?= [])
 where
  (lN, rN) = fixtureEpsAppToIdSimple
  (actEdges, actNodes) = edsOf (freeVec4 4 3) (lN, rN)
  expectedNodes =
    [ LoadedNode (Node NotConnected (void C.:> void C.:> C.Nil) Def.EraseLabel) 4
    , LoadedNode (Node (Connected (Port 0 Primary)) (void C.:> void C.:> C.Nil) Def.EraseLabel) 3
    ]
  void = Nothing

-- | \(\lambda x.x\) against an eraser: both copies reference the abstract's secondaries
reduceIdErase :: TestTree
reduceIdErase =
  testCase "erase on λx.x: two fresh erasers on the abstract's children" $
    (actNodes @?= expectedNodes) >> (actEdges @?= [])
 where
  (lN, rN) = fixtureIdErase
  (actEdges, actNodes) = edsOf (freeVec4 3 2) (lN, rN)
  expectedNodes =
    [ LoadedNode (Node (Connected (Port 0 (Id 1))) (void C.:> void C.:> C.Nil) Def.EraseLabel) 3
    , LoadedNode (Node (Connected (Port 0 (Id 0))) (void C.:> void C.:> C.Nil) Def.EraseLabel) 2
    ]
  void = Nothing

{- | `Duplicate >< X` (EDSL-only): two `Abstract` copies plus one `Duplicate` fan
per secondary of `X`. The fan's primary eats the `X` slot and its secondaries
reach the copies' secondaries; all these ends are new nodes (`EndFresh`), so the
connectivity lands inside the copy/fan slots (`writeAcc`) and no external edges
are emitted.
-}
reduceDuplicateAbstract :: TestTree
reduceDuplicateAbstract =
  testCase "Duplicate >< Abstract: two copies, two fans" $
    (actNodes @?= expectedNodes) >> (actEdges @?= expectedEdges)
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
    (actNodes @?= expectedNodes) >> (actEdges @?= expectedEdges)
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
    (actNodes @?= expectedNodes) >> (actEdges @?= expectedEdges)
 where
  res :: ReduceRuleResult 4 2 2 Def.AgentSimpleLambda
  res = makeReduction (freeVec4Full 10 11 12 13) (dupPartner Def.ApplyLabel 0 1 dupTargetSecs) (dupNode 1 0)
  actNodes = normNodesDef res
  actEdges = normEdges res
  expectedNodes = dupExpectation Def.ApplyLabel
  expectedEdges = []

-- | Mirrored dispatch (`Abstract >< Duplicate`), otherwise as `reduceDuplicateAbstract`
reduceAbstractDuplicate :: TestTree
reduceAbstractDuplicate =
  testCase "Abstract >< Duplicate: two copies, two fans" $
    (actNodes @?= expectedNodes) >> (actEdges @?= expectedEdges)
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
    (actNodes @?= expectedNodes) >> (actEdges @?= expectedEdges)
 where
  res :: ReduceRuleResult 4 2 2 Def.AgentSimpleLambda
  res = makeReduction (freeVec4Full 10 11 12 13) (dupNode 0 1) (dupPartner Def.AbstractLabel 1 0 emptySecs)
  actNodes = normNodesDef res
  actEdges = normEdges res
  expectedNodes = dupExpectationEmpty Def.AbstractLabel
  expectedEdges = []

-- | Erase rules fired by a `Duplicate`: both its slots are padding, so two fresh erasers are allocated with unconnected primaries
reduceEraseDuplicate :: TestTree
reduceEraseDuplicate =
  testCase "Erase >< Duplicate: two unreachable fresh erasers" $
    (actNodes @?= expectedNodes) >> (actEdges @?= expectedEdges)
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

-- | Mirrored dispatch (`Duplicate >< Erase`)
reduceDuplicateErase :: TestTree
reduceDuplicateErase =
  testCase "Duplicate >< Erase: two unreachable fresh erasers" $
    (actNodes @?= expectedNodes) >> (actEdges @?= expectedEdges)
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

edslRulesUnitTests :: TestTree
edslRulesUnitTests =
  testGroup
    "EDSL rules (fixed fixtures)"
    [ reduceIdAppIdBeta
    , reduceIdAppIdBetaMirrored
    , reduceLoopEdgeBeta
    , reduceBetaFull
    , reduceEpsAppToId
    , reduceEpsAppToIdSimple
    , reduceIdErase
    , reduceDuplicateAbstract
    , reduceDuplicateApply
    , reduceApplyDuplicate
    , reduceAbstractDuplicate
    , reduceDuplicateAbstractEmpty
    , reduceEraseDuplicate
    , reduceDuplicateErase
    ]
