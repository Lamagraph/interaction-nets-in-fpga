{- | Unit tests: the declared allocation count of every EDSL rule.

"Core.Reducer" sizes the free-address pool from `getAddressesAllocationCount`
("Core/Reducer.hs"), so a drift between the declared count and the number of
`fresh` calls a rule body actually performs is a memory-manager bug: too few
addresses ⇒ @fresh: free address pool exhausted@, too many ⇒ leaked cells.

The invariant is checked per label pair and is contents-independent: no EDSL
rule body inspects a secondary port's /content/, only wires it. A single
fixture shape therefore covers every pair; its secondaries target ports of
nodes @7@ and @8@, i.e. outside the pair (@0@ and @1@) and disjoint from the
pool (@10@–@13@), so no wire is ever resolved to a fresh node.
-}
module Tests.Core.Unit.EdslRulesUnit.AllocationCount (
  allocationCountTests,
) where

import qualified Clash.Prelude as C
import qualified Core.Concrete.ReduceRulesLambda as RW
import Core.Node
import qualified INet.Define as Def
import INet.Net (INet (getAddressesAllocationCount, makeReduction), ReduceRuleResult)
import Test.Tasty
import Test.Tasty.HUnit (testCase, (@?=))
import Prelude

import Tests.Core.Unit.Common.Normalize (freeVec4, freeVec4Full, normNodesDef, toDef)

-- | Active pair whose members each have both secondaries connected outside the pair.
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
has to be named explicitly for each of the two `INet`s under test.
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
