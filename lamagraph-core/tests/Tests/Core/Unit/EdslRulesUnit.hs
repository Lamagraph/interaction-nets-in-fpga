{- | Unit tests: EDSL rules ("INet.Define") on fixed, hand-written fixtures.

Aggregates the per-rule-family modules under
"Tests.Core.Unit.EdslRulesUnit": `Beta`, `Erase`, `Duplicate` and
`AllocationCount`. Mirrors the hand-written-rule unit tests
("Tests.Core.Unit.Reducer"): identical concrete nets from
"Core.Concrete.Initial", but instead of driving the full `reduce` pipeline the
EDSL `makeReduction` is called directly on the active pair. Expected values are
baked in as literals, so a regression shows up as a plain HUnit diff.

Where the hand-written rules express the same semantics at rule level, the
expected value is additionally pinned against "Core.Concrete.ReduceRulesLambda"
so both sides of the EDSL-vs-handwritten contract are checked in one place.

The `errorX` guards in "INet.Define" (`No rule for this pair`, reached by
`Apply >< Apply`, `Abstract >< Abstract` and `Duplicate >< Duplicate`) and in
"INet.Edsl" (`free address pool exhausted`, `too many wires`, `no fresh agent at
slot`) are deliberately untested: `error` cannot be caught in pure Haskell, and
covering them would mean making the rule interface surface failures as data.
-}
module Tests.Core.Unit.EdslRulesUnit (
  edslRulesUnitTests,
  allocationCountTests,
) where

import Test.Tasty
import qualified Tests.Core.Unit.EdslRulesUnit.AllocationCount as AllocationCount
import qualified Tests.Core.Unit.EdslRulesUnit.Beta as Beta
import qualified Tests.Core.Unit.EdslRulesUnit.Duplicate as Duplicate
import qualified Tests.Core.Unit.EdslRulesUnit.Erase as Erase

edslRulesUnitTests :: TestTree
edslRulesUnitTests =
  testGroup
    "EDSL rules (fixed fixtures)"
    [ Beta.betaTests
    , Erase.eraseTests
    , Duplicate.duplicateTests
    ]

allocationCountTests :: TestTree
allocationCountTests = AllocationCount.allocationCountTests
