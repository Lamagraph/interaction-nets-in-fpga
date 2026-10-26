{- | Shared assertion helpers for the HUnit-based unit tests.

Most tests compare a hand-written @expected@ value against the sampled
@actual@ one and want both sides dumped when the assertion fails. `assertDump`
centralizes that message format; the @assertEqual*@ wrappers cover the common
@==@ cases, picking `show` or `showX` for the dump.
-}
module Tests.Core.Unit.Common.Assert (
  assertDump,
  assertEqualShow,
  assertEqualShowX,
) where

import Clash.Prelude (ShowX, showX)
import Test.Tasty.HUnit (Assertion, assertBool)
import Prelude

{- | Assert @ok@, dumping the already-rendered @expected@ and @actual@ values
when it does not hold.
-}
assertDump :: String -> String -> Bool -> Assertion
assertDump expected actual = assertBool ("expected:\n" ++ expected ++ "\nactual:\n" ++ actual)

-- | `assertDump` with both sides rendered by `show`.
assertEqualShow :: (Show a, Eq a) => a -> a -> Assertion
assertEqualShow expected actual =
  assertDump (show expected) (show actual) (expected == actual)

-- | `assertDump` with both sides rendered by `showX` (Clash types without `Show`).
assertEqualShowX :: (ShowX a, Eq a) => a -> a -> Assertion
assertEqualShowX expected actual =
  assertDump (showX expected) (showX actual) (expected == actual)
