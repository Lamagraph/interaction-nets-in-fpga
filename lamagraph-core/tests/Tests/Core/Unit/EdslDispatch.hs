{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeApplications #-}

{- | Unit tests: the dispatch generator in "INet.Edsl.Dispatch".

The lambda set in "INet.Define" proves the generator on the real rule set. This
module declares a different two-agent set and a one-entry table, then checks
that the generated @ruleFor@ handles the canonical orientation, the mirrored
orientation, and the missing pair, and that the generated @INet@ instance
derives the allocation count from the rule.
-}
module Tests.Core.Unit.EdslDispatch (
  edslDispatchTests,
) where

import INet.Edsl (AgentLabel (..), Rule (..), SomeRule (..))
import INet.Edsl.Agents (declareAgents)
import INet.Edsl.Dispatch (declareINet, rule)
import INet.Net (INet (..))
import Test.Tasty
import Test.Tasty.HUnit (assertFailure, testCase, (@?=))
import Prelude

$( declareAgents
     "DispatchLabel"
     [ ("Copy", 1)
     , ("Drop", 0)
     ]
 )

-- | A minimal rule body: dispatch and allocation only, no net semantics.
copyDropRule :: Rule 0 1 1 1 DispatchLabel
copyDropRule = Rule $ \_ _ -> pure ()

$( declareINet
     "DispatchLabel"
     1
     1
     1
     [rule "Copy" "Drop" [|copyDropRule|]]
 )

assertHit :: Maybe (SomeRule 1 1 1 DispatchLabel) -> IO ()
assertHit = \case
  Just (SomeRule _) -> pure ()
  Nothing -> assertFailure "expected a rule"

assertMiss :: Maybe (SomeRule 1 1 1 DispatchLabel) -> IO ()
assertMiss = \case
  Nothing -> pure ()
  Just _ -> assertFailure "expected no rule"

edslDispatchTests :: TestTree
edslDispatchTests =
  testGroup
    "rule-table generator (INet.Edsl.Dispatch)"
    [ testCase "canonical orientation hits" $
        assertHit (ruleFor CopyLabel DropLabel)
    , testCase "mirrored orientation hits" $
        assertHit (ruleFor DropLabel CopyLabel)
    , testCase "missing pair has no rule" $
        assertMiss (ruleFor DropLabel DropLabel)
    , testCase "allocation count comes from the rule type" $ do
        getAddressesAllocationCount @DispatchLabel @1 @1 @1 CopyLabel DropLabel @?= 0
        getAddressesAllocationCount @DispatchLabel @1 @1 @1 DropLabel CopyLabel @?= 0
    ]
