{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE MultiParamTypeClasses #-}

{- | Unit tests: the agent-set generator in "INet.Edsl.Agents".

`INet.Define` only proves that the generator works for the lambda set. This
module declares a /different/ two-agent set through the same splice and checks
that the generated arity family and label mapping are usable, i.e. that the
machinery in "INet.Edsl" really is agent-set agnostic.
-}
module Tests.Core.Unit.EdslAgents (
  edslAgentsTests,
) where

import Clash.Prelude (NFDataX, SNat (..), ShowX, natVal)
import GHC.Generics (Generic)
import INet.Edsl (AgentLabel (..))
import INet.Edsl.Agents (declareAgents)
import Test.Tasty
import Test.Tasty.HUnit (testCase, (@?=))
import Prelude

$( declareAgents
     "ToyLabel"
     [ ("Copy", 2)
     , ("Drop", 0)
     ]
 )

edslAgentsTests :: TestTree
edslAgentsTests =
  testGroup
    "agent-set generator (INet.Edsl.Agents)"
    [ testCase "singleton maps to the generated runtime label" $ do
        agentLabel Copy @?= CopyLabel
        agentLabel Drop @?= DropLabel
    , testCase "generated arity family reaches the generic class" $ do
        natVal (SNat @(AgentArityOf (Agent 'CopyKind))) @?= 2
        natVal (SNat @(AgentArityOf (Agent 'DropKind))) @?= 0
    ]
