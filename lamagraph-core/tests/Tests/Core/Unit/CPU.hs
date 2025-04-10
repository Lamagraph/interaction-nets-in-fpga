{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE PartialTypeSignatures #-}
{-# OPTIONS_GHC -Wno-incomplete-uni-patterns #-}
{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-partial-type-signatures #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Eta reduce" #-}
{-# OPTIONS_GHC -Wno-unused-local-binds #-}

module Tests.Core.Unit.CPU where

import Clash.Prelude
import Core.Concrete.Initial
import Core.Concrete.ReduceRulesLambda
import Core.Core
import Core.MemoryManager.MemoryManager
import Core.Node
import INet.Net
import Test.Tasty
import Test.Tasty.HUnit
import Prelude as P

mealyCoreTestTemplate ::
  forall agentType cellsNumber nodesNumber edgesNumber portsNumber.
  ( KnownNat portsNumber
  , KnownNat nodesNumber
  , KnownNat edgesNumber
  , KnownNat cellsNumber
  , 1 <= cellsNumber
  , CLog 2 cellsNumber <= BitSize AddressNumber
  , nodesNumber <= cellsNumber
  , INet agentType cellsNumber nodesNumber edgesNumber portsNumber
  , NFDataX agentType
  , Show agentType
  , ShowX agentType
  , Eq agentType
  ) =>
  Vec cellsNumber (Maybe (Node portsNumber agentType)) ->
  MemoryManager cellsNumber ->
  [AddressNumber] ->
  Int ->
  -- [CPUOut portsNumber agentType] ->
  AddressNumber ->
  TestName ->
  TestTree
mealyCoreTestTemplate initNet initMemoryManager expectedAnswer cycleCount rootNodeAddress testName =
  testCase testName $
    assertBool
      ( "expected:\n"
          P.++ show expectedAnswer
          P.++ "\nactual:\n"
          P.++ showX systemActualAnswer
      )
      answersAreEqual
 where
  systemActualAnswer =
    sampleN
      cycleCount
      ( (logicBroad @portsNumber @nodesNumber @edgesNumber @cellsNumber @agentType @System)
          initNet
          rootNodeAddress
          initMemoryManager
          getReduceRuleInfo
      )
  answersAreEqual = P.all (`elem` systemActualAnswer) expectedAnswer

idApplyToIdMealyCore :: TestTree
idApplyToIdMealyCore =
  mealyCoreTestTemplate @AgentSimpleLambda @(2 ^ BitSize AddressNumber) @2 @2
    initialIdApplyToIdNode
    initialIdApplyToIdMM
    [0, 1]
    30
    0
    "id apply to id core ( (λx. x)(λy. y) )"

epsApplyToIdSimpleMealyCore :: TestTree
epsApplyToIdSimpleMealyCore =
  mealyCoreTestTemplate @AgentSimpleLambda @(2 ^ BitSize AddressNumber) @2 @2
    initialEpsAppToIdSimpleNode
    initialEpsAppToIdSimpleMM
    [1, 2]
    20
    1
    "id apply to eps simplified core ( (ϵ)(λx. x) )"

epsApplyToIdMealyCore :: TestTree
epsApplyToIdMealyCore =
  mealyCoreTestTemplate @AgentSimpleLambda @(2 ^ BitSize AddressNumber) @2 @2
    initialEpsAppToIdNode
    initialEpsAppToIdMM
    [1, 3, 2]
    50
    1
    "id apply to eps core ( (λx.(ϵ x))(λy.y) )"

mealyCoreUnitTests :: TestTree
mealyCoreUnitTests =
  testGroup
    "mealy Core"
    [ idApplyToIdMealyCore
    , epsApplyToIdSimpleMealyCore
    , epsApplyToIdMealyCore
    ]
