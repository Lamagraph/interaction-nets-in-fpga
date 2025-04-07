{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE PartialTypeSignatures #-}
{-# OPTIONS_GHC -Wno-incomplete-uni-patterns #-}
{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-partial-type-signatures #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Eta reduce" #-}
{-# OPTIONS_GHC -Wno-unused-local-binds #-}

module Tests.Core.Unit.Core where

import Clash.Prelude
import Test.Tasty
import Test.Tasty.HUnit

import Core.Concrete.Initial
import Core.Concrete.ReduceRulesLambda (AgentSimpleLambda)
import Core.Core
import Core.MemoryManager.MemoryManager
import Core.Node
import INet.Net
import Prelude as P

createDomain vSystem{vName = "SlowDom", vPeriod = hzToPeriod 1}
createDomain vSystem{vName = "FastDom", vPeriod = hzToPeriod 2}
clkSlow :: Clock SlowDom
clkSlow = clockGen @SlowDom
clkFast :: Clock FastDom
clkFast = clockGen @FastDom

coreTestTemplate ::
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
  ) =>
  Vec cellsNumber (Maybe (Node portsNumber agentType)) ->
  MemoryManager cellsNumber ->
  [(AddressNumber, CoreStatus)] ->
  AddressNumber ->
  TestName ->
  TestTree
coreTestTemplate initNet initMemoryManager expectedAnswer rootNodeAddress testName =
  testCase testName $
    assertBool
      ( "expected:\n"
          P.++ show expectedAnswer
          P.++ "\nactual:\n"
          P.++ showX systemActualAnswer
      )
      answersAreEqual
 where
  initializedRam =
    exposeClockResetEnable
      (initializer @portsNumber @cellsNumber @FastDom @agentType)
      clkFast
      resetGen
      enableGen
      initNet
  systemActualAnswer =
    sampleN
      (P.length expectedAnswer)
      ( bundle $
          core @portsNumber @nodesNumber @edgesNumber @cellsNumber
            clkSlow
            clkFast
            initializedRam
            rootNodeAddress
            initMemoryManager
            getReduceRuleInfo
      )
  answersAreEqual = expectedAnswer == systemActualAnswer

idApplyToIdCore :: TestTree
idApplyToIdCore =
  coreTestTemplate @AgentSimpleLambda @(2 ^ BitSize AddressNumber) @2 @2
    initialIdApplyToIdNode
    initialIdApplyToIdMM
    [(0 :: AddressNumber, Start), (0, Start), (1, InProgress), (1, Done), (1, Done)]
    0
    "id apply to id core ( (λx. x)(λy. y) )"

epsApplyToIdSimpleCore :: TestTree
epsApplyToIdSimpleCore =
  coreTestTemplate @AgentSimpleLambda @(2 ^ BitSize AddressNumber) @2 @2
    initialEpsAppToIdSimpleNode
    initialEpsAppToIdSimpleMM
    [(1 :: AddressNumber, Start), (1, Start), (2, InProgress), (2, Done)]
    1
    "id apply to eps simplified core ( (ϵ)(λx. x) )"

epsApplyToIdCore :: TestTree
epsApplyToIdCore =
  coreTestTemplate @AgentSimpleLambda @(2 ^ BitSize AddressNumber) @2 @2
    initialEpsAppToIdNode
    initialEpsAppToIdMM
    [(1 :: AddressNumber, Start), (1, Start), (3, InProgress), (2, InProgress)]
    1
    "id apply to eps core ( (λx.(ϵ x))(λy.y) )"

coreUnitTests :: TestTree
coreUnitTests =
  testGroup
    "Core"
    []

--   idApplyToIdCore
-- , epsApplyToIdSimpleCore
-- , epsApplyToIdCore
