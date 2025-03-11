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
import Core.Node
import INet.Net
import Prelude as P

createDomain vSystem{vName = "SlowDom", vPeriod = hzToPeriod 1}
createDomain vSystem{vName = "FastDom", vPeriod = hzToPeriod 2}
clkSlow :: Clock SlowDom
clkSlow = clockGen @SlowDom
clkFast :: Clock FastDom
clkFast = clockGen @FastDom

idApplyToIdCore :: TestTree
idApplyToIdCore =
  testCase "id apply to id core" $
    assertBool
      ("expected:\n" P.++ show expectedAddress P.++ "\nactual:\n" P.++ show (showX systemActualAddress))
      addressesAreEqual
 where
  initializedRam =
    exposeClockResetEnable
      (initializer @2 @(2 ^ BitSize AddressNumber) @FastDom @AgentSimpleLambda)
      clkFast
      resetGen
      enableGen
      initialIdApplyToIdNode

  expectedAddress = [(0 :: AddressNumber, Start), (0, Start), (1, InProgress), (1, Done), (1, Done)]
  systemActualAddress =
    sampleN
      5
      ( bundle $
          core
            clkSlow
            clkFast
            initializedRam
            0
            initialIdApplyToIdMM
            (getReduceRuleInfo :: AgentSimpleLambda -> AgentSimpleLambda -> ReductionRuleInfo 65536 2 2 2 AgentSimpleLambda)
      )
  addressesAreEqual = expectedAddress == systemActualAddress

coreUnitTests :: TestTree
coreUnitTests =
  testGroup
    "Core"
    [ idApplyToIdCore
    ]
