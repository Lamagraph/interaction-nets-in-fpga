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
  forall agentType nodesNumber edgesNumber portsNumber.
  ( KnownNat portsNumber
  , KnownNat nodesNumber
  , KnownNat edgesNumber
  , nodesNumber <= CellsNumber
  , INet agentType nodesNumber edgesNumber portsNumber
  , NFDataX agentType
  , Show agentType
  , ShowX agentType
  , Eq agentType
  ) =>
  Vec CellsNumber (Maybe (Node portsNumber agentType)) ->
  MemoryManager ->
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
      ( (logicBroad @portsNumber @nodesNumber @edgesNumber @agentType @System)
          initNet
          rootNodeAddress
          initMemoryManager
      )
  answersAreEqual = P.all (`elem` systemActualAnswer) expectedAnswer

idApplyToIdMealyCore :: TestTree
idApplyToIdMealyCore =
  mealyCoreTestTemplate @AgentSimpleLambda @2 @2
    initialIdApplyToIdNode
    initialIdApplyToIdMM
    [0, 1]
    30
    0
    "id apply to id core ( (λx. x)(λy. y) )"

epsApplyToIdSimpleMealyCore :: TestTree
epsApplyToIdSimpleMealyCore =
  mealyCoreTestTemplate @AgentSimpleLambda @2 @2
    initialEpsAppToIdSimpleNode
    initialEpsAppToIdSimpleMM
    [1, 2]
    20
    1
    "id apply to eps simplified core ( (ϵ)(λx. x) )"

epsApplyToIdMealyCore :: TestTree
epsApplyToIdMealyCore =
  mealyCoreTestTemplate @AgentSimpleLambda @2 @2
    initialEpsAppToIdNode
    initialEpsAppToIdMM
    [1, 3, 2]
    50
    1
    "id apply to eps core ( (λx.(ϵ x))(λy.y) )"

{- | The cycle at which the machine enters `Done` and stays there.
The machine may emit a few identical frames before it starts reducing
(Init / FetchLeftActiveAddress), so we look for the *last* index where
consecutive frames differ; `Done` starts one cycle after that.
Returns `Nothing` if the sampled window never stabilises.
-}
cyclesToDone :: (Eq a, Show a) => [a] -> Maybe Int
cyclesToDone frames =
  let pairs = P.zip frames (P.drop 1 frames)
      trailingEqual = P.length (P.takeWhile (P.uncurry (==)) (P.reverse pairs))
   in if P.null pairs P.|| (trailingEqual == P.length pairs)
        then Nothing
        else Just (P.length pairs - trailingEqual)

{- | Run the system for `maxCycles` cycles and assert the machine reaches a
stable `Done` state in exactly `expectedCycles` cycles
-}
mealyCoreCycleCountTest ::
  forall agentType nodesNumber edgesNumber portsNumber.
  ( KnownNat portsNumber
  , KnownNat nodesNumber
  , KnownNat edgesNumber
  , nodesNumber <= CellsNumber
  , INet agentType nodesNumber edgesNumber portsNumber
  , NFDataX agentType
  , Show agentType
  , ShowX agentType
  , Eq agentType
  ) =>
  Vec CellsNumber (Maybe (Node portsNumber agentType)) ->
  MemoryManager ->
  AddressNumber ->
  Int ->
  TestName ->
  TestTree
mealyCoreCycleCountTest initNet initMemoryManager rootNodeAddress expectedCycles testName =
  testCase testName $
    case cyclesToDone frames of
      Nothing -> assertFailure "system never reached a stable Done state"
      Just actualCycles ->
        assertEqual
          ( "expected cycles to Done: "
              P.++ show expectedCycles
              P.++ "\nactual: "
              P.++ show actualCycles
          )
          expectedCycles
          actualCycles
 where
  maxCycles = expectedCycles + 10
  frames =
    sampleN
      maxCycles
      ( (logicBroad @portsNumber @nodesNumber @edgesNumber @agentType @System)
          initNet
          rootNodeAddress
          initMemoryManager
      )

idApplyToIdCycleCount :: TestTree
idApplyToIdCycleCount =
  mealyCoreCycleCountTest @AgentSimpleLambda @2 @2
    initialIdApplyToIdNode
    initialIdApplyToIdMM
    0
    6
    "id apply to id cycle count"

epsApplyToIdSimpleCycleCount :: TestTree
epsApplyToIdSimpleCycleCount =
  mealyCoreCycleCountTest @AgentSimpleLambda @2 @2
    initialEpsAppToIdSimpleNode
    initialEpsAppToIdSimpleMM
    1
    6
    "id apply to eps simplified cycle count"

epsApplyToIdCycleCount :: TestTree
epsApplyToIdCycleCount =
  mealyCoreCycleCountTest @AgentSimpleLambda @2 @2
    initialEpsAppToIdNode
    initialEpsAppToIdMM
    1
    20
    "id apply to eps cycle count"

cycleCountUnitTests :: TestTree
cycleCountUnitTests =
  testGroup
    "mealy Core cycle count"
    [ idApplyToIdCycleCount
    , epsApplyToIdSimpleCycleCount
    , epsApplyToIdCycleCount
    ]

mealyCoreUnitTests :: TestTree
mealyCoreUnitTests =
  testGroup
    "mealy Core"
    [ idApplyToIdMealyCore
    , epsApplyToIdSimpleMealyCore
    , epsApplyToIdMealyCore
    ]
