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
import qualified INet.Define as Def
import INet.Net
import Test.Tasty
import Test.Tasty.HUnit
import Tests.Core.Unit.Common.Assert (assertDump)
import Tests.Core.Unit.Common.Symmetry (assertSymmetricFixtureNet)
import Prelude as P

-- | Root-address stream of the closed CPU loop for @cycleCount@ cycles.
mealyCoreRoots ::
  forall agentType nodesNumber edgesNumber portsNumber.
  ( KnownNat portsNumber
  , KnownNat nodesNumber
  , KnownNat edgesNumber
  , nodesNumber <= CellsNumber
  , INet agentType nodesNumber edgesNumber portsNumber
  , NFDataX agentType
  , Show agentType
  , Eq agentType
  ) =>
  Vec CellsNumber (Maybe (Node portsNumber agentType)) ->
  MemoryManager ->
  Int ->
  AddressNumber ->
  [AddressNumber]
mealyCoreRoots initNet initMemoryManager cycleCount rootNodeAddress =
  sampleN
    cycleCount
    ( (logicBroad @portsNumber @nodesNumber @edgesNumber @agentType @System)
        initNet
        rootNodeAddress
        initMemoryManager
    )

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
  AddressNumber ->
  TestName ->
  TestTree
mealyCoreTestTemplate initNet initMemoryManager expectedAnswer cycleCount rootNodeAddress testName =
  testCase testName $ do
    assertSymmetricFixtureNet testName initNet
    assertDump (show expectedAnswer) (showX systemActualAnswer) answersAreEqual
 where
  systemActualAnswer =
    mealyCoreRoots @agentType @nodesNumber @edgesNumber @portsNumber
      initNet
      initMemoryManager
      cycleCount
      rootNodeAddress
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

{- | The same fixture and wiring as `idApplyToIdMealyCore`, but through the
EDSL rule set of "INet.Define" (4-node ceiling), which is what `topEntity`
synthesizes. This pins the processor-level behaviour of the EDSL rules, not
only the rule bodies compared by "Tests.Core.Unit.EdslRules".
-}
idApplyToIdMealyCoreEdsl :: TestTree
idApplyToIdMealyCoreEdsl =
  mealyCoreTestTemplate @Def.AgentSimpleLambda @4 @2
    ( initialIdApplyToIdNodeWith
        Def.ApplyLabel
        Def.AbstractLabel
        Def.EraseLabel
    )
    initialIdApplyToIdMM
    [0, 1]
    30
    0
    "id apply to id core with EDSL rules ( (λx. x)(λy. y) )"

{- | The EDSL wiring must actually finish on the same root as the reference
wiring. `mealyCoreTestTemplate` only checks that the expected addresses appear
somewhere in the trace; this pins the terminal address the board wrapper prints.
-}
idApplyToIdEdslRoot :: TestTree
idApplyToIdEdslRoot =
  testCase "EDSL topEntity reaches root 1" $
    assertEqual "final root" 1 (P.last roots)
 where
  roots =
    mealyCoreRoots @Def.AgentSimpleLambda @4 @2 @2
      ( initialIdApplyToIdNodeWith
          Def.ApplyLabel
          Def.AbstractLabel
          Def.EraseLabel
      )
      initialIdApplyToIdMM
      40
      0

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
    -- Roots 1 (start) and 3 (after beta), then 1 again: the free eraser is
    -- allocated at address 1. The old malformed wiring produced root 2 here.
    [1, 3]
    50
    1
    "id apply to eps core ( (λx.(ϵ x))(λy.y) )"

{- | Pin the corrected `initialEpsAppToIdNode`: the malformed wiring used to
allocate the free eraser at address 2, so the root 2 appeared.
-}
epsApplyToIdRoots :: TestTree
epsApplyToIdRoots =
  testCase "eps fixture roots: no root 2, final root 1" $ do
    assertBool "root 2 must not appear" (2 `notElem` systemActualAnswer)
    assertEqual "final root" 1 (P.last systemActualAnswer)
 where
  systemActualAnswer =
    mealyCoreRoots @AgentSimpleLambda @2 @2 @2
      initialEpsAppToIdNode
      initialEpsAppToIdMM
      50
      1

mealyCoreUnitTests :: TestTree
mealyCoreUnitTests =
  testGroup
    "mealy Core"
    [ idApplyToIdMealyCore
    , idApplyToIdMealyCoreEdsl
    , idApplyToIdEdslRoot
    , epsApplyToIdSimpleMealyCore
    , epsApplyToIdMealyCore
    , epsApplyToIdRoots
    ]
