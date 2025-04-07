{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE PartialTypeSignatures #-}
{-# OPTIONS_GHC -Wno-incomplete-uni-patterns #-}
{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-partial-type-signatures #-}
{-# OPTIONS_GHC -Wno-type-defaults #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Eta reduce" #-}
{-# OPTIONS_GHC -Wno-unused-local-binds #-}

module Tests.Core.Unit.Loader where

import Clash.Prelude
import Test.Tasty
import Test.Tasty.HUnit

import Core.Concrete.Initial
import Core.Loader (loadActivePair)
import Core.MemoryManager.MemoryManager
import Core.Node
import qualified Prelude as P

-- activePairGettingTemplate :: TestTree
activePairGettingTemplate ::
  forall agentType cellsNumber portsNumber.
  ( KnownNat portsNumber
  , KnownNat cellsNumber
  , 1 <= cellsNumber
  , CLog 2 cellsNumber <= BitSize AddressNumber
  , NFDataX agentType
  , Show agentType
  , ShowX agentType
  , Eq agentType
  ) =>
  TestName ->
  MemoryManager cellsNumber ->
  Vec cellsNumber (Maybe (Node portsNumber agentType)) ->
  Maybe (ActivePair portsNumber agentType) ->
  TestTree
activePairGettingTemplate testName initialMM initialNet expectedActivePair =
  testCase ("load active pair: " P.++ testName)
    $ assertBool
      ( "expected:\n"
          P.++ show expectedActivePair
          P.++ "\nactual:\n"
          P.++ showX systemActualAnswer
      )
      answersAreEqual
 where
  systemMemoryManager = pure initialMM
  systemActualAnswer =
    sampleWithResetN @System
      d1
      2
      ( loadActivePair
          (exposeEnable (blockRam initialNet))
          (giveActiveAddressNumber systemMemoryManager)
      )
  answersAreEqual = expectedActivePair `elem` systemActualAnswer

activePairGettingIdToId :: TestTree
activePairGettingIdToId =
  activePairGettingTemplate
    "id apply to id"
    initialIdApplyToIdMM
    initialIdApplyToIdNode
    (ActivePair <$> head initialIdApplyToId <*> (initialIdApplyToId !! 2))

activePairGettingEpsToId :: TestTree
activePairGettingEpsToId =
  activePairGettingTemplate
    "eps apply to id"
    initialEpsAppToIdMM
    initialEpsAppToIdNode
    (ActivePair <$> (initialEpsAppToId !! 1) <*> (initialEpsAppToId !! 2))

activePairGettingEpsToIdSimple :: TestTree
activePairGettingEpsToIdSimple =
  activePairGettingTemplate
    "eps apply to id simple"
    initialEpsAppToIdSimpleMM
    initialEpsAppToIdSimpleNode
    (ActivePair <$> (initialEpsAppToIdSimple !! 1) <*> (initialEpsAppToIdSimple !! 2))

activePairGettingIdErase :: TestTree
activePairGettingIdErase =
  activePairGettingTemplate
    "id connected to erase"
    initialIdEraseMM
    initialIdEraseNode
    (ActivePair <$> head initialIdErase <*> (initialIdErase !! 1))

loaderUnitTests :: TestTree
loaderUnitTests =
  testGroup
    "Load functions"
    [ activePairGettingIdToId
    , activePairGettingEpsToId
    , activePairGettingEpsToIdSimple
    , activePairGettingIdErase
    ]
