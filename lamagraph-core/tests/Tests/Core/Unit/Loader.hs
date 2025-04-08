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
import Core.Concrete.ReduceRulesLambda
import Core.Loader (loadActivePair, loadInterface)
import Core.MemoryManager.MemoryManager
import Core.MemoryManager.NodeChanges
import Core.Node
import qualified Prelude as P

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

activePairGettingComplexTemplate ::
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
  Vec cellsNumber (Maybe (Node portsNumber agentType)) ->
  [Maybe AddressNumber] ->
  [Maybe (ActivePair portsNumber agentType)] ->
  TestTree
activePairGettingComplexTemplate testName initialNet activeAddresses expectedActivePairs =
  testCase ("load active pair: " P.++ testName)
    $ assertBool
      ( "expected:\n"
          P.++ show expectedActivePairs
          P.++ "\nactual:\n"
          P.++ showX systemActualAnswer
      )
      answersAreEqual
 where
  systemActualAnswer =
    sampleWithResetN @System
      d1
      (P.length activeAddresses - 1)
      ( loadActivePair
          (exposeEnable (blockRam initialNet))
          (fromList activeAddresses)
      )
  answersAreEqual = P.all (`elem` systemActualAnswer) expectedActivePairs

interfaceReadWriteTemplate ::
  forall agentType cellsNumber portsNumber externalNodesNumber.
  ( KnownNat portsNumber
  , KnownNat cellsNumber
  , KnownNat externalNodesNumber
  , 1 <= cellsNumber
  , CLog 2 cellsNumber <= BitSize AddressNumber
  , NFDataX agentType
  , Show agentType
  , ShowX agentType
  , Eq agentType
  ) =>
  TestName ->
  Vec cellsNumber (Maybe (Node portsNumber agentType)) ->
  Interface externalNodesNumber ->
  Vec externalNodesNumber (Maybe (LoadedNode portsNumber agentType)) ->
  Vec externalNodesNumber (Maybe (LoadedNode portsNumber agentType)) ->
  TestTree
interfaceReadWriteTemplate testName initialNet interface changedExNodes expectedNotWrittenInterface =
  testCase ("load interface: " P.++ testName)
    $ assertBool
      ( "expected:\n"
          P.++ show expectedNotWrittenInterface
          P.++ "\n"
          P.++ show changedExNodes
          P.++ "\nactual:\n"
          P.++ showX systemActualAnswer
      )
      answersAreEqual
 where
  systemActualAnswer =
    sampleWithResetN @System
      d1
      2
      ( loadInterface
          (exposeEnable (blockRam initialNet))
          (pure interface)
          (pure changedExNodes)
      )
  answersAreEqual = expectedNotWrittenInterface `elem` systemActualAnswer && changedExNodes `elem` systemActualAnswer

interfaceReadWriteComplexTemplate ::
  forall agentType cellsNumber portsNumber externalNodesNumber.
  ( KnownNat portsNumber
  , KnownNat cellsNumber
  , KnownNat externalNodesNumber
  , 1 <= cellsNumber
  , CLog 2 cellsNumber <= BitSize AddressNumber
  , NFDataX agentType
  , Show agentType
  , ShowX agentType
  , Eq agentType
  ) =>
  TestName ->
  Vec cellsNumber (Maybe (Node portsNumber agentType)) ->
  [Interface externalNodesNumber] ->
  [Vec externalNodesNumber (Maybe (LoadedNode portsNumber agentType))] ->
  Vec externalNodesNumber (Maybe (LoadedNode portsNumber agentType)) ->
  TestTree
interfaceReadWriteComplexTemplate testName initialNet interface changedExNodes expectedNotWrittenInterface =
  testCase ("load interface: " P.++ testName)
    $ assertBool
      ( "expected:\n"
          P.++ show expectedNotWrittenInterface
          P.++ "\n"
          P.++ show changedExNodes
          P.++ "\nactual:\n"
          P.++ showX systemActualAnswer
      )
      answersAreEqual
 where
  systemActualAnswer =
    sampleWithResetN @System
      d1
      (P.length changedExNodes)
      ( loadInterface
          (exposeEnable (blockRam initialNet))
          (fromList interface)
          (fromList changedExNodes)
      )
  answersAreEqual =
    expectedNotWrittenInterface
      `elem` systemActualAnswer
      && P.last changedExNodes
      `elem` systemActualAnswer
      && changedExNodes
      P.!! 3
      `elem` systemActualAnswer

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

activePairGettingComplex :: TestTree
activePairGettingComplex =
  activePairGettingComplexTemplate
    "eps to id both active pair"
    initialEpsAppToIdNode
    [def, Just 1, Just 1, Just 1, def, def, Just 3, Just 3, Just 3]
    [ ActivePair <$> initialEpsAppToId !! 1 <*> (initialEpsAppToId !! 2)
    , def
    , def
    , ActivePair <$> initialEpsAppToId !! 3 <*> (initialEpsAppToId !! 4)
    ]

interfaceReadWriteIdToIdChangeToErase :: TestTree
interfaceReadWriteIdToIdChangeToErase =
  interfaceReadWriteTemplate
    "change 1 node (id) to erase"
    initialIdApplyToIdNode
    (Just 1 :> Nil)
    (Just eraseNode :> Nil)
    (Just absIdNode :> Nil)
 where
  eraseNode =
    let prPort = Connected $ Port 0 (Id 1)
        secPorts = def
     in LoadedNode (Node prPort secPorts Erase) 1
  absIdNode =
    let prPort = Connected $ Port 0 (Id 1)
        port0 = Connected $ Port 1 (Id 1)
        port1 = Connected $ Port 1 (Id 0)
        secPorts = Just port0 :> Just port1 :> Nil
     in LoadedNode (Node prPort secPorts Abstract) 1

interfaceReadWriteComplexIdToIdChangeToErase :: TestTree
interfaceReadWriteComplexIdToIdChangeToErase =
  interfaceReadWriteComplexTemplate
    "change 1 node (id) to erase complex"
    initialIdApplyToIdNode
    (def : P.replicate 7 (Just 1 :> Nil) P.++ [def, def] P.++ P.replicate 7 (Just 1 :> Nil))
    ( [def, def] P.++ P.replicate 5 (Just eraseNode1 :> Nil) P.++ [def, def, def] P.++ P.replicate 5 (Just eraseNode2 :> Nil)
    )
    (Just absIdNode :> Nil)
 where
  eraseNode1 =
    let prPort = Connected $ Port 0 (Id 1)
        secPorts = def
     in LoadedNode (Node prPort secPorts Erase) 1
  eraseNode2 =
    let prPort = Connected $ Port 1 (Id 0)
        secPorts = def
     in LoadedNode (Node prPort secPorts Erase) 1
  absIdNode =
    let prPort = Connected $ Port 0 (Id 1)
        port0 = Connected $ Port 1 (Id 1)
        port1 = Connected $ Port 1 (Id 0)
        secPorts = Just port0 :> Just port1 :> Nil
     in LoadedNode (Node prPort secPorts Abstract) 1

loaderUnitTests :: TestTree
loaderUnitTests =
  testGroup
    "Load functions"
    [ activePairGettingIdToId
    , activePairGettingEpsToId
    , activePairGettingEpsToIdSimple
    , activePairGettingIdErase
    , activePairGettingComplex
    , interfaceReadWriteIdToIdChangeToErase
    , interfaceReadWriteComplexIdToIdChangeToErase
    ]
