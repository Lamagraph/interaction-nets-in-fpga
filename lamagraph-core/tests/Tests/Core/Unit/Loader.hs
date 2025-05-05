{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE PartialTypeSignatures #-}
{-# OPTIONS_GHC -Wno-incomplete-uni-patterns #-}
{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-partial-type-signatures #-}
{-# OPTIONS_GHC -Wno-type-defaults #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Eta reduce" #-}
{-# OPTIONS_GHC -Wno-unused-local-binds #-}

{-# HLINT ignore "Use head" #-}

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
  forall agentType portsNumber.
  ( KnownNat portsNumber
  , NFDataX agentType
  , Show agentType
  , ShowX agentType
  , Eq agentType
  ) =>
  TestName ->
  MemoryManager ->
  Vec CellsNumber (Maybe (Node portsNumber agentType)) ->
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
  systemActualAnswer =
    sampleWithResetN @System
      d1
      2
      ( loadActivePair
          (exposeEnable (blockRam initialNet))
          (pure $ giveActiveAddressNumber initialMM)
      )
  answersAreEqual = expectedActivePair `elem` systemActualAnswer

activePairGettingComplexTemplate ::
  forall agentType portsNumber.
  ( KnownNat portsNumber
  , NFDataX agentType
  , Show agentType
  , ShowX agentType
  , Eq agentType
  ) =>
  TestName ->
  Vec CellsNumber (Maybe (Node portsNumber agentType)) ->
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
  forall agentType portsNumber externalNodesNumber.
  ( KnownNat portsNumber
  , KnownNat externalNodesNumber
  , NFDataX agentType
  , Show agentType
  , ShowX agentType
  , Eq agentType
  ) =>
  TestName ->
  Vec CellsNumber (Maybe (Node portsNumber agentType)) ->
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
  forall agentType portsNumber externalNodesNumber.
  ( KnownNat portsNumber
  , KnownNat externalNodesNumber
  , NFDataX agentType
  , Show agentType
  , ShowX agentType
  , Eq agentType
  ) =>
  TestName ->
  Vec CellsNumber (Maybe (Node portsNumber agentType)) ->
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

combinationComplexTemplate ::
  forall agentType portsNumber externalNodesNumber.
  ( KnownNat portsNumber
  , KnownNat externalNodesNumber
  , NFDataX agentType
  , Show agentType
  , ShowX agentType
  , Eq agentType
  ) =>
  TestName ->
  Vec CellsNumber (Maybe (Node portsNumber agentType)) ->
  [Maybe AddressNumber] ->
  [Interface externalNodesNumber] ->
  [Vec externalNodesNumber (Maybe (LoadedNode portsNumber agentType))] ->
  [Maybe (ActivePair portsNumber agentType)] ->
  Vec externalNodesNumber (Maybe (LoadedNode portsNumber agentType)) ->
  TestTree
combinationComplexTemplate testName initialNet activeAddresses interfaces changedExNodes expectedActivePairs expectedNotWrittenInterface =
  testCase ("load interface and active pair: " P.++ testName)
    $ assertBool
      ( "expected:\n"
          P.++ show expectedNotWrittenInterface
          P.++ "\n"
          P.++ show changedExNodes
          P.++ "\n"
          P.++ show expectedActivePairs
          P.++ "\nactual:\n"
          P.++ show systemActualAnswer
      )
      answersAreEqual
 where
  signalActiveAddress = fromList activeAddresses
  signalInterface = fromList interfaces
  signalChangedExNodes = fromList changedExNodes
  systemActualAnswer =
    sampleWithResetN @System
      d1
      (P.length changedExNodes - 1)
      ( bundle
          ( mux
              (signalActiveAddress ./=. def)
              ( loadActivePair
                  (exposeEnable (blockRam initialNet))
                  signalActiveAddress
              )
              def
          , mux
              (signalInterface ./=. def)
              ( loadInterface
                  (exposeEnable (blockRam initialNet))
                  signalInterface
                  signalChangedExNodes
              )
              def
          )
      )
  answersAreEqual =
    P.all (`elem` (fst <$> systemActualAnswer)) expectedActivePairs
      && P.all (`elem` (snd <$> systemActualAnswer)) (expectedNotWrittenInterface : changedExNodes)

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
        sPorts = def
     in LoadedNode (Node prPort sPorts Erase) 1
  absIdNode =
    let prPort = Connected $ Port 0 (Id 1)
        port0 = Connected $ Port 1 (Id 1)
        port1 = Connected $ Port 1 (Id 0)
        sPorts = Just port0 :> Just port1 :> Nil
     in LoadedNode (Node prPort sPorts Abstract) 1

interfaceReadWriteComplexIdToIdChangeToErase :: TestTree
interfaceReadWriteComplexIdToIdChangeToErase =
  interfaceReadWriteComplexTemplate
    "change 1 node (id) to erase complex"
    initialIdApplyToIdNode
    (def : P.replicate 7 (Just 1 :> Nil) P.++ [def, def] P.++ P.replicate 7 (Just 1 :> Nil))
    ([def, def] P.++ P.replicate 5 (Just eraseNode1 :> Nil) P.++ [def, def, def] P.++ P.replicate 5 (Just eraseNode2 :> Nil))
    (Just absIdNode :> Nil)
 where
  eraseNode1 =
    let prPort = Connected $ Port 0 (Id 1)
        sPorts = def
     in LoadedNode (Node prPort sPorts Erase) 1
  eraseNode2 =
    let prPort = Connected $ Port 1 (Id 0)
        sPorts = def
     in LoadedNode (Node prPort sPorts Erase) 1
  absIdNode =
    let prPort = Connected $ Port 0 (Id 1)
        port0 = Connected $ Port 1 (Id 1)
        port1 = Connected $ Port 1 (Id 0)
        sPorts = Just port0 :> Just port1 :> Nil
     in LoadedNode (Node prPort sPorts Abstract) 1

combinationComplex :: TestTree
combinationComplex =
  combinationComplexTemplate
    "id apply to eps"
    initialEpsAppToIdNode
    ( def
        : P.replicate activeAddressJustCount (Just 1)
        P.++ P.replicate interfaceJustCount def
        P.++ P.replicate activeAddressJustCount (Just 3)
        P.++ P.replicate interfaceJustCount def
    ) -- 14
    ( P.replicate (activeAddressJustCount + 1) def
        P.++ P.replicate interfaceJustCount interface1
        P.++ P.replicate activeAddressJustCount def
        P.++ P.replicate interfaceJustCount interface2
    ) -- 14
    ( P.replicate (activeAddressJustCount + 2) def
        P.++ P.replicate updatedJustCount updatedExternalNodes1
        P.++ P.replicate (activeAddressJustCount + 1) def
        P.++ P.replicate updatedJustCount updatedExternalNodes2
    )
    [def, activePair1, activePair2]
    (initialEpsAppToId !! 3 :> initialEpsAppToId !! 0 :> Nothing :> Nothing :> Nil)
 where
  interfaceJustCount = 4
  updatedJustCount = interfaceJustCount - 1
  activeAddressJustCount = interfaceJustCount + 1
  interface1 = Just 3 :> Just 0 :> replicate d2 def
  interface2 = Just 0 :> replicate d3 def
  updatedExternalNodes1 =
    Just
      LoadedNode
        { _containedNode =
            Node
              { _primaryPort = Connected Port{_nodeAddress = 4, _portConnectedToId = Primary}
              , _secondaryPorts = Just Nothing :> Just (Connected (Port{_nodeAddress = 0, _portConnectedToId = Primary})) :> Nil
              , _nodeType = Apply
              }
        , _originalAddress = 3
        }
      :> Just
        LoadedNode
          { _containedNode =
              Node
                { _primaryPort = Connected Port{_nodeAddress = 3, _portConnectedToId = Id 1}
                , _secondaryPorts =
                    Just (Connected (Port{_nodeAddress = 0, _portConnectedToId = Id 1}))
                      :> Just (Connected (Port{_nodeAddress = 0, _portConnectedToId = Id 0}))
                      :> Nil
                , _nodeType = Abstract
                }
          , _originalAddress = 0
          }
      :> Nothing
      :> Nothing
      :> Nil
  updatedExternalNodes2 =
    Just
      LoadedNode
        { _containedNode =
            Node
              { _primaryPort = Connected Port{_nodeAddress = 1, _portConnectedToId = Primary}
              , _secondaryPorts =
                  Just (Connected (Port{_nodeAddress = 0, _portConnectedToId = Id 1}))
                    :> Just (Connected (Port{_nodeAddress = 0, _portConnectedToId = Id 0}))
                    :> Nil
              , _nodeType = Abstract
              }
        , _originalAddress = 0
        }
      :> Nothing
      :> Nothing
      :> Nothing
      :> Nil
  activePair1 = ActivePair <$> initialEpsAppToId !! 1 <*> (initialEpsAppToId !! 2)
  activePair2 =
    Just
      $ ActivePair
        ( LoadedNode
            { _containedNode =
                Node
                  { _primaryPort = Connected Port{_nodeAddress = 4, _portConnectedToId = Primary}
                  , _secondaryPorts =
                      Just NotConnected
                        :> Just (Connected (Port{_nodeAddress = 0, _portConnectedToId = Primary}))
                        :> Nil
                  , _nodeType = Apply
                  }
            , _originalAddress = 3
            }
        )
        ( LoadedNode
            { _containedNode =
                Node
                  { _primaryPort = Connected Port{_nodeAddress = 3, _portConnectedToId = Primary}
                  , _secondaryPorts = replicate d2 def
                  , _nodeType = Erase
                  }
            , _originalAddress = 4
            }
        )

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
    -- , combinationComplex
    ]
