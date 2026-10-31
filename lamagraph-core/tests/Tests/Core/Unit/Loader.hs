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

{- | Unit tests: the signal-level RAM access of "Core.Loader".

Each test samples a fixed number of cycles of the loader circuit built on the
fixture net (via "Tests.Core.Unit.Common.Sim".`netRam`) and checks that the
expected answer shows up in the sampled window.
-}
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
import qualified Data.Foldable as DF
import qualified Data.Maybe as DM
import Tests.Core.Unit.Common.Assert (assertDump)
import Tests.Core.Unit.Common.Fixtures (
  mkActivePair,
  pairEpsAppToIdRoot,
  pairEpsAppToIdSimple,
  pairIdApplyToId,
  pairIdErase,
 )
import Tests.Core.Unit.Common.Sim (netRam, sampleSystemN)
import Tests.Core.Unit.Common.Symmetry (
  assertSymmetricActivePair,
  assertSymmetricFixtureNet,
 )
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
  testCase ("load active pair: " P.++ testName) $ do
    assertSymmetricFixtureNet testName initialNet
    P.mapM_
      (assertSymmetricActivePair (testName P.++ " result"))
      (DM.catMaybes systemActualAnswer)
    DF.for_ expectedActivePair (assertSymmetricActivePair (testName P.++ " expected"))
    assertDump (show expectedActivePair) (showX systemActualAnswer) answersAreEqual
 where
  systemActualAnswer =
    sampleSystemN
      2
      ( loadActivePair
          (netRam initialNet)
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
  testCase ("load active pair: " P.++ testName) $
    assertDump (show expectedActivePairs) (showX systemActualAnswer) answersAreEqual
 where
  systemActualAnswer =
    sampleSystemN
      (P.length activeAddresses - 1)
      ( loadActivePair
          (netRam initialNet)
          (fromList activeAddresses)
      )
  answersAreEqual = P.all (`elem` systemActualAnswer) expectedActivePairs

{- | Interface vectors are both read results and write payloads: the tests
inject arbitrary nodes there (including erase nodes whose primary points at a
padding slot), so they are not nets and are not symmetry-checked. Only the
input fixture and loaded active pairs are.
-}
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
  testCase ("load interface: " P.++ testName) $ do
    assertSymmetricFixtureNet testName initialNet
    assertDump expectedDump (showX systemActualAnswer) answersAreEqual
 where
  expectedDump = show expectedNotWrittenInterface P.++ "\n" P.++ show changedExNodes
  systemActualAnswer =
    sampleSystemN
      2
      ( loadInterface
          (netRam initialNet)
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
  testCase ("load interface: " P.++ testName) $ do
    assertSymmetricFixtureNet testName initialNet
    assertDump expectedDump (showX systemActualAnswer) answersAreEqual
 where
  expectedDump = show expectedNotWrittenInterface P.++ "\n" P.++ show changedExNodes
  systemActualAnswer =
    sampleSystemN
      (P.length changedExNodes)
      ( loadInterface
          (netRam initialNet)
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
  testCase ("load interface and active pair: " P.++ testName) $ do
    assertSymmetricFixtureNet testName initialNet
    P.mapM_ checkPair (fst <$> systemActualAnswer)
    P.mapM_ checkPair expectedActivePairs
    assertDump expectedDump (show systemActualAnswer) answersAreEqual
 where
  checkPair maybePair =
    DF.for_ maybePair (assertSymmetricActivePair (testName P.++ " result pair"))
  expectedDump =
    show expectedNotWrittenInterface
      P.++ "\n"
      P.++ show changedExNodes
      P.++ "\n"
      P.++ show expectedActivePairs
  signalActiveAddress = fromList activeAddresses
  signalInterface = fromList interfaces
  signalChangedExNodes = fromList changedExNodes
  systemActualAnswer =
    sampleSystemN
      (P.length changedExNodes - 1)
      ( bundle
          ( mux
              (signalActiveAddress ./=. def)
              ( loadActivePair
                  (netRam initialNet)
                  signalActiveAddress
              )
              def
          , mux
              (signalInterface ./=. def)
              ( loadInterface
                  (netRam initialNet)
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
    (Just (mkActivePair pairIdApplyToId))

activePairGettingEpsToId :: TestTree
activePairGettingEpsToId =
  activePairGettingTemplate
    "eps apply to id"
    initialEpsAppToIdMM
    initialEpsAppToIdNode
    (Just (mkActivePair pairEpsAppToIdRoot))

activePairGettingEpsToIdSimple :: TestTree
activePairGettingEpsToIdSimple =
  activePairGettingTemplate
    "eps apply to id simple"
    initialEpsAppToIdSimpleMM
    initialEpsAppToIdSimpleNode
    (Just (mkActivePair pairEpsAppToIdSimple))

activePairGettingIdErase :: TestTree
activePairGettingIdErase =
  activePairGettingTemplate
    "id connected to erase"
    initialIdEraseMM
    initialIdEraseNode
    (Just (mkActivePair pairIdErase))

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

{- | Disabled: kept as a written-out fixture for the combined
active-pair\/interface load. Not part of `loaderUnitTests` below.
-}
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
    Just $
      ActivePair
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
