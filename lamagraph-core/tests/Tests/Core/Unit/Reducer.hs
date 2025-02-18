{-# LANGUAGE PartialTypeSignatures #-}
{-# OPTIONS_GHC -Wno-partial-type-signatures #-}

module Tests.Core.Unit.Reducer where

import qualified Clash.Prelude as C
import Core.MemoryManager.NodeChanges
import Core.Reducer

-- import qualified Hedgehog as H
-- import qualified Hedgehog.Gen as Gen
import Test.Tasty
import Test.Tasty.HUnit

-- import Test.Tasty.Hedgehog
-- import Test.Tasty.TH

import Clash.Explicit.Verification (RenderAs (SVA), check, cover)
import Core.Concrete.Initial
import Core.Concrete.ReduceRulesLambda
import Core.MemoryManager.MemoryManager
import Core.Node
import Data.Text
import INet.Net
import Prelude

-- | <<docs/eraseEraseEmptyInterface.svg>>
noInterface :: TestTree
noInterface =
  testCase "No interface active pair" $
    getInterface @2 (ActivePair leftActiveNode rightActiveNode) @?= (C.def :: Interface 2)
 where
  leftActiveNode = LoadedNode (Node (Connected (Port 0 Primary)) emptySecPorts Erase) 1
  rightActiveNode = LoadedNode (Node (Connected (Port 1 Primary)) emptySecPorts Erase) 0
  emptySecPorts = C.def

-- | <<docs/absApplyEmptyInterface.svg>>
emptyInterface :: TestTree
emptyInterface =
  testCase "Empty interface active pair" $
    getInterface @2 (ActivePair leftActiveNode rightActiveNode) @?= (C.def :: Interface 2)
 where
  leftActiveNode = LoadedNode (Node (Connected (Port 0 Primary)) notConnectedSecPorts Apply) 1
  rightActiveNode = LoadedNode (Node (Connected (Port 1 Primary)) notConnectedSecPorts Abstract) 0
  notConnectedSecPorts = C.repeat (Just NotConnected)

-- | <<docs/absApplyNonEmpty.svg>>
nonEmptyInterface :: TestTree
nonEmptyInterface =
  testCase "Nonempty interface active pair" $
    actual @?= expected
 where
  leftActiveNode = LoadedNode (Node (Connected (Port 0 Primary)) externalPorts Abstract) 1
  rightActiveNode = LoadedNode (Node (Connected $ Port 1 Primary) C.def Erase) 0
  externalPorts = Just NotConnected C.:> Just (Connected $ Port 2 $ Id 1) C.:> C.Nil
  expected = Just 2 C.:> Nothing C.:> C.Nil
  actual = getInterface @2 @2 (ActivePair leftActiveNode rightActiveNode)

-- | <<docs/absApplyRecInterface.svg>>
nonEmptyRecursiveInterface :: TestTree
nonEmptyRecursiveInterface =
  testCase "Nonempty recursive interface active pair" $
    actual @?= expected
 where
  leftActiveNode = LoadedNode (Node (Connected (Port 0 Primary)) externalPorts Abstract) 1
  rightActiveNode = LoadedNode (Node (Connected $ Port 1 Primary) externalPorts' Apply) 0
  externalPorts = Just NotConnected C.:> Just (Connected $ Port 0 $ Id 1) C.:> C.Nil
  externalPorts' = Just NotConnected C.:> Just (Connected $ Port 1 $ Id 1) C.:> C.Nil
  expected = C.def
  actual = getInterface @2 @2 (ActivePair leftActiveNode rightActiveNode)

reduceIdAppId :: TestTree
reduceIdAppId =
  testCase "reduce Id apply to Id" $
    assertBool
      ("expected:\n" ++ show expectedDelta ++ "\nactual:\n" ++ show (Prelude.head (C.sampleN 1 systemActualDelta)))
      deltasAreEqual
 where
  (Just applyNode C.:> Just abs1Node C.:> Just abs2Node C.:> _) = initialIdApplyToId
  acPair = ActivePair applyNode abs2Node
  expectedEdges =
    Just (Edge NotConnected (Connected $ Port 1 Primary))
      C.:> Nothing -- Just (Edge (Connected $ Port 1 Primary) (Connected $ Port 2 $ Id 1))
      C.:> C.Nil
  expectedDelta = Delta (C.def :: C.Vec 2 _) expectedEdges acPair :: Delta 2 2 2 AgentSimpleLambda
  (systemActualDelta, _) =
    reduce getReduceRuleInfo (pure initialIdApplyToIdMM) (pure acPair) ::
      (C.Signal C.System (Delta 2 2 2 AgentSimpleLambda), _)
  deltasAreEqual = or (C.sampleN 1 ((C..==. systemActualDelta) $ pure expectedDelta))

reducerUnitTests :: TestTree
reducerUnitTests =
  testGroup
    "Unit tests"
    [ noInterface
    , emptyInterface
    , nonEmptyInterface
    , nonEmptyRecursiveInterface
    , reduceIdAppId
    ]
