{-# LANGUAGE PartialTypeSignatures #-}
{-# OPTIONS_GHC -Wno-incomplete-uni-patterns #-}
{-# OPTIONS_GHC -Wno-partial-type-signatures #-}

module Tests.Core.Unit.Reducer where

import qualified Clash.Prelude as C
import Core.MemoryManager.NodeChanges
import Core.Reducer

import Test.Tasty
import Test.Tasty.HUnit

import Core.Concrete.Initial
import Core.Concrete.ReduceRulesLambda
import Core.MemoryManager.MemoryManager
import Core.Node

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

-- | <<docs/idToIdReduceRes.svg>>
reduceIdAppId :: TestTree
reduceIdAppId =
  testCase "reduce Id apply to Id" $
    assertBool
      ("expected:\n" ++ show expectedDelta ++ "\nactual:\n" ++ show (Prelude.head (C.sampleN 1 systemActualDelta)))
      deltasAreEqual
 where
  (Just applyNode C.:> Just _ C.:> Just abs2Node C.:> _) = initialIdApplyToId
  acPair = ActivePair applyNode abs2Node
  expectedEdges =
    Just (Edge (Connected $ Port 1 Primary) NotConnected)
      C.:> Nothing
      C.:> C.Nil
  expectedDelta = Delta (C.def :: C.Vec 2 _) expectedEdges acPair :: Delta 2 2 2 AgentSimpleLambda
  (systemActualDelta, _) =
    reduce getReduceRuleInfo (pure initialIdApplyToIdMM) (pure acPair) ::
      (C.Signal C.System (Delta 2 2 2 AgentSimpleLambda), _)
  deltasAreEqual = or (C.sampleN 1 ((C..==. systemActualDelta) $ pure expectedDelta))

-- | <<docs/reduceLoopEdge.svg>>
reduceLoopEdge :: TestTree
reduceLoopEdge =
  testCase "reduce closed lambda and closed apply agents" $
    assertBool
      ("expected:\n" ++ show expectedDelta ++ "\nactual:\n" ++ show (Prelude.head (C.sampleN 1 systemActualDelta)))
      deltasAreEqual
 where
  (Just applyNode C.:> Just absNode C.:> _) = initialLoopEdge
  acPair = ActivePair applyNode absNode
  expectedEdges =
    Nothing
      C.:> Nothing
      C.:> C.Nil
  expectedDelta = Delta (C.def :: C.Vec 2 _) expectedEdges acPair :: Delta 2 2 2 AgentSimpleLambda
  (systemActualDelta, _) =
    reduce getReduceRuleInfo (pure initialLoopEdgeMM) (pure acPair) ::
      (C.Signal C.System (Delta 2 2 2 AgentSimpleLambda), _)
  deltasAreEqual = or (C.sampleN 1 ((C..==. systemActualDelta) $ pure expectedDelta))

reducerUnitTests :: TestTree
reducerUnitTests =
  testGroup
    "Reducer"
    [ noInterface
    , emptyInterface
    , nonEmptyInterface
    , nonEmptyRecursiveInterface
    , reduceIdAppId
    , reduceLoopEdge
    ]
