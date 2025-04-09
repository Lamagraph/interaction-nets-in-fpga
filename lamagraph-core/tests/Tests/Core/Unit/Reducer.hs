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
      ("expected:\n" ++ show expectedDelta ++ "\nactual:\n" ++ show systemActualDelta)
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
    reduceNoSignal getReduceRuleInfo initialIdApplyToIdMM acPair :: (Delta 2 2 2 AgentSimpleLambda, _)
  deltasAreEqual = systemActualDelta == expectedDelta

-- | <<docs/reduceLoopEdge.svg>>
reduceLoopEdge :: TestTree
reduceLoopEdge =
  testCase "reduce closed lambda and closed apply agents" $
    assertBool
      ("expected:\n" ++ show expectedDelta ++ "\nactual:\n" ++ show systemActualDelta)
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
    reduceNoSignal getReduceRuleInfo initialLoopEdgeMM acPair :: (Delta 2 2 2 AgentSimpleLambda, _)
  deltasAreEqual = systemActualDelta == expectedDelta

reduceEpsAppId :: TestTree
reduceEpsAppId =
  testCase "reduce Id apply to Eps" $
    assertBool
      ("expected:\n" ++ show expectedDelta ++ "\nactual:\n" ++ show systemActualDelta)
      deltasAreEqual
 where
  (_ C.:> _ C.:> _ C.:> Just applyEraseNode C.:> Just eraseNode C.:> _) = initialEpsAppToId
  acPair = ActivePair applyEraseNode eraseNode
  expectedEdges = C.def
  expectedNodes =
    Just
      ( LoadedNode
          ( Node
              (Connected (Port 2 (Id 1)))
              C.def
              Erase
          )
          6
      )
      C.:> Just
        ( LoadedNode
            ( Node
                (Connected (Port 2 (Id 0)))
                C.def
                Erase
            )
            5
        )
      C.:> C.Nil
  expectedDelta = Delta expectedNodes expectedEdges acPair :: Delta 2 2 2 AgentSimpleLambda
  (systemActualDelta, _) =
    reduceNoSignal getReduceRuleInfo initialEpsAppToIdMM acPair :: (Delta 2 2 2 AgentSimpleLambda, _)
  deltasAreEqual = systemActualDelta == expectedDelta

reduceEpsAppIdSimple :: TestTree
reduceEpsAppIdSimple =
  testCase "reduce Id apply to Eps Simple" $
    assertBool
      ("expected:\n" ++ show expectedDelta ++ "\nactual:\n" ++ show systemActualDelta)
      deltasAreEqual
 where
  (_ C.:> Just applyEraseNode C.:> Just eraseNode C.:> _) = initialEpsAppToIdSimple
  acPair = ActivePair applyEraseNode eraseNode
  expectedEdges = C.def
  expectedNodes =
    Just
      ( LoadedNode
          ( Node
              NotConnected
              C.def
              Erase
          )
          4
      )
      C.:> Just
        ( LoadedNode
            ( Node
                (Connected (Port 0 Primary))
                C.def
                Erase
            )
            3
        )
      C.:> C.Nil
  expectedDelta = Delta expectedNodes expectedEdges acPair :: Delta 2 2 2 AgentSimpleLambda
  (systemActualDelta, _) =
    reduceNoSignal getReduceRuleInfo initialEpsAppToIdSimpleMM acPair :: (Delta 2 2 2 AgentSimpleLambda, _)
  deltasAreEqual = systemActualDelta == expectedDelta

reduceEraseId :: TestTree
reduceEraseId =
  testCase "reduce Erase Id" $
    assertBool
      ("expected:\n" ++ show expectedDelta ++ "\nactual:\n" ++ show systemActualDelta)
      deltasAreEqual
 where
  (Just idNode C.:> Just eraseNode C.:> _) = initialIdErase
  acPair = ActivePair idNode eraseNode
  expectedEdges = C.def
  expectedNodes =
    Just
      ( LoadedNode
          ( Node
              (Connected (Port 2 Primary))
              C.def
              Erase
          )
          3
      )
      C.:> Just
        ( LoadedNode
            ( Node
                (Connected (Port 3 Primary))
                C.def
                Erase
            )
            2
        )
      C.:> C.Nil
  expectedDelta = Delta expectedNodes expectedEdges acPair :: Delta 2 2 2 AgentSimpleLambda
  (systemActualDelta, _) =
    reduceNoSignal getReduceRuleInfo initialIdEraseMM acPair :: (Delta 2 2 2 AgentSimpleLambda, _)
  deltasAreEqual = systemActualDelta == expectedDelta

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
    , reduceEpsAppIdSimple
    , reduceEpsAppId
    , reduceEraseId
    ]
