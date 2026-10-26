{-# LANGUAGE PartialTypeSignatures #-}
{-# OPTIONS_GHC -Wno-partial-type-signatures #-}

{- | Unit tests: the hand-written-rule reduction pipeline ("Core.Reducer").

Fixed fixtures from "Core.Concrete.Initial" (via
"Tests.Core.Unit.Common.Fixtures") are reduced with `reduce`, and the resulting
`Delta` is compared against a literal. The `getInterface` tests pin the external
neighbour collection on hand-built pairs.
-}
module Tests.Core.Unit.Reducer where

import qualified Clash.Prelude as C
import Core.Concrete.Initial
import Core.Concrete.ReduceRulesLambda
import Core.MemoryManager.MemoryManager
import Core.MemoryManager.NodeChanges
import Core.Node
import Core.Reducer

import Test.Tasty
import Test.Tasty.HUnit

import Tests.Core.Unit.Common.Assert (assertEqualShow)
import Tests.Core.Unit.Common.Fixtures (
  mkActivePair,
  pairEpsAppToId,
  pairEpsAppToIdSimple,
  pairIdApplyToId,
  pairIdErase,
  pairLoopEdge,
 )
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
    assertEqualShow expectedDelta systemActualDelta
 where
  acPair = mkActivePair pairIdApplyToId
  expectedEdges =
    Just (Edge (Connected $ Port 1 Primary) NotConnected)
      C.:> Nothing
      C.:> C.Nil
  expectedDelta = Delta (C.def :: C.Vec 2 _) expectedEdges acPair :: Delta 2 2 2 AgentSimpleLambda
  (systemActualDelta, _) =
    reduce initialIdApplyToIdMM acPair :: (Delta 2 2 2 AgentSimpleLambda, _)

-- | <<docs/reduceLoopEdge.svg>>
reduceLoopEdge :: TestTree
reduceLoopEdge =
  testCase "reduce closed lambda and closed apply agents" $
    assertEqualShow expectedDelta systemActualDelta
 where
  acPair = mkActivePair pairLoopEdge
  expectedEdges =
    Nothing
      C.:> Nothing
      C.:> C.Nil
  expectedDelta = Delta (C.def :: C.Vec 2 _) expectedEdges acPair :: Delta 2 2 2 AgentSimpleLambda
  (systemActualDelta, _) =
    reduce initialLoopEdgeMM acPair :: (Delta 2 2 2 AgentSimpleLambda, _)

reduceEpsAppId :: TestTree
reduceEpsAppId =
  testCase "reduce Id apply to Eps" $
    assertEqualShow expectedDelta systemActualDelta
 where
  acPair = mkActivePair pairEpsAppToId
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
    reduce initialEpsAppToIdMM acPair :: (Delta 2 2 2 AgentSimpleLambda, _)

reduceEpsAppIdSimple :: TestTree
reduceEpsAppIdSimple =
  testCase "reduce Id apply to Eps Simple" $
    assertEqualShow expectedDelta systemActualDelta
 where
  acPair = mkActivePair pairEpsAppToIdSimple
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
    reduce initialEpsAppToIdSimpleMM acPair :: (Delta 2 2 2 AgentSimpleLambda, _)

reduceEraseId :: TestTree
reduceEraseId =
  testCase "reduce Erase Id" $
    assertEqualShow expectedDelta systemActualDelta
 where
  acPair = mkActivePair pairIdErase
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
    reduce initialIdEraseMM acPair :: (Delta 2 2 2 AgentSimpleLambda, _)

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
