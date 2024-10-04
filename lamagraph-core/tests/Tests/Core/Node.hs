{-# LANGUAGE PartialTypeSignatures #-}
{-# OPTIONS_GHC -Wno-partial-type-signatures #-}

module Tests.Core.Node where

import qualified Clash.Prelude as C
import qualified Clash.Sized.Vector as Vec
import Core.Node
import qualified Hedgehog as H
import qualified Hedgehog.Gen as Gen
import NodeGenerate (genAddress, genLoadedNodeByGivenAddresses, genMbObjectsVec, genPortVisitedFlag)
import Test.Tasty
import Test.Tasty.Hedgehog
import Test.Tasty.TH
import Prelude

prop_isPortToLoad_diffAddr :: H.Property
prop_isPortToLoad_diffAddr = H.property $ do
  address <- H.forAll genAddress
  isVisited <- H.forAll Gen.bool
  let
    port = Port address isVisited
    node = Node port C.Nil
    loadedNode = LoadedNode node (address + 1)

  isPortToLoad loadedNode port H.=== not isVisited

prop_isPortToLoad_sameAddr :: H.Property
prop_isPortToLoad_sameAddr = H.property $ do
  address <- H.forAll genAddress
  isVisited <- H.forAll Gen.bool
  let
    port = Port address isVisited
    node = Node port C.Nil
    loadedNode = LoadedNode node address
  isPortToLoad loadedNode port H.=== False

prop_isActive_crossreference :: H.Property
prop_isActive_crossreference = H.property $ do
  leftAddress <- H.forAll genAddress
  rightAddress <- H.forAll genAddress
  let
    leftPrimPort = Port rightAddress False
    rightPrimPort = Port leftAddress False
    leftNode = Node leftPrimPort C.Nil
    rightNode = Node rightPrimPort C.Nil
    leftLoadedNode = LoadedNode leftNode leftAddress
    rightLoadedNode = LoadedNode rightNode rightAddress
  H.assert $ isActive leftLoadedNode rightLoadedNode

prop_isActive_random :: H.Property
prop_isActive_random = H.property $ do
  leftAddress <- H.forAll genAddress
  rightAddress <- H.forAll genAddress
  leftPortAddr <- H.forAll genAddress
  rightPortAddr <- H.forAll genAddress
  leftIsVisited <- H.forAll Gen.bool
  rightIsVisited <- H.forAll Gen.bool
  let
    leftPrimPort = Port leftPortAddr leftIsVisited
    leftNode = Node leftPrimPort C.Nil
    leftLoadedNode = LoadedNode leftNode leftAddress
    rightPrimPort = Port rightPortAddr rightIsVisited
    rightNode = Node rightPrimPort C.Nil
    rightLoadedNode = LoadedNode rightNode rightAddress
  isActive leftLoadedNode rightLoadedNode H.=== (leftAddress == rightPortAddr && rightAddress == leftPortAddr)

prop_selectAddressToLoad_primary_unvisited :: H.Property
prop_selectAddressToLoad_primary_unvisited = H.property $ do
  nodeAddress <- H.forAll genAddress
  portTargetAddress <- H.forAll genAddress
  let
    port = Port portTargetAddress False
    node = Node port C.Nil
    loadedNode = LoadedNode node nodeAddress
  case selectAddressToLoad loadedNode of
    Nothing -> nodeAddress H.=== portTargetAddress
    Just address -> address H.=== portTargetAddress

prop_selectAddressToLoad_all_visited :: H.Property
prop_selectAddressToLoad_all_visited = H.property $ do
  primPort <- H.forAll $ genPortVisitedFlag True
  secondaryPortsVec <- H.forAll $ genMbObjectsVec (genPortVisitedFlag True) :: _ (C.Vec 10 _) -- 10 is random number, it can be changed
  address <- H.forAll genAddress
  let
    node = Node primPort secondaryPortsVec
    loadedNode = LoadedNode node address
  selectAddressToLoad loadedNode H.=== Nothing

prop_markAllInnerEdges_empty_vec_of_nodes :: H.Property
prop_markAllInnerEdges_empty_vec_of_nodes = H.property $ do
  let nodes = C.def :: C.Vec 10 (Maybe (LoadedNode 10))
  nodes H.=== markAllInnerEdges nodes

prop_markAllInnerEdges_uniq_addresses :: H.Property
prop_markAllInnerEdges_uniq_addresses = H.property $ do
  let
    uniqAddresses = C.iterateI (+ 1) 1 :: C.Vec 21 Address -- it can be obtained by using function genUniqAddresses, but it is very slow
    preDataToGenLoadedNodes = window 7 $ Vec.toList uniqAddresses
    listOfLoadedNodesGen = map genLoadedNode preDataToGenLoadedNodes
    genVecOfUniqLoadedNodes = sequence $ Vec.unsafeFromList listOfLoadedNodesGen :: H.Gen (C.Vec 3 (LoadedNode 5))
  vecOfUniqLoadedNodes <- H.forAll genVecOfUniqLoadedNodes
  let vecOfMbUniqLoadedNodes = Just <$> vecOfUniqLoadedNodes
  markAllInnerEdges vecOfMbUniqLoadedNodes H.=== vecOfMbUniqLoadedNodes
 where
  window size list = case splitAt size list of
    ([], []) -> []
    (xs, remainder) -> xs : window size remainder
  genLoadedNode list = genLoadedNodeByGivenAddresses (head list) (list !! 1) (drop 2 list)

prop_markAllInnerEdges_crossreference :: H.Property
prop_markAllInnerEdges_crossreference = H.property $ do
  addressOfNode1 <- H.forAll genAddress
  addressOfNode2 <- H.forAll genAddress
  port1 <- H.forAll $ genPortVisitedFlag True
  let
    loadedNode1 = Just $ LoadedNode (Node port1 C.Nil) addressOfNode1
    loadedNode2 = Just $ LoadedNode (Node (Port addressOfNode1 False) C.Nil) addressOfNode2
    expectedResult = loadedNode1 C.:> Just (LoadedNode (Node (Port addressOfNode1 True) C.Nil) addressOfNode2) C.:> C.Nil
    actualResult = markAllInnerEdges (loadedNode1 C.:> loadedNode2 C.:> C.Nil)

  actualResult H.=== expectedResult

accumTests :: TestTree
accumTests = $(testGroupGenerator)

main :: IO ()
main = defaultMain accumTests
