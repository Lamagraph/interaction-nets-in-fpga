module Tests.Core.Node where

import Prelude

import Clash.Hedgehog.Sized.Unsigned
import Test.Tasty
import Test.Tasty.Hedgehog
import Test.Tasty.TH

import qualified Clash.Prelude as C
import qualified Hedgehog as H
import qualified Hedgehog.Range as Range

import Core.Node

prop_isPortToLoad :: H.Property
prop_isPortToLoad = H.property $ do
  address <- H.forAll $ genUnsigned $ Range.singleton 16
  let
    port = Port address False
    newNode = Node port C.Nil
    loadedNode = LoadedNode newNode (address + 1)

  isPortToLoad loadedNode port H.=== True

accumTests :: TestTree
accumTests = $(testGroupGenerator)

main :: IO ()
main = defaultMain accumTests
