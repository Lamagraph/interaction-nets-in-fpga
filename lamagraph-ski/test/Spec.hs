module Main (main) where

import Relude

import Test.Tasty
import Test.Tasty.HUnit

main :: IO ()
main = defaultMain $ testGroup "lamagraph-ski" [stubTest]

stubTest :: TestTree
stubTest = testCase "package scaffolds and builds" $ do
  let stubValue :: Int
      stubValue = 1
  stubValue @=? 1
