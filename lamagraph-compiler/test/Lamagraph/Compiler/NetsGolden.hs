module Lamagraph.Compiler.NetsGolden (initialConfigurationGolden, reducedConfigurationGolden, outNetGolden) where

import Relude

import Lamagraph.Compiler.Nets.Examples
import System.FilePath
import Test.Tasty
import Test.Tasty.Golden

import Lamagraph.Compiler.GoldenCommon
import Lamagraph.Compiler.Nets.Processing
import Lamagraph.Compiler.Nets.Reduction (reduce)
import Lamagraph.Compiler.Nets.Types

initialConfigurationGoldenDir :: FilePath
initialConfigurationGoldenDir = baseGoldenTestsDir </> "nets" </> "initial_configuration"

reducedConfigurationGoldenDir :: FilePath
reducedConfigurationGoldenDir = baseGoldenTestsDir </> "nets" </> "reduced_configuration"

outNetGoldenDir :: FilePath
outNetGoldenDir = baseGoldenTestsDir </> "nets" </> "out_net"

tests :: [([Char], Net Peano, Rule Peano)]
tests =
  [ ("peanoNet1", peanoNet1, peanoRule)
  , ("peanoNet2", peanoNet2, peanoRule)
  ]

initialConfigurationGolden :: IO TestTree
initialConfigurationGolden = do
  return $
    testGroup
      "Initial Configuration Golden tests"
      [ goldenVsString name resultFilename (helper net)
      | (name, net, _) <- tests
      , let resultFilename = initialConfigurationGoldenDir </> (name ++ ".out")
      ]
 where
  helper net = show <$> runINsMachine (netToConfiguration net)

reducedConfigurationGolden :: IO TestTree
reducedConfigurationGolden = do
  return $
    testGroup
      "Reduced Configuration Golden tests"
      [ goldenVsString name resultFilename (helper net rule)
      | (name, net, rule) <- tests
      , let resultFilename = reducedConfigurationGoldenDir </> (name ++ ".out")
      ]
 where
  helper net rule = show <$> runINsMachine (netToConfiguration net >>= reduce rule)

outNetGolden :: IO TestTree
outNetGolden = do
  return $
    testGroup
      "Output Net Golden tests"
      [ goldenVsString name resultFilename (helper net rule)
      | (name, net, rule) <- tests
      , let resultFilename = outNetGoldenDir </> (name ++ ".out")
      ]
 where
  helper net rule = show <$> runINsMachine (netToConfiguration net >>= reduce rule >>= update >>= configurationToNet)
