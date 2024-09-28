module Lamagraph.Compiler.ParserGoldenTest (parserGoldenTests) where

import Relude

import System.FilePath
import Test.Tasty
import Test.Tasty.Golden

import Lamagraph.Compiler.Parser

parserGoldenTests :: IO TestTree
parserGoldenTests = do
  lmlFiles <- findByExtension [".lml"] "test/parserGolden"
  return $
    testGroup
      "Golden tests"
      [ goldenVsString (takeBaseName lmlFile) resLmlFile (helper lmlFile)
      | lmlFile <- lmlFiles
      , let resLmlFile = replaceExtension lmlFile ".lml_golden"
      ]
 where
  helper :: FilePath -> IO LByteString
  helper lmlFile = (show <$> parseLamagraphML) . decodeUtf8 <$> readFileBS lmlFile
