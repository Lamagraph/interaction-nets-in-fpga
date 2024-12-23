module Lamagraph.Compiler.Parser.PrettyLmlGolden (parserPrettyLmlGolden) where

import Relude

import Prettyprinter
import System.FilePath
import Test.Tasty
import Test.Tasty.Golden

import Lamagraph.Compiler.GoldenCommon
import Lamagraph.Compiler.Parser
import Lamagraph.Compiler.PrettyLml ()

parserPrettyLmlGolden :: IO TestTree
parserPrettyLmlGolden = do
  lmlFiles <- findByExtension [lmlExt] parserSourceGoldenTestsDir
  return $
    testGroup
      "Pretty LML Golden tests"
      [ goldenVsString (takeBaseName lmlFile) resLmlFile (helper lmlFile)
      | lmlFile <- lmlFiles
      , let resLmlFile = changeFileDir lmlFile "../ppr"
      ]
 where
  helper :: FilePath -> IO LByteString
  helper lmlFile = do
    fileBS <- readFileBS lmlFile
    let fileT = decodeUtf8 fileBS
        parseResult = parseLamagraphML fileT
    pure $ case parseResult of
      Left err -> encodeUtf8 err
      Right tree -> encodeUtf8 $ (renderPretty . pretty) tree
