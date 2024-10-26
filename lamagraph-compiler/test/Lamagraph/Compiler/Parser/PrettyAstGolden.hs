module Lamagraph.Compiler.Parser.PrettyAstGolden (parserPrettyAstGolden) where

import Relude

import Prettyprinter
import System.FilePath
import Test.Tasty
import Test.Tasty.Golden

import Lamagraph.Compiler.Parser
import Lamagraph.Compiler.Parser.GoldenCommon
import Lamagraph.Compiler.PrettyAst ()

newExt :: String
newExt = "ast"

newDir :: FilePath
newDir = ".." </> "ast"

parserPrettyAstGolden :: IO TestTree
parserPrettyAstGolden = do
  lmlFiles <- findByExtension [lmlExt] parserGoldenTestsDir
  return $
    testGroup
      "Pretty AST Golden tests"
      [ goldenVsString (takeBaseName lmlFile) resLmlFile (helper lmlFile)
      | lmlFile <- lmlFiles
      , let resLmlFile = addExtension (changeFileDir lmlFile newDir) newExt
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
