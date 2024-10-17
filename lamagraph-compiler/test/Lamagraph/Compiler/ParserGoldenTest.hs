module Lamagraph.Compiler.ParserGoldenTest (parserGoldenTestsAST) where

import Relude

import Prettyprinter
import Prettyprinter.Render.Text
import System.FilePath
import Test.Tasty
import Test.Tasty.Golden

import Lamagraph.Compiler.Parser
import Lamagraph.Compiler.PrettyAST ()

render :: Doc ann -> LText
render = renderLazy . layoutPretty defaultLayoutOptions

parserGoldenTestsAST :: IO TestTree
parserGoldenTestsAST = do
  lmlFiles <- findByExtension [".lml"] "test/parserGolden/source"
  return $
    testGroup
      "Golden tests AST"
      [ goldenVsString (takeBaseName lmlFile) resLmlFile (helper lmlFile)
      | lmlFile <- lmlFiles
      , let resLmlFile = addExtension (changePath lmlFile) "ast"
      ]
 where
  helper :: FilePath -> IO LByteString
  helper lmlFile = do
    fileBS <- readFileBS lmlFile
    let fileT = decodeUtf8 fileBS
        parseResult = parseLamagraphML fileT
    pure $ case parseResult of
      Left err -> encodeUtf8 err
      Right tree -> encodeUtf8 $ (render . pretty) tree
  changePath :: FilePath -> FilePath
  changePath filePath = astDir </> fileName
   where
    parserGoldenDir = takeDirectory $ takeDirectory filePath
    astDir = parserGoldenDir </> "ast"
    fileName = takeFileName filePath
