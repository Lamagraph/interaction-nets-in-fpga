module Lamagraph.Compiler.Core.PrettyCoreGolden (corePrettyGolden) where

import Relude

import Prettyprinter
import System.FilePath
import Test.Tasty
import Test.Tasty.Golden

import Lamagraph.Compiler.Core.LmlToCore
import Lamagraph.Compiler.Core.MonadDesugar
import Lamagraph.Compiler.Core.Pretty ()
import Lamagraph.Compiler.GoldenCommon
import Lamagraph.Compiler.Parser
import Lamagraph.Compiler.Typechecker.Infer

newExt :: String
newExt = "core"

newDir :: FilePath
newDir = ".." </> "core"

corePrettyGolden :: IO TestTree
corePrettyGolden = do
  lmlFiles <- findByExtension [lmlExt] coreSourceGoldenTestsDir
  return $
    testGroup
      "Core Golden tests"
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
      Right tree -> case inferDef tree of
        Left err -> show err
        Right core -> case (runMonadDesugar . desugarLmlModule) core of
          Left _ -> "FIXME: Either add constructors to DesugarError, or get rid of ExceptT"
          Right t -> encodeUtf8 $ (renderPretty . pretty) t
