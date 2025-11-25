module Lamagraph.Compiler.Core.PrettyCoreGolden (corePrettyGolden) where

import Relude

import Prettyprinter
import System.FilePath
import Test.Tasty
import Test.Tasty.Golden

import Lamagraph.Compiler.Core.LmlToCore
import Lamagraph.Compiler.Core.MonadDesugar
import Lamagraph.Compiler.Core.Pretty ()
import Lamagraph.Compiler.Driver
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
      Right parsedTree ->
        let resolvedTree = resolveModuleDefEnv parsedTree
         in case inferDef resolvedTree of
              Left err -> show err
              Right core ->
                let binds = (runMonadDesugar . desugarLmlModule) core
                 in encodeUtf8 $ (renderPretty . pretty) binds
