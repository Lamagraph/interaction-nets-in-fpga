module Lamagraph.Compiler.Core.LambdaLiftingGolden (coreLamLiftGolden) where

import Relude

import Prettyprinter
import System.FilePath
import Test.Tasty
import Test.Tasty.Golden

import Lamagraph.Compiler.Core.LmlToCore
import Lamagraph.Compiler.Core.MonadDesugar

import Lamagraph.Compiler.Core.Pretty ()

import Control.Monad.Extra
import Lamagraph.Compiler.GoldenCommon
import Lamagraph.Compiler.Parser
import Lamagraph.Compiler.Typechecker.Infer

import Lamagraph.Compiler.Core (CoreBind)
import Lamagraph.Compiler.Core.LambdaLifting
import Lamagraph.Compiler.Extension
import Lamagraph.Compiler.Syntax

newExt :: String
newExt = "core"

newDir :: FilePath
newDir = ".." </> "lamLift"

coreLamLiftGolden :: IO TestTree
coreLamLiftGolden = do
  lmlFiles <- findByExtension [lmlExt] coreSourceGoldenTestsDir
  return $
    testGroup
      "Core LamLifting Golden tests"
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
        Right core ->
          let binds = applyLL core
           in case runMonadDesugar binds of
                Left _ -> "FIXME: Either add constructors to DesugarError, or get rid of ExceptT"
                Right pureBinds -> encodeUtf8 $ (renderPretty . pretty) pureBinds
  applyLL :: LmlModule LmlcTc -> MonadDesugar [CoreBind]
  applyLL x = do
    binds <- desugarLmlModule x
    concatMapM lambdaLiftingBind binds
