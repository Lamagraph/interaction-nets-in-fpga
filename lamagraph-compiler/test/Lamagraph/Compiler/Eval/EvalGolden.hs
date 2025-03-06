module Lamagraph.Compiler.Eval.EvalGolden (evalGolden) where

import Relude

import System.FilePath
import Test.Tasty
import Test.Tasty.Golden
import UnliftIO.Exception

import Data.Either.Extra (mapLeft)
import Lamagraph.Compiler.Core.LmlToCore
import Lamagraph.Compiler.Core.MonadDesugar
import Lamagraph.Compiler.Core.Pretty ()
import Lamagraph.Compiler.Eval
import Lamagraph.Compiler.GoldenCommon
import Lamagraph.Compiler.Parser
import Lamagraph.Compiler.Typechecker.Infer

newExt :: String
newExt = "out"

newDir :: FilePath
newDir = ".." </> "output"

evalGolden :: IO TestTree
evalGolden = do
  lmlFiles <- findByExtension [lmlExt] evalSourceGoldenTestsDir
  return $
    testGroup
      "Eval Golden tests"
      [ goldenVsString (takeBaseName lmlFile) resLmlFile (helper' lmlFile)
      | lmlFile <- lmlFiles
      , let resLmlFile = addExtension (changeFileDir lmlFile newDir) newExt
      ]
 where
  helper' :: FilePath -> IO LByteString
  helper' lmlFile = do
    ref <- newIORef ""
    encodeUtf8 <$> runReaderT (helper lmlFile) ref

  helper :: FilePath -> EvalMock Text
  helper lmlFile = do
    fileLBS <- readFileLBS lmlFile
    let fileT = decodeUtf8 fileLBS
    parseTree <- fromEither $ mapLeft stringException $ parseLamagraphML fileT
    typedTree <- fromEither $ inferDef parseTree
    let binds = runMonadDesugar $ desugarLmlModule typedTree
    _ <- evalCoreBindsDefEnv binds
    ref <- ask
    readIORef ref
