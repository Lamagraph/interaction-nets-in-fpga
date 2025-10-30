module Lamagraph.Compiler.Module.Eval.EvalModuleGolden (evalModuleGolden) where

import Relude

import Data.Either.Extra (mapLeft)
import System.Directory
import System.FilePath
import Test.Tasty
import Test.Tasty.Golden
import UnliftIO.Exception

import Lamagraph.Compiler.Core.MonadDesugar
import Lamagraph.Compiler.Core.Pretty ()
import Lamagraph.Compiler.Eval
import Lamagraph.Compiler.GoldenCommon
import Lamagraph.Compiler.ModuleResolver
import Lamagraph.Compiler.ModuleResolver.Resolve.Program
import Lamagraph.Compiler.PrettyAst ()

moduleSourceDir :: FilePath
moduleSourceDir = evalSourceGoldenModuleTestsDir

moduleOutputDir :: FilePath
moduleOutputDir = moduleSourceDir </> ".." </> "output"

evalModuleGolden :: IO TestTree
evalModuleGolden = do
  subdirs <- listDirectory moduleSourceDir
  validDirs <- filterM (\d -> doesDirectoryExist (moduleSourceDir </> d)) subdirs
  testGroups <- forM validDirs $ \dir -> do
    let fullDir = moduleSourceDir </> dir
    lmlFiles <- sort <$> findByExtension [lmlExt] fullDir
    let outFile = moduleOutputDir </> dir <.> "out"
    let goldenTests =
          [ goldenVsString
              dir
              outFile
              (helper' lmlFiles)
          ]
    return $ testGroup dir goldenTests
  return $ testGroup "Eval Module Golden tests" testGroups

helper' :: [FilePath] -> IO LByteString
helper' lmlFiles = do
  ref <- newIORef ""
  encodeUtf8 <$> runReaderT (helper lmlFiles) ref

helper :: [FilePath] -> EvalMock Text
helper lmlFiles = do
  fileLBS <- mapM readFileLBS lmlFiles
  let contents = map decodeUtf8 fileLBS
  parsedProgram <- fromEither $ mapLeft stringException $ parseLmlProgram contents
  resolvedProgram <- fromEither $ resolveDef parsedProgram
  typedProgram <- fromEither $ typecheckLmlProgram resolvedProgram
  let binds = runMonadDesugar $ desugarLmlProgram typedProgram
  _ <- evalLmlProgramDefEnv binds
  ref <- ask
  readIORef ref
