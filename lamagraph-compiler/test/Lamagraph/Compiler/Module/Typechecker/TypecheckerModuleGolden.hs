{-# LANGUAGE BlockArguments #-}

module Lamagraph.Compiler.Module.Typechecker.TypecheckerModuleGolden (typecheckerPrettyModuleGolden) where

import Relude

import Prettyprinter
import System.Directory (createDirectoryIfMissing, doesDirectoryExist, listDirectory)
import System.FilePath
import Test.Tasty
import Test.Tasty.Golden

import Lamagraph.Compiler.GoldenCommon
import Lamagraph.Compiler.ModuleResolver
import Lamagraph.Compiler.Parser
import Lamagraph.Compiler.PrettyAst ()
import Lamagraph.Compiler.Typechecker.Infer

moduleSourceDir :: FilePath
moduleSourceDir = typecheckerSourceGoldenModuleTestsDir

moduleAstDir :: FilePath
moduleAstDir = moduleSourceDir </> ".." </> "ast"

typecheckerPrettyModuleGolden :: IO TestTree
typecheckerPrettyModuleGolden = do
  subdirs <- listDirectory moduleSourceDir
  validDirs <- filterM (\d -> doesDirectoryExist (moduleSourceDir </> d)) subdirs
  testGroups <- forM validDirs $ \dir -> do
    let fullDir = moduleSourceDir </> dir
    lmlFiles <- sort <$> findByExtension [lmlExt] fullDir
    let astSubdir = moduleAstDir </> dir
    createDirectoryIfMissing True astSubdir
    let goldenTests =
          [ goldenVsString
            (takeFileName file)
            (astSubdir </> (takeBaseName file <.> "ast"))
            (processFile file)
          | file <- lmlFiles
          ]
    return $ testGroup dir goldenTests
  return $ testGroup "Pretty Typed Module Golden tests" testGroups

processFile :: FilePath -> IO LByteString
processFile file = do
  raw <- readFileBS file
  let content = decodeUtf8 raw
  pure $
    case parseLmlProgram [content] of
      Left err -> encodeUtf8 err
      Right parsedProgram ->
        case typecheckLmlProgram parsedProgram of
          Left err -> encodeUtf8 (renderPretty $ pretty err)
          Right (LmlProgram xs) -> encodeUtf8 $ renderPretty (pretty (viaNonEmpty last xs))
