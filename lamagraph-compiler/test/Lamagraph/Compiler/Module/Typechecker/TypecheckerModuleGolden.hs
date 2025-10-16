{-# LANGUAGE BlockArguments #-}

module Lamagraph.Compiler.Module.Typechecker.TypecheckerModuleGolden (typecheckerPrettyModuleGolden) where

import Relude

import Prettyprinter
import System.Directory (doesDirectoryExist, listDirectory)
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
    return $
      goldenVsString dir (moduleAstDir </> dir <.> "ast") (helper lmlFiles)
  return $ testGroup "Pretty Typed Module Golden tests" testGroups
 where
  helper :: [FilePath] -> IO LByteString
  helper files = do
    rawContents <- traverse readFileBS files
    let contents = map decodeUtf8 rawContents
        asts = map parseLamagraphML contents
    pure $ case find isLeft asts of
      Just (Left err) -> encodeUtf8 err
      _ ->
        encodeUtf8 $ (renderPretty . pretty . resolveModules) (rights asts)
