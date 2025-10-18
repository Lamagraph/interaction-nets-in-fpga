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
    xs <- processFiles astSubdir lmlFiles
    let goldenTests =
          [ goldenVsString
            (takeFileName file)
            file
            (pure result)
          | (file, result) <- xs
          ]
    return $ testGroup dir goldenTests
  return $ testGroup "Pretty Typed Module Golden tests" testGroups

processFiles :: FilePath -> [FilePath] -> IO [(FilePath, LByteString)]
processFiles astSubdir files = do
  raw <- mapM readFileBS files
  let new_file_paths = map (\file -> astSubdir </> takeBaseName file <.> "ast") files
  let err_path = astSubdir </> "err.out"
  let content = map decodeUtf8 raw
  let parsed = parseLmlProgram content
  pure $
    case parsed of
      Left err -> [(err_path, encodeUtf8 err)]
      Right parsedProgram ->
        case typecheckLmlProgram parsedProgram of
          Left err -> [(err_path, encodeUtf8 (renderPretty $ pretty err))]
          Right (LmlProgram xs) -> zip new_file_paths (map (encodeUtf8 . renderPretty . pretty) xs)
