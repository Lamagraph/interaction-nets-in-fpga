module Lamagraph.Compiler.Core.LlvmCoreGolden (llvmGolden) where

import Relude hiding (head)
import Relude.Unsafe (head)

import System.FilePath
import Test.Tasty
import Test.Tasty.Golden

import Lamagraph.Compiler.Core.LmlToCore
import Lamagraph.Compiler.Core.MonadDesugar

import Lamagraph.Compiler.GoldenCommon
import Lamagraph.Compiler.Parser
import Lamagraph.Compiler.Typechecker.Infer

import Data.HashMap.Strict qualified as HashMap
import Lamagraph.Compiler.Core (CoreBind)
import Lamagraph.Compiler.Core.Anf (ABind, bindsLlAnf)
import Lamagraph.Compiler.Core.AnfToLllvm
import Lamagraph.Compiler.Extension
import Lamagraph.Compiler.Syntax
import Text.LLVM
import Text.LLVM.PP

newExt :: String
newExt = "ll"

newDir :: FilePath
newDir = ".." </> "llvm"

llvmGolden :: IO TestTree
llvmGolden = do
  lmlFiles <- findByExtension [lmlExt] coreSourceGoldenTestsDir
  return $
    testGroup
      "Llvm Golden tests"
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
          let binds = applyAnf core
           in case runMonadDesugar binds of
                Left _ -> "FIXME: Either add constructors to DesugarError, or get rid of ExceptT"
                Right pureBinds ->
                  let moduleL = snd $ runLLVM (aBindToLlvm HashMap.empty (head pureBinds))
                   in show $ withConfig (Config{cfgVer = 17}) ppModule moduleL
  applyAnf :: LmlModule LmlcTc -> MonadDesugar [ABind]
  applyAnf x = do
    binds <- desugarLmlModule x
    bindsLlAnf binds
