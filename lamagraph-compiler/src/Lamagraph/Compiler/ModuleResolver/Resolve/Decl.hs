module Lamagraph.Compiler.ModuleResolver.Resolve.Decl (resolveLLmlDecl) where

import Relude

import Control.Lens
import Control.Monad.Except
import Data.HashMap.Strict qualified as HashMap
import Data.HashSet qualified as HashSet

import Lamagraph.Compiler.Extension
import Lamagraph.Compiler.ModuleResolver.Resolve.Expr
import Lamagraph.Compiler.ModuleResolver.Types
import Lamagraph.Compiler.Parser.SrcLoc
import Lamagraph.Compiler.Syntax

resolveLLmlDecl :: ModuleEnv -> LLmlDecl LmlcPs -> MonadModuleResolver (ModuleEnv, LLmlDecl LmlcMr)
resolveLLmlDecl env (L loc decl) = over _2 (L loc) <$> resolveLmlDecl env decl

resolveLmlDecl :: ModuleEnv -> LmlDecl LmlcPs -> MonadModuleResolver (ModuleEnv, LmlDecl LmlcMr)
resolveLmlDecl env = \case
  ValD _ lBindGroup -> do
    (bgEnv, lBindGroupResolved) <- resolveLLmlBindGroup env lBindGroup
    pure (bgEnv & over currentNames (HashSet.union (bgEnv ^. localNames)), ValD noExtField lBindGroupResolved)
  OpenD _ (OpenDecl _ lIdent@(L _ ident)) -> do
    let (ModuleRegistry compiledModules) = env ^. moduleRegistry
    case HashMap.lookup (ModulePath ident) compiledModules of
      Nothing -> throwError (ModuleNotFound ident)
      Just names -> do
        let localShadowed = HashSet.difference (env ^. localNames) names
        let newOpens = ModulePath ident : (env ^. opens)
        pure (env & set opens newOpens & set localNames localShadowed, OpenD noExtField (OpenDecl noExtField lIdent))
  TyD _ _ -> error "Failed to resolve type declaration: unsupported!"
