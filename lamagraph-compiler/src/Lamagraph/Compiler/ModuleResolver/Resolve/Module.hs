module Lamagraph.Compiler.ModuleResolver.Resolve.Module where

import Relude

import Control.Lens
import Control.Monad.Except
import Data.HashMap.Strict qualified as HashMap
import Data.HashSet qualified as HashSet

import Lamagraph.Compiler.Extension
import Lamagraph.Compiler.ModuleResolver.DefaultEnv
import Lamagraph.Compiler.ModuleResolver.Helper
import Lamagraph.Compiler.ModuleResolver.MrTypes
import Lamagraph.Compiler.ModuleResolver.Resolve.Decl
import Lamagraph.Compiler.ModuleResolver.Resolve.Expr
import Lamagraph.Compiler.ModuleResolver.Resolve.Lit
import Lamagraph.Compiler.ModuleResolver.Resolve.Pat
import Lamagraph.Compiler.ModuleResolver.Resolve.Type
import Lamagraph.Compiler.Parser
import Lamagraph.Compiler.Parser.SrcLoc
import Lamagraph.Compiler.Syntax
import Lamagraph.Compiler.Syntax.Expr
import Lamagraph.Compiler.Syntax.Extension
import Lamagraph.Compiler.Syntax.Pat

resolveLmlModule :: ModuleEnv -> LmlModule LmlcPs -> MonadModuleResolver (ModuleEnv, LmlModule LmlcMr)
resolveLmlModule env (LmlModule _ name@(Just (L _ longident)) decls) = do
  let curEnv =
        env
          & set currentModule (ModulePath longident)
          & set currentNames HashSet.empty
          & set localNames HashSet.empty
          & set opens [ModulePath (mkLongident (stdPrefix :| []))]
  (declsEnv, declsResolved) <- resolveMany curEnv resolveLLmlDecl decls
  let finalEnv =
        declsEnv
          & over
            moduleRegistry
            (\(ModuleRegistry reg) -> ModuleRegistry $ HashMap.insert (declsEnv ^. currentModule) (declsEnv ^. currentNames) reg)
  pure (finalEnv, LmlModule noExtField name declsResolved)
resolveLmlModule _ (LmlModule _ Nothing _) = error "Please, name your modules"

resolveModuleDefEnv :: LmlModule LmlcPs -> Either ModuleResolverError (LmlModule LmlcMr)
resolveModuleDefEnv lmod = lmod & resolveLmlModule defaultModuleEnv <&> snd & runMonadModuleResolver
