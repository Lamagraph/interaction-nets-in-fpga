module Lamagraph.Compiler.ModuleResolver.Resolve.Program (resolveDef) where

import Relude

import Lamagraph.Compiler.Extension
import Lamagraph.Compiler.ModuleResolver.DefaultEnv
import Lamagraph.Compiler.ModuleResolver.Helper
import Lamagraph.Compiler.ModuleResolver.Resolve.Module
import Lamagraph.Compiler.ModuleResolver.Types
import Lamagraph.Compiler.Syntax

resolveLmlProgram :: ModuleEnv -> LmlProgram LmlcPs -> MonadModuleResolver (LmlProgram LmlcMr)
resolveLmlProgram env (LmlProgram program) = do
  (_, programResolved) <- resolveMany env resolveLmlModule program
  pure (LmlProgram programResolved)

resolve :: ModuleEnv -> LmlProgram LmlcPs -> Either ModuleResolverError (LmlProgram LmlcMr)
resolve env = runMonadModuleResolver . resolveLmlProgram env

resolveDef :: LmlProgram LmlcPs -> Either ModuleResolverError (LmlProgram LmlcMr)
resolveDef = resolve defaultModuleEnv
