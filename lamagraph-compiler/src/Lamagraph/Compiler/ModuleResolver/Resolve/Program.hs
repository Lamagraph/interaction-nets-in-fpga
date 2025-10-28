module Lamagraph.Compiler.ModuleResolver.Resolve.Program where

import Relude

import Control.Lens
import Control.Monad.Except
import Data.HashMap.Strict qualified as HashMap
import Data.HashSet qualified as HashSet

import Lamagraph.Compiler.Extension
import Lamagraph.Compiler.ModuleResolver.DefaultEnv
import Lamagraph.Compiler.ModuleResolver.Helper
import Lamagraph.Compiler.ModuleResolver.MrTypes
import Lamagraph.Compiler.ModuleResolver.Program
import Lamagraph.Compiler.ModuleResolver.Resolve.Decl
import Lamagraph.Compiler.ModuleResolver.Resolve.Expr
import Lamagraph.Compiler.ModuleResolver.Resolve.Lit
import Lamagraph.Compiler.ModuleResolver.Resolve.Module
import Lamagraph.Compiler.ModuleResolver.Resolve.Pat
import Lamagraph.Compiler.ModuleResolver.Resolve.Type
import Lamagraph.Compiler.Parser
import Lamagraph.Compiler.Parser.SrcLoc
import Lamagraph.Compiler.Syntax
import Lamagraph.Compiler.Syntax.Expr
import Lamagraph.Compiler.Syntax.Extension
import Lamagraph.Compiler.Syntax.Pat

resolveLmlProgram :: ModuleEnv -> LmlProgram LmlcPs -> MonadModuleResolver (LmlProgram LmlcMr)
resolveLmlProgram env (LmlProgram program) = do
  (_, programResolved) <- resolveMany env resolveLmlModule program
  pure (LmlProgram programResolved)

resolve :: ModuleEnv -> LmlProgram LmlcPs -> Either ModuleResolverError (LmlProgram LmlcMr)
resolve env = runMonadModuleResolver . resolveLmlProgram env

resolveDef :: LmlProgram LmlcPs -> Either ModuleResolverError (LmlProgram LmlcMr)
resolveDef = runMonadModuleResolver . resolveLmlProgram defaultModuleEnv
