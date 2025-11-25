module Lamagraph.Compiler.Driver (
  parseLmlProgram,
  typecheckLmlProgram,
  desugarLmlProgram,
  resolveLmlProgram,
  resolveModuleDefEnv,
) where

import Relude

import Lamagraph.Compiler.Core
import Lamagraph.Compiler.Core.LmlToCore
import Lamagraph.Compiler.Core.MonadDesugar
import Lamagraph.Compiler.Extension
import Lamagraph.Compiler.ModuleResolver.Resolve.Module
import Lamagraph.Compiler.ModuleResolver.Resolve.Program
import Lamagraph.Compiler.Parser
import Lamagraph.Compiler.Syntax
import Lamagraph.Compiler.Typechecker.DefaultEnv
import Lamagraph.Compiler.Typechecker.Infer
import Lamagraph.Compiler.Typechecker.TcTypes

-- Only first error
parseLmlProgram :: [Text] -> Either String (LmlProgram LmlcPs)
parseLmlProgram ts = fmap LmlProgram (mapM parseLamagraphML ts)

inferLmlProgram :: TyEnv -> LmlProgram LmlcMr -> MonadTypecheck (LmlProgram LmlcTc)
inferLmlProgram _ (LmlProgram []) = pure $ LmlProgram []
inferLmlProgram env (LmlProgram (x : xs)) = do
  lMod <- inferLmlModule env x
  let (LmlModule outEnv _ _) = lMod
  (LmlProgram xs') <- inferLmlProgram outEnv (LmlProgram xs)
  pure $ LmlProgram $ lMod : xs'

typecheckLmlProgram :: LmlProgram LmlcMr -> Either TypecheckError (LmlProgram LmlcTc)
typecheckLmlProgram = runMonadTypecheck . inferLmlProgram defaultEnv

resolveLmlProgram :: LmlProgram LmlcPs -> LmlProgram LmlcMr
resolveLmlProgram = resolveDef

desugarLmlProgram :: LmlProgram LmlcTc -> MonadDesugar [CoreBind]
desugarLmlProgram (LmlProgram []) = pure []
desugarLmlProgram (LmlProgram (x : xs)) = do
  binds <- desugarLmlModule x
  xs' <- desugarLmlProgram (LmlProgram xs)
  pure (binds <> xs')
