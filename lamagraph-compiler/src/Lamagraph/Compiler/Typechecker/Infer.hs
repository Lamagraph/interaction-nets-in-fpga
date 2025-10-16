module Lamagraph.Compiler.Typechecker.Infer (inferDef, inferLmlModule) where

import Relude

import Data.HashMap.Strict qualified as HashMap

import Lamagraph.Compiler.Extension
import Lamagraph.Compiler.Syntax
import Lamagraph.Compiler.Typechecker.DefaultEnv
import Lamagraph.Compiler.Typechecker.Infer.Decl
import Lamagraph.Compiler.Typechecker.TcTypes

inferLmlModule :: TyEnv -> LmlModule LmlcPs -> MonadTypecheck (LmlModule LmlcTc)
inferLmlModule tyEnv (LmlModule _ lLongident decls) = do
  (outEnv, declsTyped) <- inferSeq tyEnv decls
  pure $ LmlModule outEnv lLongident declsTyped
 where
  inferSeq env@(TyEnv tyEnvInner) = \case
    [] -> pure (TyEnv HashMap.empty, [])
    (x : xs) -> do
      (TyEnv tyEnv', lDeclTyped) <- inferLLmlDecl env x
      (TyEnv tyEnv'', lDeclsTyped) <- inferSeq (TyEnv $ tyEnv' `HashMap.union` tyEnvInner) xs
      pure (TyEnv (tyEnv'' `HashMap.union` tyEnv'), lDeclTyped : lDeclsTyped)

infer :: TyEnv -> LmlModule LmlcPs -> Either TypecheckError (LmlModule LmlcTc)
infer env = runMonadTypecheck . inferLmlModule env

inferDef :: LmlModule LmlcPs -> Either TypecheckError (LmlModule LmlcTc)
inferDef = infer defaultEnv
