module Lamagraph.Compiler.Typechecker.Infer (inferDef, inferLmlModule) where

import Relude

import Data.HashMap.Strict qualified as HashMap

import Lamagraph.Compiler.Extension
import Lamagraph.Compiler.Syntax
import Lamagraph.Compiler.Typechecker.DefaultEnv
import Lamagraph.Compiler.Typechecker.Infer.Decl
import Lamagraph.Compiler.Typechecker.TcTypes

inferLmlModule :: TyEnv -> TyConstrEnv -> LmlModule LmlcMr -> MonadTypecheck (LmlModule LmlcTc)
inferLmlModule tyEnv tyConstrEnv (LmlModule _ lLongident decls) = do
  (outEnv, _, declsTyped) <- inferSeq lLongident tyEnv tyConstrEnv decls
  pure $ LmlModule outEnv lLongident declsTyped
 where
  inferSeq moduleName env@(TyEnv tyEnvInner) tyConstrEnvCurrent = \case
    [] -> pure (TyEnv HashMap.empty, tyConstrEnvCurrent, [])
    (x : xs) -> do
      (TyEnv tyEnv', tyConstrEnv', lDeclTyped) <- inferLLmlDecl moduleName env tyConstrEnvCurrent x
      let newTyEnv = TyEnv $ tyEnv' `HashMap.union` tyEnvInner
          combinedTyConstrEnv = TyConstrEnv $ coerce tyConstrEnv' `HashMap.union` coerce tyConstrEnvCurrent
      (TyEnv tyEnv'', tyConstrEnv'', lDeclsTyped) <- inferSeq moduleName newTyEnv combinedTyConstrEnv xs
      pure (TyEnv (tyEnv'' `HashMap.union` tyEnv'), tyConstrEnv'', lDeclTyped : lDeclsTyped)

infer :: TyEnv -> TyConstrEnv -> LmlModule LmlcMr -> Either TypecheckError (LmlModule LmlcTc)
infer env tyConstrEnv = runMonadTypecheck . inferLmlModule env tyConstrEnv

inferDef :: LmlModule LmlcMr -> Either TypecheckError (LmlModule LmlcTc)
inferDef = infer defaultEnv defaultTyConstrEnv
