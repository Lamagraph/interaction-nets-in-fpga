module Lamagraph.Compiler.Typechecker.Infer (inferDef) where

import Relude

import Data.HashMap.Strict qualified as HashMap

import Lamagraph.Compiler.Extension
import Lamagraph.Compiler.Syntax
import Lamagraph.Compiler.Typechecker.DefaultEnv
import Lamagraph.Compiler.Typechecker.Infer.Decl
import Lamagraph.Compiler.Typechecker.TcTypes

inferSeq :: (TyEnv -> a -> MonadTypecheck TyEnv) -> TyEnv -> [a] -> MonadTypecheck TyEnv
inferSeq inferFunc (TyEnv tyEnv) = \case
  [] -> pure $ TyEnv HashMap.empty
  (x : xs) -> do
    TyEnv tyEnv' <- inferFunc (TyEnv tyEnv) x
    TyEnv tyEnv'' <- inferSeq inferFunc (TyEnv $ tyEnv' `HashMap.union` tyEnv) xs
    pure $ TyEnv (tyEnv'' `HashMap.union` tyEnv')

inferLmlModule :: TyEnv -> LmlModule LmlcPs -> MonadTypecheck TyEnv
inferLmlModule tyEnv (LmlModule _ _ decls) = inferSeq inferLLmlDecl tyEnv decls

infer :: TyEnv -> LmlModule LmlcPs -> Either TypecheckError TyEnv
infer env = runMonadTypecheck . inferLmlModule env

inferDef :: LmlModule LmlcPs -> Either TypecheckError TyEnv
inferDef = infer defaultEnv
