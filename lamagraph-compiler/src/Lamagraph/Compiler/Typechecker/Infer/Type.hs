module Lamagraph.Compiler.Typechecker.Infer.Type (lLmlTypeToTy) where

import Relude

import Lamagraph.Compiler.Extension
import Lamagraph.Compiler.Parser.SrcLoc
import Lamagraph.Compiler.Syntax
import Lamagraph.Compiler.Typechecker.Helper
import Lamagraph.Compiler.Typechecker.TcTypes

lLmlTypeToTy :: LLmlType LmlcPs -> MonadTypecheck Ty
lLmlTypeToTy (L _ lmlType) = lmlTypeToTy lmlType

lmlTypeToTy :: LmlType LmlcPs -> MonadTypecheck Ty
lmlTypeToTy = \case
  LmlTyVar _ _ -> freshTVar
  LmlTyArrow _ lTy1 lTy2 -> TArrow <$> lLmlTypeToTy lTy1 <*> lLmlTypeToTy lTy2
  LmlTyTuple _ lTy lTys -> TTuple <$> lLmlTypeToTy lTy <*> mapM lLmlTypeToTy lTys
  LmlTyConstr _ (L _ constrName) lTys -> TConstr (Name constrName) <$> mapM lLmlTypeToTy lTys
