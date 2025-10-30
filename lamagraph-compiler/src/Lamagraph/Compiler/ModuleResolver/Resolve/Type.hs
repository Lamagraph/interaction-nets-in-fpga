module Lamagraph.Compiler.ModuleResolver.Resolve.Type (resolveLLmlType) where

import Relude

import Lamagraph.Compiler.Extension
import Lamagraph.Compiler.ModuleResolver.MrTypes
import Lamagraph.Compiler.Parser.SrcLoc
import Lamagraph.Compiler.Syntax

resolveLLmlType :: ModuleEnv -> LLmlType LmlcPs -> MonadModuleResolver (LLmlType LmlcMr)
resolveLLmlType env (L loc ty) = L loc <$> resolveLmlType env ty

resolveLmlType :: ModuleEnv -> LmlType LmlcPs -> MonadModuleResolver (LmlType LmlcMr)
resolveLmlType env = \case
  LmlTyVar _ ident -> pure $ LmlTyVar noExtField ident
  LmlTyArrow _ lTy1 lTy2 -> do
    lTy1Resolved <- resolveLLmlType env lTy1
    lTy2Resolved <- resolveLLmlType env lTy2
    pure $ LmlTyArrow noExtField lTy1Resolved lTy2Resolved
  LmlTyTuple _ lTy lTys -> do
    lTyResolved <- resolveLLmlType env lTy
    lTysResolved <- mapM (resolveLLmlType env) lTys
    pure $ LmlTyTuple noExtField lTyResolved lTysResolved
  LmlTyConstr _ lConstrName lTys -> do
    lTysResolved <- mapM (resolveLLmlType env) lTys
    pure $ LmlTyConstr noExtField lConstrName lTysResolved
