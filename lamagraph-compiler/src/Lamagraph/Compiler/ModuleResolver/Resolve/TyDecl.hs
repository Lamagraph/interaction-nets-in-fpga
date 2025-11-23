module Lamagraph.Compiler.ModuleResolver.Resolve.TyDecl (
  resolveLTyDecl,
  extractConstructorNames,
  extractTypeConstructorNames,
) where

import Relude

import Data.HashSet qualified as HashSet

import Lamagraph.Compiler.Extension
import Lamagraph.Compiler.ModuleResolver.Resolve.Type
import Lamagraph.Compiler.ModuleResolver.Types
import Lamagraph.Compiler.Parser.SrcLoc
import Lamagraph.Compiler.Syntax

resolveLTyDecl :: ModuleEnv -> LTyDecl LmlcPs -> MonadModuleResolver (LTyDecl LmlcMr)
resolveLTyDecl env (L loc tyDecl) = L loc <$> resolveTyDecl env tyDecl

resolveTyDecl :: ModuleEnv -> TyDecl LmlcPs -> MonadModuleResolver (TyDecl LmlcMr)
resolveTyDecl env (DataDecl _ name params conDecls) = do
  paramsResolved <- traverse (resolveLLmlType env) params
  conDeclsResolved <- traverse (resolveConDecl env) conDecls
  pure $ DataDecl noExtField name paramsResolved conDeclsResolved
resolveTyDecl env (AliasDecl _ name params ty) = do
  paramsResolved <- traverse (resolveLLmlType env) params
  tyResolved <- resolveLLmlType env ty
  pure $ AliasDecl noExtField name paramsResolved tyResolved

resolveConDecl :: ModuleEnv -> LConDecl LmlcPs -> MonadModuleResolver (LConDecl LmlcMr)
resolveConDecl env (L loc (ConDecl _ name args)) = do
  argsResolved <- traverse (resolveLLmlType env) args
  pure $ L loc (ConDecl noExtField name argsResolved)

extractConstructorNames :: NonEmpty (LTyDecl LmlcPs) -> HashSet Text
extractConstructorNames = foldMap extractFromTyDecl
 where
  extractFromTyDecl :: LTyDecl LmlcPs -> HashSet Text
  extractFromTyDecl (L _ (DataDecl _ _ _ conDecls)) =
    HashSet.fromList $ fmap (unLoc . _cdConName . unLoc) (toList conDecls)
  extractFromTyDecl (L _ (AliasDecl{})) = HashSet.empty

extractTypeConstructorNames :: NonEmpty (LTyDecl LmlcPs) -> HashSet Text
extractTypeConstructorNames = foldMap extractTypeName
 where
  extractTypeName :: LTyDecl LmlcPs -> HashSet Text
  extractTypeName (L _ (DataDecl _ (L _ name) _ _)) = HashSet.singleton name
  extractTypeName (L _ (AliasDecl _ (L _ name) _ _)) = HashSet.singleton name
