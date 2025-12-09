module Lamagraph.Compiler.Typechecker.Infer.Decl (inferLLmlDecl) where

import Relude

import Control.Lens

import Lamagraph.Compiler.Extension
import Lamagraph.Compiler.Parser.SrcLoc
import Lamagraph.Compiler.Syntax
import Lamagraph.Compiler.Typechecker.Infer.Expr
import Lamagraph.Compiler.Typechecker.Infer.TyDecl
import Lamagraph.Compiler.Typechecker.TcTypes

inferLLmlDecl ::
  Maybe (LLongident LmlcMr) ->
  TyEnv ->
  TyConstrEnv ->
  LLmlDecl LmlcMr ->
  MonadTypecheck (TyEnv, TyConstrEnv, LLmlDecl LmlcTc)
inferLLmlDecl moduleName tyEnv tyConstrEnv (L loc decl) = over _3 (L loc) <$> inferLmlDecl moduleName tyEnv tyConstrEnv decl

inferLmlDecl ::
  Maybe (LLongident LmlcMr) ->
  TyEnv ->
  TyConstrEnv ->
  LmlDecl LmlcMr ->
  MonadTypecheck (TyEnv, TyConstrEnv, LmlDecl LmlcTc)
inferLmlDecl _moduleName tyEnv tyConstrEnv = \case
  ValD _ lBindGroup -> do
    (envOut, bindGroupTyped) <- inferLLmlBindGroup tyEnv tyConstrEnv lBindGroup
    pure (envOut, tyConstrEnv, ValD noExtField bindGroupTyped)
  OpenD _ (OpenDecl _ name) -> pure (tyEnv, tyConstrEnv, OpenD noExtField (OpenDecl noExtField name))
  TyD _ tyDecls -> do
    (combinedEnv, extendedTyConstrEnv, tyDeclsTyped) <- inferTyDecls _moduleName tyEnv tyConstrEnv tyDecls
    pure (combinedEnv, extendedTyConstrEnv, TyD noExtField tyDeclsTyped)
