module Lamagraph.Compiler.Typechecker.Infer.Decl (inferLLmlDecl) where

import Relude

import Control.Lens

import Lamagraph.Compiler.Extension
import Lamagraph.Compiler.Parser.SrcLoc
import Lamagraph.Compiler.Syntax
import Lamagraph.Compiler.Typechecker.Infer.Expr
import Lamagraph.Compiler.Typechecker.TcTypes

inferLLmlDecl :: TyEnv -> LLmlDecl LmlcMr -> MonadTypecheck (TyEnv, LLmlDecl LmlcTc)
inferLLmlDecl tyEnv (L loc decl) = over _2 (L loc) <$> inferLmlDecl tyEnv decl

inferLmlDecl :: TyEnv -> LmlDecl LmlcMr -> MonadTypecheck (TyEnv, LmlDecl LmlcTc)
inferLmlDecl tyEnv = \case
  ValD _ lBindGroup -> over _2 (ValD noExtField) <$> inferLLmlBindGroup tyEnv lBindGroup
  OpenD _ (OpenDecl _ name) -> pure (tyEnv, OpenD noExtField (OpenDecl noExtField name))
  TyD _ _ -> error "Failed to typecheck type declaration: unsupported!"
