module Lamagraph.Compiler.Typechecker.Infer.Decl (inferLLmlDecl) where

import Relude

import Control.Lens

import Lamagraph.Compiler.Extension
import Lamagraph.Compiler.Parser.SrcLoc
import Lamagraph.Compiler.Syntax
import Lamagraph.Compiler.Typechecker.Infer.Expr
import Lamagraph.Compiler.Typechecker.TcTypes

inferLLmlDecl :: TyEnv -> LLmlDecl LmlcPs -> MonadTypecheck (TyEnv, LLmlDecl LmlcTc)
inferLLmlDecl tyEnv (L loc decl) = over _2 (L loc) <$> inferLmlDecl tyEnv decl

inferLmlDecl :: TyEnv -> LmlDecl LmlcPs -> MonadTypecheck (TyEnv, LmlDecl LmlcTc)
inferLmlDecl tyEnv = \case
  ValD _ lBindGroup -> over _2 (ValD noExtField) <$> inferLLmlBindGroup tyEnv lBindGroup
  OpenD _ _ -> error "Failed to typecheck open declaration: unsupported!"
  TyD _ _ -> error "Failed to typecheck type declaration: unsupported!"
