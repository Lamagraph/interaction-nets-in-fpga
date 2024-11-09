module Lamagraph.Compiler.Typechecker.Infer.Decl where

import Lamagraph.Compiler.Extension
import Lamagraph.Compiler.Parser.SrcLoc
import Lamagraph.Compiler.Syntax
import Lamagraph.Compiler.Typechecker.Infer.Expr
import Lamagraph.Compiler.Typechecker.Types

inferLLmlDecl :: TyEnv -> LLmlDecl LmlcPs -> MonadTypecheck TyEnv
inferLLmlDecl tyEnv (L _ decl) = inferLmlDecl tyEnv decl

inferLmlDecl :: TyEnv -> LmlDecl LmlcPs -> MonadTypecheck TyEnv
inferLmlDecl tyEnv = \case
  ValD _ lBindGroup -> inferLLmlBindGroup tyEnv lBindGroup
