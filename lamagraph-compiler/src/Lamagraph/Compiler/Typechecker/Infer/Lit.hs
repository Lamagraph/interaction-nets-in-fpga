module Lamagraph.Compiler.Typechecker.Infer.Lit (inferLmlLit) where

import Relude

import Lamagraph.Compiler.Extension
import Lamagraph.Compiler.Syntax
import Lamagraph.Compiler.Typechecker.DefaultEnv
import Lamagraph.Compiler.Typechecker.Types

inferLmlLit :: LmlLit LmlcPs -> MonadTypecheck Ty
inferLmlLit = \case
  LmlInt _ _ -> pure tyInt
  LmlChar _ _ -> pure tyChar
  LmlString _ _ -> pure tyString
