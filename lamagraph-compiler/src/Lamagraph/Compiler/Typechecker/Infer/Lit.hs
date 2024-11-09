module Lamagraph.Compiler.Typechecker.Infer.Lit (inferLmlLit) where

import Relude

import Lamagraph.Compiler.Extension
import Lamagraph.Compiler.Syntax
import Lamagraph.Compiler.Typechecker.DefaultEnv
import Lamagraph.Compiler.Typechecker.TcTypes

inferLmlLit :: LmlLit LmlcPs -> MonadTypecheck (Ty, LmlLit LmlcTc)
inferLmlLit = \case
  LmlInt _ val -> pure (tyInt, LmlInt tyInt val)
  LmlChar _ val -> pure (tyChar, LmlChar tyChar val)
  LmlString _ val -> pure (tyString, LmlString tyString val)
