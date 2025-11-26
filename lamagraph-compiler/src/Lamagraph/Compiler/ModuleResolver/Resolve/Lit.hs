module Lamagraph.Compiler.ModuleResolver.Resolve.Lit (resolveLmlLit) where

import Lamagraph.Compiler.Extension
import Lamagraph.Compiler.Syntax

resolveLmlLit :: LmlLit LmlcPs -> LmlLit LmlcMr
resolveLmlLit = \case
  LmlInt _ val -> LmlInt noExtField val
  LmlChar _ val -> LmlChar noExtField val
  LmlString _ val -> LmlString noExtField val
