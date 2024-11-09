{-# LANGUAGE UndecidableInstances #-}

-- | LamagraphML literals
module Lamagraph.Compiler.Syntax.Lit (LmlLit (..), ForallLmlLit) where

import Relude

import Lamagraph.Compiler.Syntax.Extension

-- | LamagraphML literal
data LmlLit pass
  = LmlInt (XLmlInt pass) Int
  | LmlChar (XLmlChar pass) Char
  | LmlString (XLmlString pass) Text
  | XLmlLit !(XXLit pass)

type ForallLmlLit (tc :: Type -> Constraint) pass =
  ( tc (XLmlInt pass)
  , tc (XLmlChar pass)
  , tc (XLmlString pass)
  , tc (XXLit pass)
  )

deriving instance (ForallLmlLit Show pass) => Show (LmlLit pass)
deriving instance (ForallLmlLit Eq pass) => Eq (LmlLit pass)
