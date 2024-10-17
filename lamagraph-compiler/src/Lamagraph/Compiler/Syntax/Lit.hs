{-# LANGUAGE UndecidableInstances #-}

-- | LamagraphML literals
module Lamagraph.Compiler.Syntax.Lit (LmlLit (..), ForallLmlLit) where

import Relude

import Lamagraph.Compiler.Syntax.Extension

-- | LamagraphML literal
data LmlLit pass
  = LmlInt (XLmlInt pass) Int
  | LmlInt32 (XLmlInt32 pass) Int32
  | LmlUInt32 (XLmlUInt32 pass) Word32
  | LmlInt64 (XLmlInt64 pass) Int64
  | LmlUInt64 (XLmlUInt64 pass) Word64
  | LmlChar (XLmlChar pass) Char
  | LmlString (XLmlString pass) Text
  | XLmlLit !(XXLit pass)

type ForallLmlLit (tc :: Type -> Constraint) pass =
  ( tc (XLmlInt pass)
  , tc (XLmlInt32 pass)
  , tc (XLmlUInt32 pass)
  , tc (XLmlInt64 pass)
  , tc (XLmlUInt64 pass)
  , tc (XLmlChar pass)
  , tc (XLmlString pass)
  , tc (XXLit pass)
  )

deriving instance (ForallLmlLit Show pass) => Show (LmlLit pass)
deriving instance (ForallLmlLit Eq pass) => Eq (LmlLit pass)
