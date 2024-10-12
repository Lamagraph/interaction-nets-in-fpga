{-# LANGUAGE UndecidableInstances #-}

module Lamagraph.Compiler.Syntax.Decl where

import Relude

import Lamagraph.Compiler.Syntax.Extension
import Lamagraph.Compiler.Syntax.Longident

-- | Located 'LmlDecl'.
type LLmlDecl p = XLocated p (LmlDecl p)

-- | A LamagraphML declaration
data LmlDecl pass = OpenD (XOpenD pass) (OpenDecl pass) | XLmlDecl !(XXLmlDecl pass)

type ForallLmlDecl (f :: Type -> Constraint) p = (f (XOpenD p), f (OpenDecl p), f (XXLmlDecl p))

deriving instance (ForallLmlDecl Show p) => Show (LmlDecl p)

-- | Located Open Declaration
type LOpenDecl pass = XLocated pass (OpenDecl pass)

{- | Open Declaration

N.B. Too small to have separate module
-}
data OpenDecl pass
  = OpenDecl (XOpenDecl pass) (LLongident pass)
  | XOpenDecl !(XXOpenDecl pass)

type ForallOpenDecl (f :: Type -> Constraint) pass =
  (f (XOpenDecl pass), f (XXOpenDecl pass), f (LLongident pass))

deriving instance (ForallOpenDecl Show p) => Show (OpenDecl p)
