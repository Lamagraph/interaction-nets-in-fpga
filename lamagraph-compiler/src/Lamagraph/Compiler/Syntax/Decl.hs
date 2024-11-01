{-# LANGUAGE UndecidableInstances #-}

-- | LamagraphML declarations
module Lamagraph.Compiler.Syntax.Decl (
  LLmlDecl,
  LmlDecl (..),
  ForallLmlDecl,
  LOpenDecl,
  OpenDecl (..),
  ForallOpenDecl,
  LTyDecl,
  TyDecl (..),
  ForallTyDecl,
  LConDecl,
  ConDecl (..),
  ForallConDecl,
) where

import Relude

import Lamagraph.Compiler.Syntax.Expr
import Lamagraph.Compiler.Syntax.Extension
import Lamagraph.Compiler.Syntax.Longident
import Lamagraph.Compiler.Syntax.Type

-- | Located 'LmlDecl'
type LLmlDecl pass = XLocated pass (LmlDecl pass)

-- | A LamagraphML declaration
data LmlDecl pass
  = OpenD (XOpenD pass) (OpenDecl pass)
  | ValD (XValD pass) (LLmlBindGroup pass)
  | TyD (XTyD pass) (NonEmpty (LTyDecl pass))
  | XLmlDecl !(XXDecl pass)

type ForallLmlDecl (tc :: Type -> Constraint) pass =
  ( tc (XOpenD pass)
  , tc (OpenDecl pass)
  , tc (XValD pass)
  , tc (LLmlBindGroup pass)
  , tc (XTyD pass)
  , tc (LTyDecl pass)
  , tc (XXDecl pass)
  )

deriving instance (ForallLmlDecl Show pass) => Show (LmlDecl pass)
deriving instance (ForallLmlDecl Eq pass) => Eq (LmlDecl pass)

-- | Located open declaration
type LOpenDecl pass = XLocated pass (OpenDecl pass)

{- | Open declaration

Too small to have separate module.
-}
data OpenDecl pass
  = OpenDecl (XOpenDecl pass) (LLongident pass)
  | XOpenDecl !(XXOpenDecl pass)

type ForallOpenDecl (tc :: Type -> Constraint) pass =
  (tc (XOpenDecl pass), tc (XXOpenDecl pass), tc (LLongident pass))

deriving instance (ForallOpenDecl Show pass) => Show (OpenDecl pass)
deriving instance (ForallOpenDecl Eq pass) => Eq (OpenDecl pass)

-- | Located type declaration
type LTyDecl pass = XLocated pass (TyDecl pass)

-- | Type declaration
data TyDecl pass
  = -- | Type alias from @type name = typexpr@
    AliasDecl (XAliasDecl pass) (XLocated pass Text) [LLmlType pass] (LLmlType pass)
  | -- | ADT declaration from @type 'a name = C1 of 'a * 'a type | ...@.
    DataDecl (XDataDecl pass) (XLocated pass Text) [LLmlType pass] [LConDecl pass]
  | XTyDecl !(XXTyDecl pass)

type ForallTyDecl (tc :: Type -> Constraint) pass =
  ( tc (XAliasDecl pass)
  , tc (XLocated pass Text)
  , tc (LLmlType pass)
  , tc (XDataDecl pass)
  , tc (LConDecl pass)
  , tc (XXTyDecl pass)
  )

deriving instance (ForallTyDecl Show pass) => Show (TyDecl pass)
deriving instance (ForallTyDecl Eq pass) => Eq (TyDecl pass)

-- | Located ADT constructor declaration
type LConDecl pass = XLocated pass (ConDecl pass)

-- | ADT constructor declaration
data ConDecl pass
  = ConDecl
      { _cdExt :: XConDecl pass
      -- ^ ConDecl extension point
      , _cdConName :: XLocated pass Text
      -- ^ Constructor name
      , _cdArgs :: [LLmlType pass]
      -- ^ Constructor type arguments
      }
  | XConDecl !(XXConDecl pass)

type ForallConDecl (tc :: Type -> Constraint) pass =
  (tc (XConDecl pass), tc (XLocated pass Text), tc (LLmlType pass), tc (XXConDecl pass))

deriving instance (ForallConDecl Show pass) => Show (ConDecl pass)
deriving instance (ForallConDecl Eq pass) => Eq (ConDecl pass)
