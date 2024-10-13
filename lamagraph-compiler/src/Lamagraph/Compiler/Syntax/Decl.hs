{-# LANGUAGE UndecidableInstances #-}

-- TODO: Exports
module Lamagraph.Compiler.Syntax.Decl where

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
  | ValD (XValD pass) RecFlag (NonEmpty (LLmlBind pass))
  | TyD (XTyD pass) (NonEmpty (LTyDecl pass))
  | XLmlDecl !(XXDecl pass)

type ForallLmlDecl (f :: Type -> Constraint) pass =
  ( f (XOpenD pass)
  , f (OpenDecl pass)
  , f (XValD pass)
  , f (LLmlBind pass)
  , f (XTyD pass)
  , f (LTyDecl pass)
  , f (XXDecl pass)
  )

deriving instance (ForallLmlDecl Show p) => Show (LmlDecl p)

-- | Located open declaration
type LOpenDecl pass = XLocated pass (OpenDecl pass)

{- | Open declaration

Too small to have separate module.
-}
data OpenDecl pass
  = OpenDecl (XOpenDecl pass) (LLongident pass)
  | XOpenDecl !(XXOpenDecl pass)

type ForallOpenDecl (f :: Type -> Constraint) pass =
  (f (XOpenDecl pass), f (XXOpenDecl pass), f (LLongident pass))

deriving instance (ForallOpenDecl Show p) => Show (OpenDecl p)

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
