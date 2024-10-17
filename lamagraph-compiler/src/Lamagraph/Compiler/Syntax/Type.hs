{-# LANGUAGE UndecidableInstances #-}

-- | LamagraphML type related types
module Lamagraph.Compiler.Syntax.Type (LLmlType, LmlType (..), ForallLmlType) where

import Relude

import Lamagraph.Compiler.Syntax.Extension
import Lamagraph.Compiler.Syntax.Longident

-- | Located 'LmlType'.
type LLmlType pass = XLocated pass (LmlType pass)

-- | LamagraphML type representation.
data LmlType pass
  = -- TODO: Maybe we'll need something like RdrName instead of (XLocated pass Text)

    -- | Type variables like @'a@.
    LmlTyVar (XLmlTyVar pass) (XLocated pass Text)
  | -- | Type arrow like @'a -> 'a@.
    LmlTyArrow (XLmlTyArrow pass) (LLmlType pass) (LLmlType pass)
  | -- | Tuple on type level.
    --
    -- /Invariant/: \(n \geq 2\)
    LmlTyTuple (XLmlTyTuple pass) (LLmlType pass) (NonEmpty (LLmlType pass))
  | -- | Type constructor application.
    -- @"'LmlTyConstr' _ lindent types"@  represents
    --
    -- - @/typeconstr/@ when @types = []@
    -- - @/typexpr/ /typeconstr/@ when @types = [type]@
    -- - @( /typexpr/ { , /typexpr/ } ) /typeconstr/@ when @types = [type1, ..., typen]@
    LmlTyConstr (XLmlTyConstr pass) (LLongident pass) [LLmlType pass]
  | XLmlType !(XXType pass)

type ForallLmlType (tc :: Type -> Constraint) pass =
  ( tc (XLmlTyVar pass)
  , tc (XLocated pass Text)
  , tc (XLmlTyArrow pass)
  , tc (LLmlType pass)
  , tc (XLmlTyTuple pass)
  , tc (XLmlTyConstr pass)
  , tc (LLongident pass)
  , tc (XXType pass)
  )

deriving instance (ForallLmlType Show pass) => Show (LmlType pass)
deriving instance (ForallLmlType Eq pass) => Eq (LmlType pass)
