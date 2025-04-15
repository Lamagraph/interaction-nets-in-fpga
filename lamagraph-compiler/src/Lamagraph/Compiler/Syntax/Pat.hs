{-# LANGUAGE UndecidableInstances #-}

-- | LamagraphML patterns
module Lamagraph.Compiler.Syntax.Pat (LLmlPat, LmlPat (..), ForallLmlPat) where

import Relude

import Lamagraph.Compiler.Syntax.Extension
import Lamagraph.Compiler.Syntax.Lit
import Lamagraph.Compiler.Syntax.Longident
import Lamagraph.Compiler.Syntax.Type

-- | Located 'LmlPat'
type LLmlPat pass = XLocated pass (LmlPat pass)

-- | LamagraphML pattern
data LmlPat pass
  = -- | Pattern wildcard @_@
    LmlPatAny (XLmlPatAny pass)
  | -- | Represents pattern variable as @x@
    LmlPatVar (XLmlPatVar pass) (XLocated pass Text)
  | -- | Constant pattern
    LmlPatConstant (XLmlPatConstant pass) (LmlLit pass)
  | -- | Tuple pattern, invariant \(n \geq 2\)
    LmlPatTuple (XLmlPatTuple pass) (LLmlPat pass) (NonEmpty (LLmlPat pass))
  | {- | Constructor application pattern

    Constructors aren't curried, this means that they must be applied to a tuple.
    -}
    LmlPatConstruct (XLmlPatConstruct pass) (LLongident pass) (Maybe (LLmlPat pass))
  | -- | Pattern alternation @pat | pat@
    LmlPatOr (XLmlPatOr pass) (LLmlPat pass) (LLmlPat pass)
  | -- | Pattern constrained with type, e.g. @(pat : typexpr)@
    LmlPatConstraint (XLmlPatConstraint pass) (LLmlPat pass) (LLmlType pass)
  | XLmlPat !(XXPat pass)

type ForallLmlPat (tc :: Type -> Constraint) pass =
  ( tc (XLmlPatAny pass)
  , tc (XLmlPatVar pass)
  , tc (XLocated pass Text)
  , tc (XLmlPatConstant pass)
  , tc (LmlLit pass)
  , tc (XLmlPatTuple pass)
  , tc (LLmlPat pass)
  , tc (XLmlPatConstruct pass)
  , tc (LLongident pass)
  , tc (XLmlPatOr pass)
  , tc (XLmlPatConstraint pass)
  , tc (LLmlType pass)
  , tc (XXPat pass)
  )

deriving instance (ForallLmlPat Show pass) => Show (LmlPat pass)
deriving instance (ForallLmlPat Eq pass) => Eq (LmlPat pass)
