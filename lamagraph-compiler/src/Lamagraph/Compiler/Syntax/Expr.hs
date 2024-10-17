{-# LANGUAGE UndecidableInstances #-}

-- | LamagraphML expressions
module Lamagraph.Compiler.Syntax.Expr (
  RecFlag (..),
  LLmlExpr,
  LmlExpr (..),
  ForallLmlExpr,
  LLmlBind,
  LmlBind (..),
  ForallLmlBind,
  LLmlCase,
  LmlCase (..),
  ForallLmlCase,
) where

import Relude

import Lamagraph.Compiler.Syntax.Extension
import Lamagraph.Compiler.Syntax.Lit
import Lamagraph.Compiler.Syntax.Longident
import Lamagraph.Compiler.Syntax.Pat
import Lamagraph.Compiler.Syntax.Type

-- | Flag for recursive let-bindings
data RecFlag = Recursive | NonRecursive deriving (Show, Eq)

-- | Located 'LmlExpr'
type LLmlExpr pass = XLocated pass (LmlExpr pass)

-- | LamagraphML expression
data LmlExpr pass
  = -- | Represents identifier, e.g. @A.B.C.fun@
    LmlExprIdent (XLmlExprIdent pass) Longident
  | -- | Represents constant value like @-1@ or @"text"@
    LmlExprConstant (XLmlExprConstant pass) (LmlLit pass)
  | -- | Represents let-binding
    LmlExprLet (XLmlExprLet pass) RecFlag (NonEmpty (LLmlBind pass)) (LLmlExpr pass)
  | -- | Represents @fun x -> expr@ construction
    LmlExprFunction (XLmlExprFunction pass) (LLmlPat pass) (LLmlExpr pass)
  | -- | Function application @f x y z@
    LmlExprApply (XLmlExprApply pass) (LLmlExpr pass) (NonEmpty (LLmlExpr pass))
  | -- | Match expression
    --
    -- @
    -- match x with
    -- | pat -> expr
    -- @
    LmlExprMatch (XLmlExprMatch pass) (LLmlExpr pass) (NonEmpty (LLmlCase pass))
  | -- | Tuple representation, invariant \(n \geq 2\)
    LmlExprTuple (XLmlExprTuple pass) (LLmlExpr pass) (NonEmpty (LLmlExpr pass))
  | -- | Constructor application
    --
    -- Constructors aren't curried, this means that they must be applied to a tuple.
    LmlExprConstruct (XLmlExprConstruct pass) (LLongident pass) (Maybe (LLmlExpr pass))
  | -- | Represents @if expr then expr else expr@
    LmlExprIfThenElse (XLmlExprIfThenElse pass) (LLmlExpr pass) (LLmlExpr pass) (LLmlExpr pass)
  | -- | Represents expression constrained by type as in @(expr : typexpr)@
    LmlExprConstraint (XLmlExprConstraint pass) (LLmlExpr pass) (LLmlType pass)
  | XLmlExpr !(XXExpr pass)

type ForallLmlExpr (tc :: Type -> Constraint) pass =
  ( tc (XLmlExprIdent pass)
  , tc (XLmlExprConstant pass)
  , tc (LmlLit pass)
  , tc (XLmlExprLet pass)
  , tc (LLmlBind pass)
  , tc (LLmlExpr pass)
  , tc (XLmlExprFunction pass)
  , tc (LLmlPat pass)
  , tc (XLmlExprApply pass)
  , tc (XLmlExprMatch pass)
  , tc (LLmlCase pass)
  , tc (XLmlExprTuple pass)
  , tc (XLmlExprConstruct pass)
  , tc (LLongident pass)
  , tc (XLmlExprIfThenElse pass)
  , tc (XLmlExprConstraint pass)
  , tc (LLmlType pass)
  , tc (XXExpr pass)
  )

deriving instance (ForallLmlExpr Show pass) => Show (LmlExpr pass)
deriving instance (ForallLmlExpr Eq pass) => Eq (LmlExpr pass)

-- | Located let binder
type LLmlBind pass = XLocated pass (LmlBind pass)

{- | Let binder type.

Resides here due to size and to save us from .hs-boot file.
-}
data LmlBind pass
  = LmlBind (XLmlBind pass) (LLmlPat pass) (LLmlExpr pass)
  | XLmlBind !(XXBind pass)

type ForallLmlBind (tc :: Type -> Constraint) pass =
  (tc (XLmlBind pass), tc (LLmlPat pass), tc (LLmlExpr pass), tc (XXBind pass))

deriving instance (ForallLmlBind Show pass) => Show (LmlBind pass)
deriving instance (ForallLmlBind Eq pass) => Eq (LmlBind pass)

-- | Located case binder type
type LLmlCase pass = XLocated pass (LmlCase pass)

-- | Case binder type
data LmlCase pass
  = LmlCase (XLmlCase pass) (LLmlPat pass) (Maybe (LLmlExpr pass)) (LLmlExpr pass)
  | XLmlCase !(XXCase pass)

type ForallLmlCase (tc :: Type -> Constraint) pass =
  (tc (XLmlCase pass), tc (LLmlPat pass), tc (LLmlExpr pass), tc (XXCase pass))

deriving instance (ForallLmlCase Show pass) => Show (LmlCase pass)
deriving instance (ForallLmlCase Eq pass) => Eq (LmlCase pass)
