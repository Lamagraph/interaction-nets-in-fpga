-- Enabled because of weird warning produced by @Show (LmlExpr pass)@ instance
{-# LANGUAGE MonoLocalBinds #-}
{-# LANGUAGE UndecidableInstances #-}

-- TODO: Exports
module Lamagraph.Compiler.Syntax.Expr where

import Relude

import Lamagraph.Compiler.Syntax.Extension
import Lamagraph.Compiler.Syntax.Lit
import Lamagraph.Compiler.Syntax.Longident
import Lamagraph.Compiler.Syntax.Pat
import Lamagraph.Compiler.Syntax.Type

data RecFlag = Recursive | NonRecursive deriving (Show)

type LLmlExpr pass = XLocated pass (LmlExpr pass)

data LmlExpr pass
  = LmlExprIdent (XLmlExprIdent pass) Longident
  | LmlExprConstant (XLmlExprConstant pass) (LmlLit pass)
  | LmlExprLet (XLmlExprLet pass) RecFlag (NonEmpty (LLmlBind pass)) (LLmlExpr pass)
  | LmlExprFunction (XLmlExprFunction pass) (LLmlPat pass) (LLmlExpr pass)
  | LmlExprApply (XLmlExprApply pass) (LLmlExpr pass) (NonEmpty (LLmlExpr pass))
  | LmlExprMatch (XLmlExprMatch pass) (LLmlExpr pass) (NonEmpty (LLmlCase pass))
  | LmlExprTuple (XLmlExprTuple pass) (LLmlExpr pass) (NonEmpty (LLmlExpr pass))
  | LmlExprConstruct (XLmlExprConstruct pass) (LLongident pass) (Maybe (LLmlExpr pass))
  | LmlExprIfThenElse (XLmlExprIfThenElse pass) (LLmlExpr pass) (LLmlExpr pass) (LLmlExpr pass)
  | LmlExprConstraint (XLmlExprConstraint pass) (LLmlExpr pass) (LLmlType pass)
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

-- | Located case binder type
type LLmlCase pass = XLocated pass (LmlCase pass)

-- | Case binder type
data LmlCase pass
  = LmlCase (XLmlCase pass) (LLmlPat pass) (Maybe (LLmlExpr pass)) (LLmlExpr pass)
  | XLmlCase !(XXCase pass)

type ForallLmlCase (tc :: Type -> Constraint) pass =
  (tc (XLmlCase pass), tc (LLmlPat pass), tc (LLmlExpr pass), tc (XXCase pass))

deriving instance (ForallLmlCase Show pass) => Show (LmlCase pass)
