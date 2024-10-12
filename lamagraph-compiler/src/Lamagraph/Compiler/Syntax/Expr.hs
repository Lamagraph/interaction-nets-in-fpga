module Lamagraph.Compiler.Syntax.Expr where

import Relude

import Lamagraph.Compiler.Syntax.Extension
import Lamagraph.Compiler.Syntax.Lit
import Lamagraph.Compiler.Syntax.Longident
import Lamagraph.Compiler.Syntax.Pat
import Lamagraph.Compiler.Syntax.Type

data RecFlag = Recursive | NonRecursive

type LLmlExpr pass = XLocated pass (LmlExpr pass)

data LmlExpr pass
  = LmlExprIdent (XLmlExprIdent pass) (XLocated pass Text)
  | LmlExprConstant (XLmlExprConstant pass) (LmlLit pass)
  | LmlExprLet (XLmlExprLet pass) RecFlag (NonEmpty (LmlBind pass)) (LLmlExpr pass)
  | LmlExprFunction (XLmlExprFunction pass) (LLmlPat pass) (LLmlExpr pass)
  | LmlExprApply (XLmlExprApply pass) (LLmlExpr pass) (NonEmpty (LLmlExpr pass))
  | LmlExprMatch (XLmlExprMatch pass) (LLmlExpr pass) (NonEmpty (LLmlCase pass))
  | LmlExprTuple (XLmlExprTuple pass) (LLmlExpr pass) (NonEmpty (LLmlExpr pass))
  | LmlExprConstruct (XLmlExprConstruct pass) (LLongident pass) (Maybe (LLmlExpr pass))
  | LmlExprIfThenElse (XLmlExprIfThenElse pass) (LLmlExpr pass) (LLmlExpr pass) (LLmlExpr pass)
  | LmlExprConstraint (XLmlExprConstraint pass) (LLmlExpr pass) (LLmlType pass)
  | XLmlExpr !(XXExpr pass)

-- | Located let binder
type LLmlBind pass = XLocated pass (LmlBind pass)

{- | Let binder type.

Resides here due to size and to save us from .hs-boot file.
-}
data LmlBind pass
  = LmlBind (XLmlBind pass) (LLmlPat pass) (LLmlExpr pass)
  | XLmlBind !(XXBind pass)

-- | Located case binder type
type LLmlCase pass = XLocated pass (LmlCase pass)

-- | Case binder type
data LmlCase pass
  = LmlCase (XLmlCase pass) (LLmlPat pass) (Maybe (LLmlExpr pass)) (LLmlExpr pass)
  | XLmlCase !(XXCase pass)
