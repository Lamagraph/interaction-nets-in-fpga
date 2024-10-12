module Lamagraph.Compiler.Syntax.Pat where

import Relude

import Lamagraph.Compiler.Syntax.Extension
import Lamagraph.Compiler.Syntax.Lit
import Lamagraph.Compiler.Syntax.Longident
import Lamagraph.Compiler.Syntax.Type

type LLmlPat pass = XLocated pass (LmlPat pass)

data LmlPat pass
  = LmlPatAny (XLmlPatAny pass)
  | LmlPatVar (XLmlPatVar pass) (XLocated pass Text)
  | LmlPatConstant (XLmlPatConstant pass) (LmlLit pass)
  | LmlPatTuple (XLmlPatTuple pass) (LLmlPat pass) (NonEmpty (LLmlPat pass))
  | LmlPatConstruct (XLmlPatConstruct pass) (LLongident pass) (Maybe (LLmlPat pass))
  | LmlPatOr (XLmlPatOr pass) (LLmlPat pass) (LLmlPat pass)
  | LmlPatConstraint (XLmlPatConstraint pass) (LLmlPat pass) (LLmlType pass)
  | XLmlPat !(XXPat pass)
