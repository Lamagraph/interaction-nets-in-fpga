{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeFamilies #-}

-- | Lmlc (LamagraphML Compiler) specializations for LML AST
module Lamagraph.Compiler.Extension (Pass (..), LmlcPass (..), LmlcPs) where

import Lamagraph.Compiler.Parser.SrcLoc
import Lamagraph.Compiler.Syntax

data Pass = Parsed

data LmlcPass (c :: Pass) where
  LmlcPs :: LmlcPass 'Parsed

type LmlcPs =
  -- | Output of parser
  LmlcPass 'Parsed

type instance XLocated (LmlcPass p) a = Located a

type instance XCModule (LmlcPass _) = NoExtField
type instance XXModule (LmlcPass _) = DataConCantHappen

type instance XOpenD (LmlcPass _) = NoExtField
type instance XValD (LmlcPass _) = NoExtField
type instance XTyD (LmlcPass _) = NoExtField
type instance XXDecl (LmlcPass _) = DataConCantHappen

type instance XOpenDecl (LmlcPass _) = NoExtField
type instance XXOpenDecl (LmlcPass _) = DataConCantHappen

type instance XAliasDecl (LmlcPass _) = NoExtField
type instance XDataDecl (LmlcPass _) = NoExtField
type instance XXTyDecl (LmlcPass _) = DataConCantHappen

type instance XConDecl (LmlcPass _) = NoExtField
type instance XXConDecl (LmlcPass _) = DataConCantHappen

type instance XLmlTyVar (LmlcPass _) = NoExtField
type instance XLmlTyArrow (LmlcPass _) = NoExtField
type instance XLmlTyTuple (LmlcPass _) = NoExtField
type instance XLmlTyConstr (LmlcPass _) = NoExtField
type instance XXType (LmlcPass _) = DataConCantHappen

type instance XLmlInt (LmlcPass _) = NoExtField
type instance XLmlInt32 (LmlcPass _) = NoExtField
type instance XLmlUInt32 (LmlcPass _) = NoExtField
type instance XLmlInt64 (LmlcPass _) = NoExtField
type instance XLmlUInt64 (LmlcPass _) = NoExtField
type instance XLmlChar (LmlcPass _) = NoExtField
type instance XLmlString (LmlcPass _) = NoExtField
type instance XXLit (LmlcPass _) = DataConCantHappen

type instance XLmlPatAny (LmlcPass _) = NoExtField
type instance XLmlPatVar (LmlcPass _) = NoExtField
type instance XLmlPatConstant (LmlcPass _) = NoExtField
type instance XLmlPatTuple (LmlcPass _) = NoExtField
type instance XLmlPatConstruct (LmlcPass _) = NoExtField
type instance XLmlPatOr (LmlcPass _) = NoExtField
type instance XLmlPatConstraint (LmlcPass _) = NoExtField
type instance XXPat (LmlcPass _) = DataConCantHappen

type instance XLmlExprIdent (LmlcPass _) = NoExtField
type instance XLmlExprConstant (LmlcPass _) = NoExtField
type instance XLmlExprLet (LmlcPass _) = NoExtField
type instance XLmlExprFunction (LmlcPass _) = NoExtField
type instance XLmlExprApply (LmlcPass _) = NoExtField
type instance XLmlExprMatch (LmlcPass _) = NoExtField
type instance XLmlExprTuple (LmlcPass _) = NoExtField
type instance XLmlExprConstruct (LmlcPass _) = NoExtField
type instance XLmlExprIfThenElse (LmlcPass _) = NoExtField
type instance XLmlExprConstraint (LmlcPass _) = NoExtField
type instance XXExpr (LmlcPass _) = DataConCantHappen

type instance XLmlBindGroup (LmlcPass _) = NoExtField
type instance XXBindGroup (LmlcPass _) = DataConCantHappen

type instance XLmlBind (LmlcPass _) = NoExtField
type instance XXBind (LmlcPass _) = DataConCantHappen

type instance XLmlCase (LmlcPass _) = NoExtField
type instance XXCase (LmlcPass _) = DataConCantHappen
