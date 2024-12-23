{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeFamilies #-}

-- | Lmlc (LamagraphML Compiler) specializations for LML AST
module Lamagraph.Compiler.Extension (Pass (..), LmlcPass (..), LmlcPs, LmlcTc) where

import Lamagraph.Compiler.Parser.SrcLoc
import Lamagraph.Compiler.Syntax
import Lamagraph.Compiler.Typechecker.TcTypes

data Pass = Parsed | Typechecked

data LmlcPass (c :: Pass) where
  LmlcPs :: LmlcPass 'Parsed
  LmlcTc :: LmlcPass 'Typechecked

-- | Output of parser
type LmlcPs = LmlcPass 'Parsed

-- | Output of typechecker
type LmlcTc = LmlcPass 'Typechecked

type instance XLocated (LmlcPass p) a = Located a

type instance XCModule LmlcPs = NoExtField
type instance XCModule LmlcTc = TyEnv
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

type instance XLmlTyVar LmlcPs = NoExtField
type instance XLmlTyVar LmlcTc = Ty
type instance XLmlTyArrow LmlcPs = NoExtField
type instance XLmlTyArrow LmlcTc = Ty
type instance XLmlTyTuple LmlcPs = NoExtField
type instance XLmlTyTuple LmlcTc = Ty
type instance XLmlTyConstr LmlcPs = NoExtField
type instance XLmlTyConstr LmlcTc = Ty
type instance XXType (LmlcPass _) = DataConCantHappen

type instance XLmlInt LmlcPs = NoExtField
type instance XLmlInt LmlcTc = Ty
type instance XLmlChar LmlcPs = NoExtField
type instance XLmlChar LmlcTc = Ty
type instance XLmlString LmlcPs = NoExtField
type instance XLmlString LmlcTc = Ty
type instance XXLit (LmlcPass _) = DataConCantHappen

type instance XLmlPatAny LmlcPs = NoExtField
type instance XLmlPatAny LmlcTc = Ty
type instance XLmlPatVar LmlcPs = NoExtField
type instance XLmlPatVar LmlcTc = Ty
type instance XLmlPatConstant LmlcPs = NoExtField
type instance XLmlPatConstant LmlcTc = Ty
type instance XLmlPatTuple LmlcPs = NoExtField
type instance XLmlPatTuple LmlcTc = Ty
type instance XLmlPatConstruct LmlcPs = NoExtField
type instance XLmlPatConstruct LmlcTc = Ty
type instance XLmlPatOr LmlcPs = NoExtField
type instance XLmlPatOr LmlcTc = Ty
type instance XLmlPatConstraint LmlcPs = NoExtField
type instance XLmlPatConstraint LmlcTc = Ty
type instance XXPat (LmlcPass _) = DataConCantHappen

type instance XLmlExprIdent LmlcPs = NoExtField
type instance XLmlExprIdent LmlcTc = Ty
type instance XLmlExprConstant LmlcPs = NoExtField
type instance XLmlExprConstant LmlcTc = Ty
type instance XLmlExprLet LmlcPs = NoExtField
type instance XLmlExprLet LmlcTc = Ty
type instance XLmlExprFunction LmlcPs = NoExtField
type instance XLmlExprFunction LmlcTc = Ty
type instance XLmlExprApply LmlcPs = NoExtField
type instance XLmlExprApply LmlcTc = Ty
type instance XLmlExprMatch LmlcPs = NoExtField
type instance XLmlExprMatch LmlcTc = Ty
type instance XLmlExprTuple LmlcPs = NoExtField
type instance XLmlExprTuple LmlcTc = Ty
type instance XLmlExprConstruct LmlcPs = NoExtField
type instance XLmlExprConstruct LmlcTc = Ty
type instance XLmlExprIfThenElse LmlcPs = NoExtField
type instance XLmlExprIfThenElse LmlcTc = Ty
type instance XLmlExprConstraint LmlcPs = NoExtField
type instance XLmlExprConstraint LmlcTc = Ty
type instance XXExpr (LmlcPass _) = DataConCantHappen

type instance XLmlBindGroup (LmlcPass _) = NoExtField
type instance XXBindGroup (LmlcPass _) = DataConCantHappen

type instance XLmlBind LmlcPs = NoExtField
type instance XLmlBind LmlcTc = TyEnv
type instance XXBind (LmlcPass _) = DataConCantHappen

type instance XLmlCase LmlcPs = NoExtField
type instance XLmlCase LmlcTc = Ty
type instance XXCase (LmlcPass _) = DataConCantHappen
