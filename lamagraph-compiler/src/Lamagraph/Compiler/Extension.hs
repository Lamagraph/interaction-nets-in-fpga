{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeFamilies #-}

-- | Lmlc (LamagraphML Compiler) specializations for LML AST
module Lamagraph.Compiler.Extension (Pass (..), LmlcPass (..), LmlcPs, LmlcMr, LmlcTc) where

import Relude

import Lamagraph.Compiler.ModuleResolver.MrTypes
import Lamagraph.Compiler.Parser.SrcLoc
import Lamagraph.Compiler.Syntax
import Lamagraph.Compiler.Typechecker.TcTypes

data Pass = Parsed | NameResolved | Typechecked

data LmlcPass (c :: Pass) where
  LmlcPs :: LmlcPass 'Parsed
  LmlcMr :: LmlcPass 'NameResolved
  LmlcTc :: LmlcPass 'Typechecked

-- | Output of parser
type LmlcPs = LmlcPass 'Parsed

-- | Output of module resolver
type LmlcMr = LmlcPass 'NameResolved

-- | Output of typechecker
type LmlcTc = LmlcPass 'Typechecked

type instance XLocated (LmlcPass p) a = Located a

type instance XCModule LmlcPs = NoExtField
type instance XCModule LmlcMr = NoExtField
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
type instance XLmlTyVar LmlcMr = NoExtField
type instance XLmlTyVar LmlcTc = Ty
type instance XLmlTyArrow LmlcPs = NoExtField
type instance XLmlTyArrow LmlcMr = NoExtField
type instance XLmlTyArrow LmlcTc = Ty
type instance XLmlTyTuple LmlcPs = NoExtField
type instance XLmlTyTuple LmlcMr = NoExtField
type instance XLmlTyTuple LmlcTc = Ty
type instance XLmlTyConstr LmlcPs = NoExtField
type instance XLmlTyConstr LmlcMr = NoExtField
type instance XLmlTyConstr LmlcTc = Ty
type instance XXType (LmlcPass _) = DataConCantHappen

type instance XLmlInt LmlcPs = NoExtField
type instance XLmlInt LmlcMr = NoExtField
type instance XLmlInt LmlcTc = Ty
type instance XLmlChar LmlcPs = NoExtField
type instance XLmlChar LmlcMr = NoExtField
type instance XLmlChar LmlcTc = Ty
type instance XLmlString LmlcPs = NoExtField
type instance XLmlString LmlcMr = NoExtField
type instance XLmlString LmlcTc = Ty
type instance XXLit (LmlcPass _) = DataConCantHappen

type instance XLmlPatAny LmlcPs = NoExtField
type instance XLmlPatAny LmlcMr = NoExtField
type instance XLmlPatAny LmlcTc = Ty
type instance XLmlPatVar LmlcPs = NoExtField
type instance XLmlPatVar LmlcMr = FullName
type instance XLmlPatVar LmlcTc = Ty
type instance XLmlPatConstant LmlcPs = NoExtField
type instance XLmlPatConstant LmlcMr = NoExtField
type instance XLmlPatConstant LmlcTc = Ty
type instance XLmlPatTuple LmlcPs = NoExtField
type instance XLmlPatTuple LmlcMr = NoExtField
type instance XLmlPatTuple LmlcTc = Ty
type instance XLmlPatConstruct LmlcPs = NoExtField
type instance XLmlPatConstruct LmlcMr = NoExtField
type instance XLmlPatConstruct LmlcTc = Ty
type instance XLmlPatOr LmlcPs = NoExtField
type instance XLmlPatOr LmlcMr = NoExtField
type instance XLmlPatOr LmlcTc = Ty
type instance XLmlPatConstraint LmlcPs = NoExtField
type instance XLmlPatConstraint LmlcMr = NoExtField
type instance XLmlPatConstraint LmlcTc = Ty
type instance XXPat (LmlcPass _) = DataConCantHappen

type instance XLmlExprIdent LmlcPs = NoExtField
type instance XLmlExprIdent LmlcMr = NoExtField
type instance XLmlExprIdent LmlcTc = Ty
type instance XLmlExprConstant LmlcPs = NoExtField
type instance XLmlExprConstant LmlcMr = NoExtField
type instance XLmlExprConstant LmlcTc = Ty
type instance XLmlExprLet LmlcPs = NoExtField
type instance XLmlExprLet LmlcMr = NoExtField
type instance XLmlExprLet LmlcTc = Ty
type instance XLmlExprFunction LmlcPs = NoExtField
type instance XLmlExprFunction LmlcMr = NoExtField
type instance XLmlExprFunction LmlcTc = Ty
type instance XLmlExprApply LmlcPs = NoExtField
type instance XLmlExprApply LmlcMr = NoExtField
type instance XLmlExprApply LmlcTc = Ty
type instance XLmlExprMatch LmlcPs = NoExtField
type instance XLmlExprMatch LmlcMr = NoExtField
type instance XLmlExprMatch LmlcTc = Ty
type instance XLmlExprTuple LmlcPs = NoExtField
type instance XLmlExprTuple LmlcMr = NoExtField
type instance XLmlExprTuple LmlcTc = Ty
type instance XLmlExprConstruct LmlcPs = NoExtField
type instance XLmlExprConstruct LmlcMr = NoExtField
type instance XLmlExprConstruct LmlcTc = Ty
type instance XLmlExprIfThenElse LmlcPs = NoExtField
type instance XLmlExprIfThenElse LmlcMr = NoExtField
type instance XLmlExprIfThenElse LmlcTc = Ty
type instance XLmlExprConstraint LmlcPs = NoExtField
type instance XLmlExprConstraint LmlcMr = NoExtField
type instance XLmlExprConstraint LmlcTc = Ty
type instance XXExpr (LmlcPass _) = DataConCantHappen

type instance XLmlBindGroup (LmlcPass _) = NoExtField
type instance XXBindGroup (LmlcPass _) = DataConCantHappen

type instance XLmlBind LmlcPs = NoExtField
type instance XLmlBind LmlcMr = NoExtField
type instance XLmlBind LmlcTc = TyEnv
type instance XXBind (LmlcPass _) = DataConCantHappen

type instance XLmlCase LmlcPs = NoExtField
type instance XLmlCase LmlcMr = NoExtField
type instance XLmlCase LmlcTc = Ty
type instance XXCase (LmlcPass _) = DataConCantHappen
