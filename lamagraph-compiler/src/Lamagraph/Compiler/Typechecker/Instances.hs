{-# OPTIONS_GHC -Wno-orphans #-}

{- | Module with (boilerplated) orphan instances of 'Tys' for selected node of the AST.

Is a separate module because in general these instances aren't required for the outside user,
and because of cyclic dependency between "Lamagraph.Compiler.Extension" and "Lamagraph.Compiler.Typechecker.TcTypes".

I'm not sure whether 'ftv' instances really required, but they are pretty easy to write, so let them be.
-}
module Lamagraph.Compiler.Typechecker.Instances () where

import Relude

import Data.HashSet qualified as HashSet

import Lamagraph.Compiler.Extension
import Lamagraph.Compiler.Parser.SrcLoc
import Lamagraph.Compiler.Syntax
import Lamagraph.Compiler.Typechecker.TcTypes

instance (Tys (a LmlcTc)) => Tys (Located (a LmlcTc)) where
  apply :: Subst -> Located (a LmlcTc) -> Located (a LmlcTc)
  apply subst (L pos a) = L pos (apply subst a)
  ftv :: Located (a LmlcTc) -> HashSet Name
  ftv (L _ a) = ftv a

instance Tys NoExtField where
  apply :: Subst -> NoExtField -> NoExtField
  apply _ = identity
  ftv :: NoExtField -> HashSet Name
  ftv _ = HashSet.empty

instance Tys (LmlExpr LmlcTc) where
  apply :: Subst -> LmlExpr LmlcTc -> LmlExpr LmlcTc
  apply subst = \case
    LmlExprIdent ext longident -> LmlExprIdent (apply subst ext) longident
    LmlExprConstant ext lit -> LmlExprConstant (apply subst ext) lit
    LmlExprLet ext lBindGroup lExpr -> LmlExprLet (apply subst ext) (apply subst lBindGroup) (apply subst lExpr)
    LmlExprFunction ext lPat lExpr -> LmlExprFunction (apply subst ext) (apply subst lPat) (apply subst lExpr)
    LmlExprApply ext lExpr lExprs -> LmlExprApply (apply subst ext) (apply subst lExpr) (apply subst lExprs)
    LmlExprMatch ext lExpr lCases -> LmlExprMatch (apply subst ext) (apply subst lExpr) (apply subst lCases)
    LmlExprTuple ext lExpr lExprs -> LmlExprTuple (apply subst ext) (apply subst lExpr) (apply subst lExprs)
    LmlExprConstruct ext lLongident maybeLExpr -> LmlExprConstruct (apply subst ext) lLongident (apply subst <$> maybeLExpr)
    LmlExprIfThenElse ext lCond lTrue lFalse -> LmlExprIfThenElse (apply subst ext) (apply subst lCond) (apply subst lTrue) (apply subst lFalse)
    LmlExprConstraint ext lExpr lTy -> LmlExprConstraint (apply subst ext) (apply subst lExpr) (apply subst lTy)
  ftv :: LmlExpr LmlcTc -> HashSet Name
  ftv = \case
    LmlExprIdent ext _ -> ftv ext
    LmlExprConstant ext lit -> ftv ext `HashSet.union` ftv lit
    LmlExprLet ext lBindGroup lExpr -> ftv ext `HashSet.union` ftv lBindGroup `HashSet.union` ftv lExpr
    LmlExprFunction ext lPat lExpr -> ftv ext `HashSet.union` ftv lPat `HashSet.union` ftv lExpr
    LmlExprApply ext lExpr lExprs -> ftv ext `HashSet.union` ftv lExpr `HashSet.union` ftv lExprs
    LmlExprMatch ext lExpr lCases -> ftv ext `HashSet.union` ftv lExpr `HashSet.union` ftv lCases
    LmlExprTuple ext lExpr lExprs -> ftv ext `HashSet.union` ftv lExpr `HashSet.union` ftv lExprs
    LmlExprConstruct ext _ maybeLExpr -> ftv ext `HashSet.union` maybe HashSet.empty ftv maybeLExpr
    LmlExprIfThenElse ext lCond lTrue lFalse -> ftv ext `HashSet.union` ftv lCond `HashSet.union` ftv lTrue `HashSet.union` ftv lFalse
    LmlExprConstraint ext lExpr lTy -> ftv ext `HashSet.union` ftv lExpr `HashSet.union` ftv lTy

instance Tys (LmlBindGroup LmlcTc) where
  apply :: Subst -> LmlBindGroup LmlcTc -> LmlBindGroup LmlcTc
  apply subst (LmlBindGroup ext recFlag lBinds) = LmlBindGroup (apply subst ext) recFlag (apply subst <$> lBinds)
  ftv :: LmlBindGroup LmlcTc -> HashSet Name
  ftv (LmlBindGroup ext _ lBinds) = ftv ext `HashSet.union` ftv lBinds

instance Tys (LmlBind LmlcTc) where
  apply :: Subst -> LmlBind LmlcTc -> LmlBind LmlcTc
  apply subst (LmlBind ext lPat lExpr) = LmlBind (apply subst ext) (apply subst lPat) (apply subst lExpr)
  ftv :: LmlBind LmlcTc -> HashSet Name
  ftv (LmlBind ext lPat lExpr) = ftv ext `HashSet.union` ftv lPat `HashSet.union` ftv lExpr

instance Tys (LmlCase LmlcTc) where
  apply :: Subst -> LmlCase LmlcTc -> LmlCase LmlcTc
  apply subst (LmlCase ext lPat maybeLExpr lExpr) = LmlCase (apply subst ext) (apply subst lPat) (apply subst <$> maybeLExpr) (apply subst lExpr)
  ftv :: LmlCase LmlcTc -> HashSet Name
  ftv (LmlCase ext lPat maybeLExpr lExpr) = ftv ext `HashSet.union` ftv lPat `HashSet.union` maybe HashSet.empty ftv maybeLExpr `HashSet.union` ftv lExpr

instance Tys (LmlLit LmlcTc) where
  apply :: Subst -> LmlLit LmlcTc -> LmlLit LmlcTc
  apply subst = \case
    LmlInt ext int -> LmlInt (apply subst ext) int
    LmlChar ext char -> LmlChar (apply subst ext) char
    LmlString ext string -> LmlString (apply subst ext) string
  ftv :: LmlLit LmlcTc -> HashSet Name
  ftv = \case
    LmlInt ext _ -> ftv ext
    LmlChar ext _ -> ftv ext
    LmlString ext _ -> ftv ext

instance Tys (LmlPat LmlcTc) where
  apply :: Subst -> LmlPat LmlcTc -> LmlPat LmlcTc
  apply subst = \case
    LmlPatAny ext -> LmlPatAny (apply subst ext)
    LmlPatVar ext name -> LmlPatVar (apply subst ext) name
    LmlPatConstant ext lit -> LmlPatConstant (apply subst ext) (apply subst lit)
    LmlPatTuple ext lPat lPats -> LmlPatTuple (apply subst ext) (apply subst lPat) (apply subst <$> lPats)
    LmlPatConstruct ext lLongident maybeLPat -> LmlPatConstruct (apply subst ext) lLongident (apply subst <$> maybeLPat)
    LmlPatOr ext lPat1 lPat2 -> LmlPatOr (apply subst ext) (apply subst lPat1) (apply subst lPat2)
    LmlPatConstraint ext lPat lTy -> LmlPatConstraint (apply subst ext) (apply subst lPat) (apply subst lTy)
  ftv :: LmlPat LmlcTc -> HashSet Name
  ftv = \case
    LmlPatAny ext -> ftv ext
    LmlPatVar ext _ -> ftv ext
    LmlPatConstant ext lit -> ftv ext `HashSet.union` ftv lit
    LmlPatTuple ext pat pats -> ftv ext `HashSet.union` ftv pat `HashSet.union` ftv pats
    LmlPatConstruct ext _ maybeLPat -> ftv ext `HashSet.union` maybe HashSet.empty ftv maybeLPat
    LmlPatOr ext lPat1 lPat2 -> ftv ext `HashSet.union` ftv lPat1 `HashSet.union` ftv lPat2
    LmlPatConstraint ext lPat lTy -> ftv ext `HashSet.union` ftv lPat `HashSet.union` ftv lTy

instance Tys (LmlType LmlcTc) where
  apply :: Subst -> LmlType LmlcTc -> LmlType LmlcTc
  apply subst = \case
    LmlTyVar ext name -> LmlTyVar (apply subst ext) name
    LmlTyArrow ext lTy1 lTy2 -> LmlTyArrow (apply subst ext) (apply subst lTy1) (apply subst lTy2)
    LmlTyTuple ext lTy lTys -> LmlTyTuple (apply subst ext) (apply subst lTy) (apply subst lTys)
    LmlTyConstr ext lLongident lTys -> LmlTyConstr (apply subst ext) lLongident (apply subst lTys)
  ftv :: LmlType LmlcTc -> HashSet Name
  ftv = \case
    LmlTyVar ext _ -> ftv ext
    LmlTyArrow ext lTy1 lTy2 -> ftv ext `HashSet.union` ftv lTy1 `HashSet.union` ftv lTy2
    LmlTyTuple ext lTy lTys -> ftv ext `HashSet.union` ftv lTy `HashSet.union` ftv lTys
    LmlTyConstr ext _ lTys -> ftv ext `HashSet.union` ftv lTys
