module Lamagraph.Compiler.Typechecker.Infer.Type (lLmlTypeToTy, lLmlTypeToTyWithEnv, lLmlTypeToTyStrict) where

import Relude

import Control.Lens
import Control.Monad.Except
import Data.HashMap.Strict qualified as HashMap
import Data.List.NonEmpty.Extra qualified as NE

import Lamagraph.Compiler.Extension
import Lamagraph.Compiler.Parser.SrcLoc
import Lamagraph.Compiler.Syntax
import Lamagraph.Compiler.Typechecker.Helper (freshTVar)
import Lamagraph.Compiler.Typechecker.TcTypes (
  MonadTypecheck,
  Name (..),
  Ty (..),
  TyConstrEnv (..),
  TyConstrInfo (..),
  TypecheckError (..),
 )

substTy :: HashMap Name Ty -> Ty -> Ty
substTy subst = \case
  tv@(TVar name) -> HashMap.lookupDefault tv name subst
  TArrow t1 t2 -> TArrow (substTy subst t1) (substTy subst t2)
  TConstr name tys -> TConstr name (map (substTy subst) tys)
  TTuple t ts -> TTuple (substTy subst t) (fmap (substTy subst) ts)

lLmlTypeToTy :: TyConstrEnv -> LLmlType LmlcMr -> MonadTypecheck (Ty, LLmlType LmlcTc)
lLmlTypeToTy = lLmlTypeToTyWithEnv HashMap.empty

lLmlTypeToTyWithEnv :: HashMap Name Ty -> TyConstrEnv -> LLmlType LmlcMr -> MonadTypecheck (Ty, LLmlType LmlcTc)
lLmlTypeToTyWithEnv env tyConstrEnv (L loc lmlType) = over _2 (L loc) <$> lmlTypeToTyWithEnv' env tyConstrEnv lmlType

lmlTypeToTyWithEnv' :: HashMap Name Ty -> TyConstrEnv -> LmlType LmlcMr -> MonadTypecheck (Ty, LmlType LmlcTc)
lmlTypeToTyWithEnv' env tyConstrEnv = \case
  LmlTyVar _ ident@(L _ identText) -> do
    let varName = Name (Longident (identText :| []))
    case HashMap.lookup varName env of
      Just ty -> pure (ty, LmlTyVar ty ident)
      Nothing -> do
        tVar <- freshTVar
        pure (tVar, LmlTyVar tVar ident)
  LmlTyArrow _ lTy1 lTy2 -> do
    (leftTy, lTy1Typed) <- lLmlTypeToTyWithEnv env tyConstrEnv lTy1
    (rightTy, lTy2Typed) <- lLmlTypeToTyWithEnv env tyConstrEnv lTy2
    let outTy = leftTy `TArrow` rightTy
    pure (outTy, LmlTyArrow outTy lTy1Typed lTy2Typed)
  LmlTyTuple _ lTy lTys -> do
    (ty, lTyTyped) <- lLmlTypeToTyWithEnv env tyConstrEnv lTy
    (tys, lTysTyped) <- NE.unzip <$> traverse (lLmlTypeToTyWithEnv env tyConstrEnv) lTys
    let outTy = TTuple ty tys
    pure (outTy, LmlTyTuple outTy lTyTyped lTysTyped)
  LmlTyConstr _ lLongident@(L _ constrName) lTys -> do
    let constrNameTy = Name constrName
        TyConstrEnv tyConstrMap = tyConstrEnv
    case HashMap.lookup constrNameTy tyConstrMap of
      Nothing -> throwError $ UnboundTypeConstructor constrNameTy
      Just tyConstrInfo -> do
        case tyConstrInfo of
          DataConstr expectedArity -> do
            let actualArity = length lTys
            when (actualArity /= expectedArity) $
              throwError $
                ArityMismatch constrNameTy expectedArity actualArity
            (tys, lTyTyped) <- NE.unzip <$> traverse (lLmlTypeToTyWithEnv env tyConstrEnv) lTys
            let outTy = TConstr constrNameTy tys
            pure (outTy, LmlTyConstr outTy lLongident lTyTyped)
          TypeAlias paramNames rhsTy -> do
            let actualArity = length lTys
                expectedArity = length paramNames
            when (actualArity /= expectedArity) $
              throwError $
                ArityMismatch constrNameTy expectedArity actualArity
            (tys, lTyTyped) <- NE.unzip <$> traverse (lLmlTypeToTyWithEnv env tyConstrEnv) lTys
            let subst = HashMap.fromList $ zip paramNames tys
                outTy = substTy subst rhsTy
            pure (outTy, LmlTyConstr outTy lLongident lTyTyped)

lLmlTypeToTyStrict :: HashMap Name Ty -> TyConstrEnv -> LLmlType LmlcMr -> MonadTypecheck (Ty, LLmlType LmlcTc)
lLmlTypeToTyStrict env tyConstrEnv (L loc lmlType) = over _2 (L loc) <$> lmlTypeToTyStrict' env tyConstrEnv lmlType

lmlTypeToTyStrict' :: HashMap Name Ty -> TyConstrEnv -> LmlType LmlcMr -> MonadTypecheck (Ty, LmlType LmlcTc)
lmlTypeToTyStrict' env tyConstrEnv = \case
  LmlTyVar _ ident@(L _ identText) -> do
    let varName = Name (Longident (identText :| []))
    case HashMap.lookup varName env of
      Just ty -> pure (ty, LmlTyVar ty ident)
      Nothing -> throwError $ UndefinedTypeVariable varName
  LmlTyArrow _ lTy1 lTy2 -> do
    (leftTy, lTy1Typed) <- lLmlTypeToTyStrict env tyConstrEnv lTy1
    (rightTy, lTy2Typed) <- lLmlTypeToTyStrict env tyConstrEnv lTy2
    let outTy = leftTy `TArrow` rightTy
    pure (outTy, LmlTyArrow outTy lTy1Typed lTy2Typed)
  LmlTyTuple _ lTy lTys -> do
    (ty, lTyTyped) <- lLmlTypeToTyStrict env tyConstrEnv lTy
    (tys, lTysTyped) <- NE.unzip <$> traverse (lLmlTypeToTyStrict env tyConstrEnv) lTys
    let outTy = TTuple ty tys
    pure (outTy, LmlTyTuple outTy lTyTyped lTysTyped)
  LmlTyConstr _ lLongident@(L _ constrName) lTys -> do
    let constrNameTy = Name constrName
        TyConstrEnv tyConstrMap = tyConstrEnv
    case HashMap.lookup constrNameTy tyConstrMap of
      Nothing -> throwError $ UnboundTypeConstructor constrNameTy
      Just tyConstrInfo -> do
        case tyConstrInfo of
          DataConstr expectedArity -> do
            let actualArity = length lTys
            when (actualArity /= expectedArity) $
              throwError $
                ArityMismatch constrNameTy expectedArity actualArity
            (tys, lTyTyped) <- NE.unzip <$> traverse (lLmlTypeToTyStrict env tyConstrEnv) lTys
            let outTy = TConstr constrNameTy tys
            pure (outTy, LmlTyConstr outTy lLongident lTyTyped)
          TypeAlias paramNames rhsTy -> do
            let actualArity = length lTys
                expectedArity = length paramNames
            when (actualArity /= expectedArity) $
              throwError $
                ArityMismatch constrNameTy expectedArity actualArity
            (tys, lTyTyped) <- NE.unzip <$> traverse (lLmlTypeToTyStrict env tyConstrEnv) lTys
            let subst = HashMap.fromList $ zip paramNames tys
                outTy = substTy subst rhsTy
            pure (outTy, LmlTyConstr outTy lLongident lTyTyped)
