module Lamagraph.Compiler.Typechecker.Infer.Type (lLmlTypeToTy) where

import Relude

import Control.Lens
import Data.List.NonEmpty.Extra qualified as NE

import Lamagraph.Compiler.Extension
import Lamagraph.Compiler.Parser.SrcLoc
import Lamagraph.Compiler.Syntax
import Lamagraph.Compiler.Typechecker.Helper
import Lamagraph.Compiler.Typechecker.TcTypes

lLmlTypeToTy :: LLmlType LmlcMr -> MonadTypecheck (Ty, LLmlType LmlcTc)
lLmlTypeToTy (L loc lmlType) = over _2 (L loc) <$> lmlTypeToTy lmlType

lmlTypeToTy :: LmlType LmlcMr -> MonadTypecheck (Ty, LmlType LmlcTc)
lmlTypeToTy = \case
  LmlTyVar _ ident -> do
    tVar <- freshTVar
    pure (tVar, LmlTyVar tVar ident)
  LmlTyArrow _ lTy1 lTy2 -> do
    (leftTy, lTy1Typed) <- lLmlTypeToTy lTy1
    (rightTy, lTy2Typed) <- lLmlTypeToTy lTy2
    let outTy = leftTy `TArrow` rightTy
    pure (outTy, LmlTyArrow outTy lTy1Typed lTy2Typed)
  LmlTyTuple _ lTy lTys -> do
    (ty, lTyTyped) <- lLmlTypeToTy lTy
    (tys, lTysTyped) <- NE.unzip <$> mapM lLmlTypeToTy lTys
    let outTy = TTuple ty tys
    pure (outTy, LmlTyTuple outTy lTyTyped lTysTyped)
  LmlTyConstr _ lLongident@(L _ constrName) lTys -> do
    (tys, lTyTyped) <- NE.unzip <$> mapM lLmlTypeToTy lTys
    let outTy = TConstr (Name constrName) tys
    pure (outTy, LmlTyConstr outTy lLongident lTyTyped)
