module Lamagraph.Compiler.Typechecker.Infer.Expr (inferLLmlBindGroup) where

import Relude

import Control.Lens
import Control.Monad.Except
import Data.Foldable1 hiding (head)
import Data.HashMap.Strict qualified as HashMap

import Lamagraph.Compiler.Extension
import Lamagraph.Compiler.Parser.SrcLoc
import Lamagraph.Compiler.Syntax
import Lamagraph.Compiler.Typechecker.DefaultEnv
import Lamagraph.Compiler.Typechecker.Helper
import Lamagraph.Compiler.Typechecker.Infer.Lit
import Lamagraph.Compiler.Typechecker.Infer.Pat
import Lamagraph.Compiler.Typechecker.Infer.Type
import Lamagraph.Compiler.Typechecker.TcTypes
import Lamagraph.Compiler.Typechecker.Unification

inferLLmlExpr :: TyEnv -> LLmlExpr LmlcPs -> MonadTypecheck Ty
inferLLmlExpr tyEnv (L _ expr) = inferLmlExpr tyEnv expr

inferLmlExpr :: TyEnv -> LmlExpr LmlcPs -> MonadTypecheck Ty
inferLmlExpr env@(TyEnv tyEnv) = \case
  LmlExprIdent _ longident -> case HashMap.lookup (Name longident) tyEnv of
    Nothing -> throwError $ UnboundVariable (coerce longident)
    Just tyScheme -> do
      instantiate tyScheme
  LmlExprConstant _ lit -> inferLmlLit lit
  LmlExprLet _ lBindGroup lExpr -> do
    bgTyEnv <- inferLLmlBindGroup env lBindGroup
    inferLLmlExpr bgTyEnv lExpr
  LmlExprFunction _ lPat lExpr -> do
    (patTy, Subst substMap) <- inferLLmlPat env NonRecursive lPat
    -- Forall [] here is allowed, since variables inferred from pattern will always be fresh
    let patEnv = fmap (Forall []) substMap
        envWithPats = TyEnv $ patEnv `HashMap.union` tyEnv
    exprTy <- inferLLmlExpr envWithPats lExpr
    pure $ patTy `TArrow` exprTy
  LmlExprApply _ apHead apArgs -> do
    tVar <- freshTVar
    apHeadTy <- inferLLmlExpr env apHead
    apArgsInferred <- mapM (inferLLmlExpr env) apArgs
    let apArgsTy = foldr TArrow tVar apArgsInferred
    unify apHeadTy apArgsTy
    pure tVar
  LmlExprMatch _ lExpr lCases -> do
    exprTy <- inferLLmlExpr env lExpr
    casesTys <- mapM (inferLLmlCase env) lCases
    tVar <- freshTVar
    mapM_ (unify (exprTy `TArrow` tVar)) casesTys
    pure tVar
  LmlExprTuple _ lExpr lExprs -> do
    ty <- inferLLmlExpr env lExpr
    tys <- mapM (inferLLmlExpr env) lExprs
    pure $ TTuple ty tys
  LmlExprConstruct _ (L _ longident) maybeLExpr -> case HashMap.lookup (Name longident) tyEnv of
    Nothing -> throwError $ ConstructorDoestExist (coerce longident)
    Just tyScheme -> do
      constrTy <- instantiate tyScheme
      case maybeLExpr of
        Nothing -> pure constrTy
        Just lExpr -> do
          exprTy <- inferLLmlExpr env lExpr
          tVar <- freshTVar
          unify constrTy (exprTy `TArrow` tVar)
          pure tVar
  LmlExprIfThenElse _ lCond lTrue lFalse -> do
    condTy <- inferLLmlExpr env lCond
    trueTy <- inferLLmlExpr env lTrue
    falseTy <- inferLLmlExpr env lFalse
    unify condTy tyBool
    unify trueTy falseTy
    pure falseTy
  LmlExprConstraint _ lExpr lType -> do
    exprTy <- inferLLmlExpr env lExpr
    expectedTy <- lLmlTypeToTy lType
    unify exprTy expectedTy
    pure expectedTy

inferLLmlBindGroup :: TyEnv -> LLmlBindGroup LmlcPs -> MonadTypecheck TyEnv
inferLLmlBindGroup env (L _ bg) = inferLmlBindGroup env bg

inferLmlBindGroup :: TyEnv -> LmlBindGroup LmlcPs -> MonadTypecheck TyEnv
inferLmlBindGroup env (LmlBindGroup _ NonRecursive binds) = do
  newEnvs <- mapM (inferLLmlBind env NonRecursive) binds
  bgEnv <- foldlM1 tyEnvUnionDisj newEnvs
  pure $ TyEnv $ coerce bgEnv `HashMap.union` coerce env
inferLmlBindGroup env (LmlBindGroup _ Recursive binds) = do
  patSubsts <- mapM (\(L _ (LmlBind _ lPat _)) -> snd <$> inferLLmlPat env Recursive lPat) binds
  (Subst patMap) <- foldlM1 (!@@) patSubsts
  -- Forall [] here is allowed, since variables inferred from pattern will always be fresh
  let patsEnv = fmap (Forall []) patMap
      envWithPats = TyEnv $ patsEnv `HashMap.union` coerce env
  foldM helper envWithPats binds
 where
  helper :: TyEnv -> LLmlBind LmlcPs -> MonadTypecheck TyEnv
  helper accEnv@(TyEnv tyEnv) lBind = do
    (TyEnv outEnv) <- inferLLmlBind accEnv Recursive lBind
    pure $ TyEnv $ outEnv `HashMap.union` tyEnv

inferLLmlBind :: TyEnv -> RecFlag -> LLmlBind LmlcPs -> MonadTypecheck TyEnv
inferLLmlBind env recFlag (L _ bind) = inferLmlBind env recFlag bind

inferLmlBind :: TyEnv -> RecFlag -> LmlBind LmlcPs -> MonadTypecheck TyEnv
inferLmlBind env recFlag (LmlBind _ lPat lExpr) = do
  (patTy, Subst patSubst) <- inferLLmlPat env recFlag lPat
  exprTy <- inferLLmlExpr env lExpr
  unify exprTy patTy
  subst <- use currentSubst
  pure $ TyEnv $ fmap (generalize (apply subst env)) (apply subst patSubst)

inferLLmlCase :: TyEnv -> LLmlCase LmlcPs -> MonadTypecheck Ty
inferLLmlCase env (L _ case') = inferLmlCase env case'

inferLmlCase :: TyEnv -> LmlCase LmlcPs -> MonadTypecheck Ty
inferLmlCase env@(TyEnv tyEnv) (LmlCase _ lPat maybeLGuard lExpr) = do
  (patTy, Subst substMap) <- inferLLmlPat env NonRecursive lPat
  -- Forall [] here is allowed, since variables inferred from pattern will always be fresh
  let patEnv = fmap (Forall []) substMap
      envWithPats = TyEnv $ patEnv `HashMap.union` tyEnv
  case maybeLGuard of
    Nothing -> pure ()
    Just lGuard -> do
      guardTy <- inferLLmlExpr envWithPats lGuard
      unify guardTy tyBool
  exprTy <- inferLLmlExpr envWithPats lExpr
  pure $ patTy `TArrow` exprTy
