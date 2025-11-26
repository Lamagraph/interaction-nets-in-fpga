module Lamagraph.Compiler.Typechecker.Infer.Expr (inferLLmlBindGroup) where

import Relude

import Control.Lens
import Control.Monad.Except
import Data.Foldable1 hiding (head)
import Data.HashMap.Strict qualified as HashMap
import Data.List.NonEmpty.Extra qualified as NE

import Lamagraph.Compiler.Extension
import Lamagraph.Compiler.Parser.SrcLoc
import Lamagraph.Compiler.PrettyAst ()
import Lamagraph.Compiler.Syntax
import Lamagraph.Compiler.Typechecker.DefaultEnv
import Lamagraph.Compiler.Typechecker.Helper
import Lamagraph.Compiler.Typechecker.Infer.Lit
import Lamagraph.Compiler.Typechecker.Infer.Pat
import Lamagraph.Compiler.Typechecker.Infer.Type
import Lamagraph.Compiler.Typechecker.Instances ()
import Lamagraph.Compiler.Typechecker.TcTypes
import Lamagraph.Compiler.Typechecker.Unification

inferLLmlExpr :: TyEnv -> LLmlExpr LmlcMr -> MonadTypecheck (Ty, LLmlExpr LmlcTc)
inferLLmlExpr tyEnv (L loc expr) = over _2 (L loc) <$> inferLmlExpr tyEnv expr

inferLmlExpr :: TyEnv -> LmlExpr LmlcMr -> MonadTypecheck (Ty, LmlExpr LmlcTc)
inferLmlExpr env@(TyEnv tyEnv) = \case
  LmlExprIdent _ longident -> case HashMap.lookup (Name longident) tyEnv of
    Nothing -> throwError $ UnboundVariable (coerce longident)
    Just tyScheme -> do
      outTy <- instantiate tyScheme
      pure (outTy, LmlExprIdent outTy longident)
  LmlExprConstant _ lit -> do
    (ty, litTyped) <- inferLmlLit lit
    pure (ty, LmlExprConstant ty litTyped)
  LmlExprLet _ lBindGroup lExpr -> do
    (bgTyEnv, lBindGroupTyped) <- inferLLmlBindGroup env lBindGroup
    (outTy, lExprTyped) <- inferLLmlExpr bgTyEnv lExpr
    pure (outTy, LmlExprLet outTy lBindGroupTyped lExprTyped)
  LmlExprFunction _ lPat lExpr -> do
    (patTy, Subst substMap, lPatTyped) <- inferLLmlPat env NonRecursive lPat
    -- Forall [] here is allowed, since variables inferred from pattern will always be fresh
    let patEnv = fmap (Forall []) substMap
        envWithPats = TyEnv $ patEnv `HashMap.union` tyEnv
    (exprTy, lExprTyped) <- inferLLmlExpr envWithPats lExpr
    let outTy = patTy `TArrow` exprTy
    pure (outTy, LmlExprFunction outTy lPatTyped lExprTyped)
  LmlExprApply _ apHead apArgs -> do
    tVar <- freshTVar
    (apHeadTy, apHeadTyped) <- inferLLmlExpr env apHead
    (apArgsInferred, apArgsTyped) <- NE.unzip <$> mapM (inferLLmlExpr env) apArgs
    let apArgsTy = foldr TArrow tVar apArgsInferred
    unify apHeadTy apArgsTy
    pure (tVar, LmlExprApply tVar apHeadTyped apArgsTyped)
  LmlExprMatch _ lExpr lCases -> do
    (exprTy, lExprTyped) <- inferLLmlExpr env lExpr
    (casesTys, lCasesTyped) <- NE.unzip <$> mapM (inferLLmlCase env) lCases
    tVar <- freshTVar
    mapM_ (unify (exprTy `TArrow` tVar)) casesTys
    pure (tVar, LmlExprMatch tVar lExprTyped lCasesTyped)
  LmlExprTuple _ lExpr lExprs -> do
    (ty, lExprTyped) <- inferLLmlExpr env lExpr
    (tys, lExprsTyped) <- NE.unzip <$> mapM (inferLLmlExpr env) lExprs
    let outTy = TTuple ty tys
    pure (outTy, LmlExprTuple outTy lExprTyped lExprsTyped)
  LmlExprConstruct _ lLongident@(L _ longident) maybeLExpr -> case HashMap.lookup (Name longident) tyEnv of
    Nothing -> throwError $ ConstructorDoesntExist (coerce longident)
    Just tyScheme -> do
      constrTy <- instantiate tyScheme
      case maybeLExpr of
        Nothing -> pure (constrTy, LmlExprConstruct constrTy lLongident Nothing)
        Just lExpr -> do
          (exprTy, lExprTyped) <- inferLLmlExpr env lExpr
          tVar <- freshTVar
          unify constrTy (exprTy `TArrow` tVar)
          pure (tVar, LmlExprConstruct tVar lLongident (Just lExprTyped))
  LmlExprIfThenElse _ lCond lTrue lFalse -> do
    (condTy, lCondTyped) <- inferLLmlExpr env lCond
    (trueTy, lTrueTyped) <- inferLLmlExpr env lTrue
    (falseTy, lFalseTyped) <- inferLLmlExpr env lFalse
    unify condTy tyBool
    unify trueTy falseTy
    pure (falseTy, LmlExprIfThenElse falseTy lCondTyped lTrueTyped lFalseTyped)
  LmlExprConstraint _ lExpr lType -> do
    (exprTy, lExprTyped) <- inferLLmlExpr env lExpr
    (expectedTy, lTypeTyped) <- lLmlTypeToTy lType
    unify exprTy expectedTy
    pure (expectedTy, LmlExprConstraint expectedTy lExprTyped lTypeTyped)

inferLLmlBindGroup :: TyEnv -> LLmlBindGroup LmlcMr -> MonadTypecheck (TyEnv, LLmlBindGroup LmlcTc)
inferLLmlBindGroup env (L loc bg) = over _2 (L loc) <$> inferLmlBindGroup env bg

inferLmlBindGroup :: TyEnv -> LmlBindGroup LmlcMr -> MonadTypecheck (TyEnv, LmlBindGroup LmlcTc)
inferLmlBindGroup env (LmlBindGroup _ NonRecursive binds) = do
  (newEnvs, lBinds) <- NE.unzip <$> mapM (inferLLmlNonRecBind env) binds
  bgEnv <- foldlM1 tyEnvUnionDisj newEnvs
  pure (TyEnv $ coerce bgEnv `HashMap.union` coerce env, LmlBindGroup noExtField NonRecursive lBinds)
inferLmlBindGroup env (LmlBindGroup _ Recursive binds) = do
  patInfo <- mapM (\(L _ (LmlBind _ lPat _)) -> inferLLmlPat env Recursive lPat) binds
  let (_, patSubsts, _) = unzip3F patInfo
  (Subst patMap) <- foldlM1 (!@@) patSubsts
  -- Forall [] here is allowed, since variables inferred from pattern will always be fresh
  let patsEnv = fmap (Forall []) patMap
      envWithPats = TyEnv $ patsEnv `HashMap.union` coerce env
      t = NE.zip patInfo binds
  (newEnvs, lBinds) <- NE.unzip <$> mapM (uncurry (inferLLmlRecBind envWithPats)) t
  bgEnv <- foldlM1 tyEnvUnionDisj (NE.cons env newEnvs)
  pure (bgEnv, LmlBindGroup noExtField Recursive lBinds)

inferLLmlRecBind :: TyEnv -> (Ty, Subst, LLmlPat LmlcTc) -> LLmlBind LmlcMr -> MonadTypecheck (TyEnv, LLmlBind LmlcTc)
inferLLmlRecBind env patInfo (L loc bind) = over _2 (L loc) <$> inferLmlRecBind env patInfo bind

inferLmlRecBind :: TyEnv -> (Ty, Subst, LLmlPat LmlcTc) -> LmlBind LmlcMr -> MonadTypecheck (TyEnv, LmlBind LmlcTc)
inferLmlRecBind env (patTy, Subst patSubst, lPatTyped) (LmlBind _ _ lExpr) = do
  (exprTy, lExprTyped) <- inferLLmlExpr env lExpr
  unify exprTy patTy
  subst <- use currentSubst
  let outEnv = TyEnv $ fmap (generalize (apply subst env)) (apply subst patSubst)
  pure (outEnv, LmlBind outEnv (apply subst lPatTyped) (apply subst lExprTyped))

inferLLmlNonRecBind :: TyEnv -> LLmlBind LmlcMr -> MonadTypecheck (TyEnv, LLmlBind LmlcTc)
inferLLmlNonRecBind env (L loc bind) = over _2 (L loc) <$> inferLmlNonRecBind env bind

inferLmlNonRecBind :: TyEnv -> LmlBind LmlcMr -> MonadTypecheck (TyEnv, LmlBind LmlcTc)
inferLmlNonRecBind env (LmlBind _ lPat lExpr) = do
  (patTy, Subst patSubst, lPatTyped) <- inferLLmlPat env NonRecursive lPat
  (exprTy, lExprTyped) <- inferLLmlExpr env lExpr
  unify exprTy patTy
  subst <- use currentSubst
  let outEnv = TyEnv $ fmap (generalize (apply subst env)) (apply subst patSubst)
  pure (outEnv, LmlBind outEnv (apply subst lPatTyped) (apply subst lExprTyped))

inferLLmlCase :: TyEnv -> LLmlCase LmlcMr -> MonadTypecheck (Ty, LLmlCase LmlcTc)
inferLLmlCase env (L loc case') = over _2 (L loc) <$> inferLmlCase env case'

inferLmlCase :: TyEnv -> LmlCase LmlcMr -> MonadTypecheck (Ty, LmlCase LmlcTc)
inferLmlCase env@(TyEnv tyEnv) (LmlCase _ lPat maybeLGuard lExpr) = do
  (patTy, Subst substMap, lPatTyped) <- inferLLmlPat env NonRecursive lPat
  -- Forall [] here is allowed, since variables inferred from pattern will always be fresh
  let patEnv = fmap (Forall []) substMap
      envWithPats = TyEnv $ patEnv `HashMap.union` tyEnv
  maybeLGuardTyped <- case maybeLGuard of
    Nothing -> pure Nothing
    Just lGuard -> do
      (guardTy, lGuardTyped) <- inferLLmlExpr envWithPats lGuard
      unify guardTy tyBool
      pure $ Just lGuardTyped
  (exprTy, lExprTyped) <- inferLLmlExpr envWithPats lExpr
  let outTy = patTy `TArrow` exprTy
  pure (outTy, LmlCase outTy lPatTyped maybeLGuardTyped lExprTyped)
