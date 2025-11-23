module Lamagraph.Compiler.Typechecker.Infer.Expr (inferLLmlBindGroup) where

import Relude

import Control.Lens
import Control.Monad.Except
import Data.Foldable1 hiding (head)
import Data.HashMap.Strict qualified as HashMap
import Data.List.NonEmpty.Extra qualified as NE

import Lamagraph.Compiler.Extension
import Lamagraph.Compiler.ModuleResolver.Types (FullName (..))
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

inferLLmlExpr :: TyEnv -> TyConstrEnv -> LLmlExpr LmlcMr -> MonadTypecheck (Ty, LLmlExpr LmlcTc)
inferLLmlExpr tyEnv tyConstrEnv (L loc expr) = over _2 (L loc) <$> inferLmlExpr tyEnv tyConstrEnv expr

inferLmlExpr :: TyEnv -> TyConstrEnv -> LmlExpr LmlcMr -> MonadTypecheck (Ty, LmlExpr LmlcTc)
inferLmlExpr env@(TyEnv tyEnv) tyConstrEnv = \case
  LmlExprIdent _ longident -> case HashMap.lookup (Name longident) tyEnv of
    Nothing -> throwError $ UnboundVariable (coerce longident)
    Just tyScheme -> do
      outTy <- instantiate tyScheme
      pure (outTy, LmlExprIdent outTy longident)
  LmlExprConstant _ lit -> do
    (ty, litTyped) <- inferLmlLit lit
    pure (ty, LmlExprConstant ty litTyped)
  LmlExprLet _ lBindGroup lExpr -> do
    (bgTyEnv, lBindGroupTyped) <- inferLLmlBindGroup env tyConstrEnv lBindGroup
    (outTy, lExprTyped) <- inferLLmlExpr bgTyEnv tyConstrEnv lExpr
    pure (outTy, LmlExprLet outTy lBindGroupTyped lExprTyped)
  LmlExprFunction _ lPat lExpr -> do
    (patTy, Subst substMap, lPatTyped) <- inferLLmlPat env tyConstrEnv NonRecursive lPat
    -- Forall [] here is allowed, since variables inferred from pattern will always be fresh
    let patEnv = fmap (Forall []) substMap
        envWithPats = TyEnv $ patEnv `HashMap.union` tyEnv
    (exprTy, lExprTyped) <- inferLLmlExpr envWithPats tyConstrEnv lExpr
    let outTy = patTy `TArrow` exprTy
    pure (outTy, LmlExprFunction outTy lPatTyped lExprTyped)
  LmlExprApply _ apHead apArgs -> do
    tVar <- freshTVar
    (apHeadTy, apHeadTyped) <- inferLLmlExpr env tyConstrEnv apHead
    (apArgsInferred, apArgsTyped) <- NE.unzip <$> mapM (inferLLmlExpr env tyConstrEnv) apArgs
    let apArgsTy = foldr TArrow tVar apArgsInferred
    unify apHeadTy apArgsTy
    pure (tVar, LmlExprApply tVar apHeadTyped apArgsTyped)
  LmlExprMatch _ lExpr lCases -> do
    (exprTy, lExprTyped) <- inferLLmlExpr env tyConstrEnv lExpr
    (casesTys, lCasesTyped) <- NE.unzip <$> mapM (inferLLmlCase env tyConstrEnv) lCases
    tVar <- freshTVar
    mapM_ (unify (exprTy `TArrow` tVar)) casesTys
    pure (tVar, LmlExprMatch tVar lExprTyped lCasesTyped)
  LmlExprTuple _ lExpr lExprs -> do
    (ty, lExprTyped) <- inferLLmlExpr env tyConstrEnv lExpr
    (tys, lExprsTyped) <- NE.unzip <$> mapM (inferLLmlExpr env tyConstrEnv) lExprs
    let outTy = TTuple ty tys
    pure (outTy, LmlExprTuple outTy lExprTyped lExprsTyped)
  LmlExprConstruct _ lLongident@(L _ longident) maybeLExpr -> case HashMap.lookup (Name longident) tyEnv of
    Nothing -> throwError $ ConstructorDoesntExist (coerce longident)
    Just tyScheme -> do
      constrTy <- instantiate tyScheme
      case maybeLExpr of
        Nothing -> pure (constrTy, LmlExprConstruct constrTy lLongident Nothing)
        Just lExpr -> do
          (exprTy, lExprTyped) <- inferLLmlExpr env tyConstrEnv lExpr
          tVar <- freshTVar
          unify constrTy (exprTy `TArrow` tVar)
          pure (tVar, LmlExprConstruct tVar lLongident (Just lExprTyped))
  LmlExprIfThenElse _ lCond lTrue lFalse -> do
    (condTy, lCondTyped) <- inferLLmlExpr env tyConstrEnv lCond
    (trueTy, lTrueTyped) <- inferLLmlExpr env tyConstrEnv lTrue
    (falseTy, lFalseTyped) <- inferLLmlExpr env tyConstrEnv lFalse
    unify condTy tyBool
    unify trueTy falseTy
    pure (falseTy, LmlExprIfThenElse falseTy lCondTyped lTrueTyped lFalseTyped)
  LmlExprConstraint _ lExpr lType -> do
    (exprTy, lExprTyped) <- inferLLmlExpr env tyConstrEnv lExpr
    (expectedTy, lTypeTyped) <- lLmlTypeToTy tyConstrEnv lType
    unify exprTy expectedTy
    pure (expectedTy, LmlExprConstraint expectedTy lExprTyped lTypeTyped)

inferLLmlBindGroup :: TyEnv -> TyConstrEnv -> LLmlBindGroup LmlcMr -> MonadTypecheck (TyEnv, LLmlBindGroup LmlcTc)
inferLLmlBindGroup env tyConstrEnv (L loc bg) = over _2 (L loc) <$> inferLmlBindGroup env tyConstrEnv bg

inferLmlBindGroup :: TyEnv -> TyConstrEnv -> LmlBindGroup LmlcMr -> MonadTypecheck (TyEnv, LmlBindGroup LmlcTc)
inferLmlBindGroup env tyConstrEnv (LmlBindGroup _ NonRecursive binds) = do
  (newEnvs, lBinds) <- NE.unzip <$> mapM (inferLLmlNonRecBind env tyConstrEnv) binds
  bgEnv <- foldlM1 tyEnvUnionDisj newEnvs
  pure (TyEnv $ coerce bgEnv `HashMap.union` coerce env, LmlBindGroup noExtField NonRecursive lBinds)
inferLmlBindGroup env tyConstrEnv (LmlBindGroup _ Recursive binds) = do
  let varNames = fmap (\(L _ (LmlBind _ lPat _)) -> extractVarName lPat) binds
  let mkPlaceholderType :: LLmlExpr LmlcMr -> MonadTypecheck Ty
      mkPlaceholderType (L _ expr) = go expr
       where
        go :: LmlExpr LmlcMr -> MonadTypecheck Ty
        go (LmlExprFunction _ _ body) = do
          paramT <- freshTVar
          restTy <- go (unLoc body)
          pure $ paramT `TArrow` restTy
        go _ = freshTVar

  placeholderTypes <- forM binds $ \(L _ (LmlBind _ _ lExpr)) -> mkPlaceholderType lExpr

  let placeholderEnv = HashMap.fromList $ NE.toList $ NE.zip varNames (fmap (Forall []) placeholderTypes)
      envWithPlaceholders = TyEnv $ placeholderEnv `HashMap.union` coerce env

  patInfos <- forM (NE.zip binds placeholderTypes) $ \(L _ (LmlBind _ lPat _), placeholder) -> do
    (patTy, patSubst, lPatTyped) <- inferLLmlPat envWithPlaceholders tyConstrEnv Recursive lPat
    pure (patTy, placeholder, patSubst, lPatTyped)

  lBindsTyped <- forM (NE.zip patInfos binds) $ \((patTy, placeholder, patSubst, lPatTyped), L loc (LmlBind _ _ lExpr)) -> do
    (exprTy, lExprTyped) <- inferLLmlExpr envWithPlaceholders tyConstrEnv lExpr
    unify patTy placeholder
    unify exprTy placeholder
    pure (L loc (patSubst, lPatTyped, lExprTyped))

  finalSubst <- use currentSubst
  let finalTypes = fmap (apply finalSubst) placeholderTypes
      generalizedSchemes = fmap (generalize (apply finalSubst env)) finalTypes
      finalEnv = HashMap.fromList $ NE.toList $ NE.zip varNames generalizedSchemes
      resultEnv = TyEnv $ finalEnv `HashMap.union` coerce env

      finalBinds =
        NE.zipWith
          ( \(varName, scheme) (L loc (_, lPatTyped, lExprTyped)) ->
              let bindEnv = TyEnv $ HashMap.singleton varName scheme
               in L loc $ LmlBind bindEnv (apply finalSubst lPatTyped) (apply finalSubst lExprTyped)
          )
          (NE.zip varNames generalizedSchemes)
          lBindsTyped

  pure (resultEnv, LmlBindGroup noExtField Recursive finalBinds)
 where
  extractVarName :: LLmlPat LmlcMr -> Name
  extractVarName (L _ (LmlPatVar (FullName fname) _)) = Name fname
  extractVarName (L _ (LmlPatConstraint _ lPat _)) = extractVarName lPat
  extractVarName _ = error "Recursive bindings must be simple variable patterns"

inferLLmlNonRecBind :: TyEnv -> TyConstrEnv -> LLmlBind LmlcMr -> MonadTypecheck (TyEnv, LLmlBind LmlcTc)
inferLLmlNonRecBind env tyConstrEnv (L loc bind) = over _2 (L loc) <$> inferLmlNonRecBind env tyConstrEnv bind

inferLmlNonRecBind :: TyEnv -> TyConstrEnv -> LmlBind LmlcMr -> MonadTypecheck (TyEnv, LmlBind LmlcTc)
inferLmlNonRecBind env tyConstrEnv (LmlBind _ lPat lExpr) = do
  (patTy, Subst patSubst, lPatTyped) <- inferLLmlPat env tyConstrEnv NonRecursive lPat
  (exprTy, lExprTyped) <- inferLLmlExpr env tyConstrEnv lExpr
  unify exprTy patTy
  subst <- use currentSubst
  let outEnv = TyEnv $ fmap (generalize (apply subst env)) (apply subst patSubst)
  pure (outEnv, LmlBind outEnv (apply subst lPatTyped) (apply subst lExprTyped))

inferLLmlCase :: TyEnv -> TyConstrEnv -> LLmlCase LmlcMr -> MonadTypecheck (Ty, LLmlCase LmlcTc)
inferLLmlCase env tyConstrEnv (L loc case') = over _2 (L loc) <$> inferLmlCase env tyConstrEnv case'

inferLmlCase :: TyEnv -> TyConstrEnv -> LmlCase LmlcMr -> MonadTypecheck (Ty, LmlCase LmlcTc)
inferLmlCase env@(TyEnv tyEnv) tyConstrEnv (LmlCase _ lPat maybeLGuard lExpr) = do
  (patTy, Subst substMap, lPatTyped) <- inferLLmlPat env tyConstrEnv NonRecursive lPat
  currentSubst' <- use currentSubst
  let patEnv = fmap (Forall [] . apply currentSubst') substMap
      envWithPats = TyEnv $ patEnv `HashMap.union` tyEnv
  maybeLGuardTyped <- case maybeLGuard of
    Nothing -> pure Nothing
    Just lGuard -> do
      (guardTy, lGuardTyped) <- inferLLmlExpr envWithPats tyConstrEnv lGuard
      unify guardTy tyBool
      pure $ Just lGuardTyped
  (exprTy, lExprTyped) <- inferLLmlExpr envWithPats tyConstrEnv lExpr
  let outTy = patTy `TArrow` exprTy
  pure (outTy, LmlCase outTy lPatTyped maybeLGuardTyped lExprTyped)
