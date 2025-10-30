module Lamagraph.Compiler.ModuleResolver.Resolve.Expr (resolveLLmlBindGroup) where

import Relude

import Control.Lens
import Control.Monad.Except

import Lamagraph.Compiler.Extension
import Lamagraph.Compiler.ModuleResolver.Helper
import Lamagraph.Compiler.ModuleResolver.MrTypes
import Lamagraph.Compiler.ModuleResolver.Resolve.Lit
import Lamagraph.Compiler.ModuleResolver.Resolve.Pat
import Lamagraph.Compiler.ModuleResolver.Resolve.Type
import Lamagraph.Compiler.Parser.SrcLoc
import Lamagraph.Compiler.Syntax.Expr
import Lamagraph.Compiler.Syntax.Extension

resolveLLmlExpr :: ModuleEnv -> LLmlExpr LmlcPs -> MonadModuleResolver (ModuleEnv, LLmlExpr LmlcMr)
resolveLLmlExpr env (L loc expr) = over _2 (L loc) <$> resolveLmlExpr env expr

resolveLmlExpr :: ModuleEnv -> LmlExpr LmlcPs -> MonadModuleResolver (ModuleEnv, LmlExpr LmlcMr)
resolveLmlExpr env = \case
  LmlExprIdent _ longident ->
    case lookupName env longident of
      Nothing -> throwError (NameNotFound longident)
      Just (FullName realName) -> pure (env, LmlExprIdent noExtField realName)
  LmlExprConstant _ lit -> pure (env, LmlExprConstant noExtField (resolveLmlLit lit))
  LmlExprLet _ lBindGroup lExpr -> do
    (lBindGroupEnv, lBindGroupResolved) <- resolveLLmlBindGroup env lBindGroup
    (_, lExprResolved) <- resolveLLmlExpr lBindGroupEnv lExpr
    pure (env, LmlExprLet noExtField lBindGroupResolved lExprResolved)
  LmlExprFunction _ lPat lExpr -> do
    (lPatEnv, lPatResolved) <- resolveLLmlPat env lPat
    (lExprEnv, lExprResolved) <- resolveLLmlExpr lPatEnv lExpr
    pure (lExprEnv, LmlExprFunction noExtField lPatResolved lExprResolved)
  LmlExprApply _ apHead apArgs -> do
    (apHeadEnv, apHeadResolved) <- resolveLLmlExpr env apHead
    (_, apArgsResolvedList) <- resolveMany apHeadEnv resolveLLmlExpr apArgs
    let apArgsResolved = fromList apArgsResolvedList
    pure (env, LmlExprApply noExtField apHeadResolved apArgsResolved)
  LmlExprMatch _ lExpr lCases -> do
    (lExprEnv, lExprResolved) <- resolveLLmlExpr env lExpr
    (_, lCasesResolvedList) <- resolveMany lExprEnv resolveLLmlCase lCases
    let lCasesResolved = fromList lCasesResolvedList
    pure (env, LmlExprMatch noExtField lExprResolved lCasesResolved)
  LmlExprTuple _ lExpr lExprs -> do
    (lExprEnv, lExprResolved) <- resolveLLmlExpr env lExpr
    (_, lExprsResolvedList) <- resolveMany lExprEnv resolveLLmlExpr lExprs
    let lExprsResolved = fromList lExprsResolvedList
    pure (env, LmlExprTuple noExtField lExprResolved lExprsResolved)
  LmlExprConstruct _ (L loc longident) maybeLExpr -> case lookupName env longident of
    Nothing -> throwError (ConstructorNotFound longident)
    Just (FullName realLongident) ->
      case maybeLExpr of
        Nothing ->
          pure (env, LmlExprConstruct noExtField (L loc realLongident) Nothing)
        Just lExpr -> do
          (_, lExprResolved) <- resolveLLmlExpr env lExpr
          pure (env, LmlExprConstruct noExtField (L loc realLongident) (Just lExprResolved))
  LmlExprIfThenElse _ lCond lTrue lFalse -> do
    (_, lCondResolved) <- resolveLLmlExpr env lCond
    (_, lTrueResolved) <- resolveLLmlExpr env lTrue
    (_, lFalseResolved) <- resolveLLmlExpr env lFalse
    pure (env, LmlExprIfThenElse noExtField lCondResolved lTrueResolved lFalseResolved)
  LmlExprConstraint _ lExpr lType -> do
    (_, lExprResolved) <- resolveLLmlExpr env lExpr
    lTypeResolved <- resolveLLmlType env lType
    pure (env, LmlExprConstraint noExtField lExprResolved lTypeResolved)

resolveLLmlBindGroup :: ModuleEnv -> LLmlBindGroup LmlcPs -> MonadModuleResolver (ModuleEnv, LLmlBindGroup LmlcMr)
resolveLLmlBindGroup env (L loc bg) = over _2 (L loc) <$> resolveLmlBindGroup env bg

resolveLmlBindGroup :: ModuleEnv -> LmlBindGroup LmlcPs -> MonadModuleResolver (ModuleEnv, LmlBindGroup LmlcMr)
resolveLmlBindGroup env (LmlBindGroup _ NonRecursive binds) = do
  (newEnv, bindsResolvedList) <- resolveMany env resolveLLmlNonRecBind binds
  let bindsResolved = fromList bindsResolvedList
  pure
    ( newEnv
    , LmlBindGroup
        NoExtField
        NonRecursive
        bindsResolved
    )
resolveLmlBindGroup env (LmlBindGroup _ Recursive binds) = do
  (patsEnv, _) <- resolveMany env (\env' (L _ (LmlBind _ lPat _)) -> resolveLLmlPat env' lPat) binds
  (newEnv, bindsResolvedList) <- resolveMany patsEnv resolveLLmlRecBind binds
  let bindsResolved = fromList bindsResolvedList
  pure
    ( newEnv
    , LmlBindGroup
        NoExtField
        Recursive
        bindsResolved
    )

resolveLLmlNonRecBind :: ModuleEnv -> LLmlBind LmlcPs -> MonadModuleResolver (ModuleEnv, LLmlBind LmlcMr)
resolveLLmlNonRecBind env (L loc bind) = over _2 (L loc) <$> resolveLmlNonRecBind env bind

resolveLmlNonRecBind :: ModuleEnv -> LmlBind LmlcPs -> MonadModuleResolver (ModuleEnv, LmlBind LmlcMr)
resolveLmlNonRecBind env (LmlBind _ lPat lExpr) = do
  (lPatEnv, lPatResolved) <- resolveLLmlPat env lPat
  (_, lExprResolved) <- resolveLLmlExpr env lExpr
  pure (lPatEnv, LmlBind noExtField lPatResolved lExprResolved)

resolveLLmlRecBind :: ModuleEnv -> LLmlBind LmlcPs -> MonadModuleResolver (ModuleEnv, LLmlBind LmlcMr)
resolveLLmlRecBind env (L loc bind) = over _2 (L loc) <$> resolveLmlRecBind env bind

resolveLmlRecBind :: ModuleEnv -> LmlBind LmlcPs -> MonadModuleResolver (ModuleEnv, LmlBind LmlcMr)
resolveLmlRecBind env (LmlBind _ lPat lExpr) = do
  (lPatEnv, lPatResolved) <- resolveLLmlPat env lPat
  (_, lExprResolved) <- resolveLLmlExpr lPatEnv lExpr
  pure (lPatEnv, LmlBind noExtField lPatResolved lExprResolved)

resolveLLmlCase :: ModuleEnv -> LLmlCase LmlcPs -> MonadModuleResolver (ModuleEnv, LLmlCase LmlcMr)
resolveLLmlCase env (L loc case') = over _2 (L loc) <$> resolveLmlCase env case'

resolveLmlCase :: ModuleEnv -> LmlCase LmlcPs -> MonadModuleResolver (ModuleEnv, LmlCase LmlcMr)
resolveLmlCase env (LmlCase _ lPat maybeLGuard lExpr) = do
  (lPatEnv, lPatResolved) <- resolveLLmlPat env lPat
  case maybeLGuard of
    Nothing -> do
      (_, lExprResolved) <- resolveLLmlExpr lPatEnv lExpr
      pure (env, LmlCase noExtField lPatResolved Nothing lExprResolved)
    Just lGuard -> do
      (_, lGuardResolved) <- resolveLLmlExpr lPatEnv lGuard
      (_, lExprResolved) <- resolveLLmlExpr lPatEnv lExpr
      pure (env, LmlCase noExtField lPatResolved (Just lGuardResolved) lExprResolved)
