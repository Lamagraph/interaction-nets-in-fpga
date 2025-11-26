module Lamagraph.Compiler.ModuleResolver.Resolve.Expr (resolveLLmlBindGroup) where

import Relude

import Control.Lens

import Control.Exception (throw)
import Lamagraph.Compiler.Extension
import Lamagraph.Compiler.ModuleResolver.Helper
import Lamagraph.Compiler.ModuleResolver.Resolve.Lit
import Lamagraph.Compiler.ModuleResolver.Resolve.Pat
import Lamagraph.Compiler.ModuleResolver.Resolve.Type
import Lamagraph.Compiler.ModuleResolver.Types
import Lamagraph.Compiler.Parser.SrcLoc
import Lamagraph.Compiler.Syntax.Expr
import Lamagraph.Compiler.Syntax.Extension

resolveLLmlExpr :: ModuleEnv -> LLmlExpr LmlcPs -> MonadModuleResolver (LLmlExpr LmlcMr)
resolveLLmlExpr env (L loc expr) = L loc <$> resolveLmlExpr env expr

resolveLmlExpr :: ModuleEnv -> LmlExpr LmlcPs -> MonadModuleResolver (LmlExpr LmlcMr)
resolveLmlExpr env = \case
  LmlExprIdent _ longident ->
    case lookupName env longident of
      Nothing -> throw (NameNotFound longident)
      Just (FullName realName) -> pure (LmlExprIdent noExtField realName)
  LmlExprConstant _ lit -> pure (LmlExprConstant noExtField (resolveLmlLit lit))
  LmlExprLet _ lBindGroup lExpr -> do
    (lBindGroupEnv, lBindGroupResolved) <- resolveLLmlBindGroup env lBindGroup
    lExprResolved <- resolveLLmlExpr lBindGroupEnv lExpr
    pure (LmlExprLet noExtField lBindGroupResolved lExprResolved)
  LmlExprFunction _ lPat lExpr -> do
    (lPatEnv, lPatResolved) <- resolveLLmlPat env lPat
    lExprResolved <- resolveLLmlExpr lPatEnv lExpr
    pure (LmlExprFunction noExtField lPatResolved lExprResolved)
  LmlExprApply _ apHead apArgs -> do
    apHeadResolved <- resolveLLmlExpr env apHead
    apArgsResolved <- mapM (resolveLLmlExpr env) apArgs
    pure (LmlExprApply noExtField apHeadResolved apArgsResolved)
  LmlExprMatch _ lExpr lCases -> do
    lExprResolved <- resolveLLmlExpr env lExpr
    lCasesResolved <- mapM (resolveLLmlCase env) lCases
    pure (LmlExprMatch noExtField lExprResolved lCasesResolved)
  LmlExprTuple _ lExpr lExprs -> do
    lExprResolved <- resolveLLmlExpr env lExpr
    lExprsResolved <- mapM (resolveLLmlExpr env) lExprs
    pure (LmlExprTuple noExtField lExprResolved lExprsResolved)
  LmlExprConstruct _ (L loc longident) maybeLExpr -> case lookupName env longident of
    Nothing -> throw (ConstructorNotFound longident)
    Just (FullName realLongident) ->
      case maybeLExpr of
        Nothing ->
          pure (LmlExprConstruct noExtField (L loc realLongident) Nothing)
        Just lExpr -> do
          lExprResolved <- resolveLLmlExpr env lExpr
          pure (LmlExprConstruct noExtField (L loc realLongident) (Just lExprResolved))
  LmlExprIfThenElse _ lCond lTrue lFalse -> do
    lCondResolved <- resolveLLmlExpr env lCond
    lTrueResolved <- resolveLLmlExpr env lTrue
    lFalseResolved <- resolveLLmlExpr env lFalse
    pure (LmlExprIfThenElse noExtField lCondResolved lTrueResolved lFalseResolved)
  LmlExprConstraint _ lExpr lType -> do
    lExprResolved <- resolveLLmlExpr env lExpr
    lTypeResolved <- resolveLLmlType env lType
    pure (LmlExprConstraint noExtField lExprResolved lTypeResolved)

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
  lExprResolved <- resolveLLmlExpr env lExpr
  pure (lPatEnv, LmlBind noExtField lPatResolved lExprResolved)

resolveLLmlRecBind :: ModuleEnv -> LLmlBind LmlcPs -> MonadModuleResolver (ModuleEnv, LLmlBind LmlcMr)
resolveLLmlRecBind env (L loc bind) = over _2 (L loc) <$> resolveLmlRecBind env bind

resolveLmlRecBind :: ModuleEnv -> LmlBind LmlcPs -> MonadModuleResolver (ModuleEnv, LmlBind LmlcMr)
resolveLmlRecBind env (LmlBind _ lPat lExpr) = do
  (lPatEnv, lPatResolved) <- resolveLLmlPat env lPat
  lExprResolved <- resolveLLmlExpr lPatEnv lExpr
  pure (lPatEnv, LmlBind noExtField lPatResolved lExprResolved)

resolveLLmlCase :: ModuleEnv -> LLmlCase LmlcPs -> MonadModuleResolver (LLmlCase LmlcMr)
resolveLLmlCase env (L loc case') = L loc <$> resolveLmlCase env case'

resolveLmlCase :: ModuleEnv -> LmlCase LmlcPs -> MonadModuleResolver (LmlCase LmlcMr)
resolveLmlCase env (LmlCase _ lPat maybeLGuard lExpr) = do
  (lPatEnv, lPatResolved) <- resolveLLmlPat env lPat
  case maybeLGuard of
    Nothing -> do
      lExprResolved <- resolveLLmlExpr lPatEnv lExpr
      pure $ LmlCase noExtField lPatResolved Nothing lExprResolved
    Just lGuard -> do
      lGuardResolved <- resolveLLmlExpr lPatEnv lGuard
      lExprResolved <- resolveLLmlExpr lPatEnv lExpr
      pure $ LmlCase noExtField lPatResolved (Just lGuardResolved) lExprResolved
