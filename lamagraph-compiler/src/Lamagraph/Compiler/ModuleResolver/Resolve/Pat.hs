module Lamagraph.Compiler.ModuleResolver.Resolve.Pat (resolveLLmlPat) where

import Relude

import Control.Lens
import Control.Monad.Except
import Data.HashSet qualified as HashSet

import Lamagraph.Compiler.Extension
import Lamagraph.Compiler.ModuleResolver.Helper
import Lamagraph.Compiler.ModuleResolver.Resolve.Lit
import Lamagraph.Compiler.ModuleResolver.Resolve.Type
import Lamagraph.Compiler.ModuleResolver.Types
import Lamagraph.Compiler.Parser.SrcLoc
import Lamagraph.Compiler.Syntax.Extension
import Lamagraph.Compiler.Syntax.Pat

resolveLLmlPat :: ModuleEnv -> LLmlPat LmlcPs -> MonadModuleResolver (ModuleEnv, LLmlPat LmlcMr)
resolveLLmlPat env (L loc pat) = over _2 (L loc) <$> resolveLmlPat env pat

resolveLmlPat :: ModuleEnv -> LmlPat LmlcPs -> MonadModuleResolver (ModuleEnv, LmlPat LmlcMr)
resolveLmlPat env = \case
  LmlPatAny _ -> pure (env, LmlPatAny noExtField)
  LmlPatVar _ lIdent@(L _ ident) ->
    let fullName = prependModuleName (env ^. currentModule) ident
     in pure (over localNames (HashSet.insert ident) env, LmlPatVar fullName lIdent)
  LmlPatConstant _ lit -> pure (env, LmlPatConstant noExtField (resolveLmlLit lit))
  LmlPatTuple _ lPat lPats -> do
    (patEnv, lPatResolved) <- resolveLLmlPat env lPat
    (finalEnv, resolvedList) <- resolveMany patEnv resolveLLmlPat lPats
    let lPatsResolved = fromList resolvedList
    pure (finalEnv, LmlPatTuple noExtField lPatResolved lPatsResolved)
  LmlPatConstruct _ (L loc longident) maybeLPat ->
    case lookupName env longident of
      Nothing -> throwError (ConstructorNotFound longident)
      Just (FullName realLongident) ->
        case maybeLPat of
          Nothing -> pure (env, LmlPatConstruct noExtField (L loc realLongident) Nothing)
          Just lPat -> do
            (finalEnv, lPatResolved) <- resolveLLmlPat env lPat
            pure (finalEnv, LmlPatConstruct noExtField (L loc realLongident) (Just lPatResolved))
  LmlPatOr _ leftLPat rightLPat -> do
    (leftEnv, leftLPatResolved) <- resolveLLmlPat env leftLPat
    (rightEnv, rightLPatResolved) <- resolveLLmlPat leftEnv rightLPat
    pure (rightEnv, LmlPatOr noExtField leftLPatResolved rightLPatResolved)
  LmlPatConstraint _ lPat lTy -> do
    (finalEnv, lPatResolved) <- resolveLLmlPat env lPat
    lTyResolved <- resolveLLmlType finalEnv lTy
    pure (finalEnv, LmlPatConstraint noExtField lPatResolved lTyResolved)
