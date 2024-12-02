-- | Module with type inference function for 'LmlPat'
module Lamagraph.Compiler.Typechecker.Infer.Pat (inferLLmlPat) where

import Relude

import Control.Monad.Except
import Data.HashMap.Strict qualified as HashMap
import Data.HashSet qualified as HashSet
import Data.List.NonEmpty qualified as NE

import Lamagraph.Compiler.Extension
import Lamagraph.Compiler.Parser.SrcLoc
import Lamagraph.Compiler.Syntax
import Lamagraph.Compiler.Typechecker.Helper
import Lamagraph.Compiler.Typechecker.Infer.Lit
import Lamagraph.Compiler.Typechecker.Infer.Type
import Lamagraph.Compiler.Typechecker.TcTypes
import Lamagraph.Compiler.Typechecker.Unification

-- | Returns total 'Ty' of the 'LmlPat' and substitution over all captured variables
inferLLmlPat :: TyEnv -> RecFlag -> LLmlPat LmlcPs -> MonadTypecheck (Ty, Subst)
inferLLmlPat env recFlag (L _ pat) = inferLmlPat env recFlag pat

inferLmlPat :: TyEnv -> RecFlag -> LmlPat LmlcPs -> MonadTypecheck (Ty, Subst)
inferLmlPat env@(TyEnv tyEnv) NonRecursive = \case
  LmlPatAny _ -> do
    tVar <- freshTVar
    pure (tVar, nullSubst)
  LmlPatVar _ (L _ ident) -> do
    tVar <- freshTVar
    let name = Name $ mkLongident $ pure ident
    pure (tVar, Subst $ HashMap.singleton name tVar)
  LmlPatConstant _ lit -> do
    ty <- inferLmlLit lit
    pure (ty, nullSubst)
  LmlPatTuple _ lPat lPats -> do
    (patTy, collectedSubst) <- inferLLmlPat env NonRecursive lPat
    pats <- mapM (inferLLmlPat env NonRecursive) lPats
    let (patTys, collectedSubsts) = NE.unzip pats
    fullCollectedSubst <- foldM (!@@) collectedSubst collectedSubsts
    pure (TTuple patTy patTys, fullCollectedSubst)
  LmlPatConstruct _ (L _ longident) maybeLPat -> case HashMap.lookup (Name longident) tyEnv of
    Nothing -> throwError $ ConstructorDoestExist (coerce longident)
    Just tyScheme -> do
      constrTy <- instantiate tyScheme
      case maybeLPat of
        Nothing -> pure (constrTy, nullSubst)
        Just lPat -> do
          (patTy, collectedSubst) <- inferLLmlPat env NonRecursive lPat
          tVar <- freshTVar
          constrSubst <- mostGeneralUnifier constrTy (patTy `TArrow` tVar)
          pure (apply constrSubst tVar, constrSubst @@ collectedSubst)
  LmlPatOr _ leftLPat rightLPat -> do
    (leftTy, leftSubst@(Subst leftMap)) <- inferLLmlPat env NonRecursive leftLPat
    (rightTy, rightSubst@(Subst rightMap)) <- inferLLmlPat env NonRecursive rightLPat
    let leftKeys = HashMap.keysSet leftMap
        rightKeys = HashMap.keysSet rightMap
        union = leftKeys `HashSet.union` rightKeys
    if union == leftKeys
      then do
        unifierSubst <- mostGeneralUnifier leftTy rightTy
        pure (apply unifierSubst leftTy, unifierSubst @@ rightSubst @@ leftSubst)
      else do
        let difference = leftKeys `HashSet.difference` rightKeys
        case viaNonEmpty head (toList difference) of
          Nothing -> error "Unordered containers internal error!"
          Just name -> throwError $ VarMustOccurOnBothSidesOfOrPattern name
  LmlPatConstraint _ lPat lTy -> do
    ty <- lLmlTypeToTy lTy
    (patTy, patSubst) <- inferLLmlPat env NonRecursive lPat
    unifierSubst <- mostGeneralUnifier ty patTy
    pure (apply unifierSubst ty, unifierSubst @@ patSubst)
inferLmlPat env Recursive = \case
  LmlPatVar _ (L _ ident) -> do
    tVar <- freshTVar
    let name = Name $ mkLongident $ pure ident
    pure (tVar, Subst $ HashMap.singleton name tVar)
  LmlPatConstraint _ lPat lTy -> do
    ty <- lLmlTypeToTy lTy
    (patTy, patSubst) <- inferLLmlPat env Recursive lPat
    unifierSubst <- mostGeneralUnifier ty patTy
    pure (apply unifierSubst ty, unifierSubst @@ patSubst)
  _ -> throwError NonVariableInLetRec
