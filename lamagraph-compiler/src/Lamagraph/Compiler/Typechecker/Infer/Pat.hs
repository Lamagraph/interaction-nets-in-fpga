-- | Module with type inference function for 'LmlPat'
module Lamagraph.Compiler.Typechecker.Infer.Pat (inferLLmlPat) where

import Relude

import Control.Lens
import Control.Monad
import Control.Monad.Except
import Data.HashMap.Strict qualified as HashMap
import Data.HashSet qualified as HashSet

import Lamagraph.Compiler.Extension
import Lamagraph.Compiler.ModuleResolver.Types
import Lamagraph.Compiler.Parser.SrcLoc
import Lamagraph.Compiler.Syntax
import Lamagraph.Compiler.Typechecker.Helper
import Lamagraph.Compiler.Typechecker.Infer.Lit
import Lamagraph.Compiler.Typechecker.Infer.Type
import Lamagraph.Compiler.Typechecker.TcTypes
import Lamagraph.Compiler.Typechecker.Unification

-- | Returns total 'Ty' of the 'LmlPat' and substitution over all captured variables
inferLLmlPat :: TyEnv -> TyConstrEnv -> RecFlag -> LLmlPat LmlcMr -> MonadTypecheck (Ty, Subst, LLmlPat LmlcTc)
inferLLmlPat env tyConstrEnv recFlag (L loc pat) = over _3 (L loc) <$> inferLmlPat env tyConstrEnv recFlag pat

inferLmlPat :: TyEnv -> TyConstrEnv -> RecFlag -> LmlPat LmlcMr -> MonadTypecheck (Ty, Subst, LmlPat LmlcTc)
inferLmlPat env@(TyEnv tyEnv) tyConstrEnv NonRecursive = \case
  LmlPatAny _ -> do
    tVar <- freshTVar
    pure (tVar, nullSubst, LmlPatAny tVar)
  LmlPatVar fname@(FullName n) lIdent -> do
    tVar <- freshTVar
    let name = Name n
    pure (tVar, Subst $ HashMap.singleton name tVar, LmlPatVar (fname, tVar) lIdent)
  LmlPatConstant _ lit -> do
    (ty, lLitTyped) <- inferLmlLit lit
    pure (ty, nullSubst, LmlPatConstant ty lLitTyped)
  LmlPatTuple _ lPat lPats -> do
    (patTy, collectedSubst, lPatTyped) <- inferLLmlPat env tyConstrEnv NonRecursive lPat
    pats <- mapM (inferLLmlPat env tyConstrEnv NonRecursive) lPats
    let (patTys, collectedSubsts, lPatsTyped) = unzip3F pats
    fullCollectedSubst <- foldM (!@@) collectedSubst collectedSubsts
    let patTyFinal = apply fullCollectedSubst patTy
        patTysFinal = fmap (apply fullCollectedSubst) patTys
        outType = TTuple patTyFinal patTysFinal
    pure (outType, fullCollectedSubst, LmlPatTuple outType lPatTyped lPatsTyped)
  LmlPatConstruct _ lLongident@(L _ longident) maybeLPat -> case HashMap.lookup (Name longident) tyEnv of
    Nothing -> throwError $ ConstructorDoesntExist (coerce longident)
    Just tyScheme -> do
      constrTy <- instantiate tyScheme
      case maybeLPat of
        Nothing -> pure (constrTy, nullSubst, LmlPatConstruct constrTy lLongident Nothing)
        Just lPat -> do
          (patTy, collectedSubst, lPatTyped) <- inferLLmlPat env tyConstrEnv NonRecursive lPat
          tVar <- freshTVar
          constrSubst <- mostGeneralUnifier constrTy (patTy `TArrow` tVar)
          let outType = apply constrSubst tVar
              finalSubst = constrSubst @@ collectedSubst
          pure (outType, finalSubst, LmlPatConstruct outType lLongident (Just lPatTyped))
  LmlPatOr _ leftLPat rightLPat -> do
    (leftTy, leftSubst@(Subst leftMap), leftLPatTyped) <- inferLLmlPat env tyConstrEnv NonRecursive leftLPat
    (rightTy, rightSubst@(Subst rightMap), rightLPatTyped) <- inferLLmlPat env tyConstrEnv NonRecursive rightLPat
    let leftKeys = HashMap.keysSet leftMap
        rightKeys = HashMap.keysSet rightMap
        union = leftKeys `HashSet.union` rightKeys
        intersection = leftKeys `HashSet.intersection` rightKeys
        symmetricDifference = union `HashSet.difference` intersection

    -- Both sides must bind same identifiers with the same type,
    -- thus symmetric difference must be empty.
    case viaNonEmpty head (toList symmetricDifference) of
      Nothing -> do
        unifierSubst <- mostGeneralUnifier leftTy rightTy
        let outTy = apply unifierSubst leftTy
        pure (outTy, unifierSubst @@ rightSubst @@ leftSubst, LmlPatOr outTy leftLPatTyped rightLPatTyped)
      Just name -> throwError $ VarMustOccurOnBothSidesOfOrPattern name
  LmlPatConstraint _ lPat lTy -> do
    (ty, lTyTyped) <- lLmlTypeToTy tyConstrEnv lTy
    (patTy, patSubst, lPatTyped) <- inferLLmlPat env tyConstrEnv NonRecursive lPat
    unifierSubst <- mostGeneralUnifier ty patTy
    let outTy = apply unifierSubst ty
    pure (outTy, unifierSubst @@ patSubst, LmlPatConstraint outTy lPatTyped lTyTyped)
inferLmlPat env tyConstrEnv Recursive = \case
  LmlPatVar fname@(FullName n) lIdent -> do
    tVar <- freshTVar
    let name = Name n
    pure (tVar, Subst $ HashMap.singleton name tVar, LmlPatVar (fname, tVar) lIdent)
  LmlPatConstraint _ lPat lTy -> do
    (ty, lTyTyped) <- lLmlTypeToTy tyConstrEnv lTy
    (patTy, patSubst, lPatTyped) <- inferLLmlPat env tyConstrEnv Recursive lPat
    unifierSubst <- mostGeneralUnifier ty patTy
    let outTy = apply unifierSubst ty
    pure (outTy, unifierSubst @@ patSubst, LmlPatConstraint outTy lPatTyped lTyTyped)
  _ -> throwError NonVariableInLetRec
