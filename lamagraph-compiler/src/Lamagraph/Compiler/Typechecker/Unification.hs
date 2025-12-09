-- | Unification related stuff
module Lamagraph.Compiler.Typechecker.Unification (mostGeneralUnifier, unify) where

import Relude

import Control.Lens
import Control.Monad.Except
import Data.HashMap.Strict qualified as HashMap
import Data.HashSet qualified as HashSet

import Lamagraph.Compiler.PrettyAst ()
import Lamagraph.Compiler.Typechecker.Helper ((@@))
import Lamagraph.Compiler.Typechecker.TcTypes

occursCheck :: (Tys a) => Name -> a -> Bool
occursCheck name t = name `HashSet.member` ftv t

bindTVar :: Name -> Ty -> MonadTypecheck Subst
bindTVar name ty
  | ty == TVar name = pure nullSubst
  | occursCheck name ty = throwError $ OccursCheck name ty
  | otherwise = pure $ Subst $ HashMap.singleton name ty

mostGeneralUnifier :: Ty -> Ty -> MonadTypecheck Subst
mostGeneralUnifier (l1 `TArrow` r1) (l2 `TArrow` r2) = do
  lSubst <- mostGeneralUnifier l1 l2
  rSubst <- mostGeneralUnifier (apply lSubst r1) (apply lSubst r2)
  pure $ rSubst @@ lSubst
mostGeneralUnifier (TVar name) rTy = bindTVar name rTy
mostGeneralUnifier lTy (TVar name) = bindTVar name lTy
mostGeneralUnifier l@(TConstr lName lTy) r@(TConstr rName rTy)
  | lName == rName = do
      substs <- zipWithM mostGeneralUnifier lTy rTy
      let composedSubst = foldr (@@) nullSubst substs
      pure composedSubst
  | otherwise = throwError $ CantUnify l r
mostGeneralUnifier (TTuple lTy lTys) (TTuple rTy rTys) = do
  subst <- mostGeneralUnifier lTy rTy
  substs <- zipWithM mostGeneralUnifier (toList lTys) (toList rTys)
  let composedSubst = foldr (@@) subst substs
  pure composedSubst
mostGeneralUnifier lTy rTy
  | lTy == rTy = pure nullSubst
  | otherwise = throwError $ CantUnify lTy rTy

unify :: Ty -> Ty -> MonadTypecheck ()
unify lTy rTy = do
  subst <- use currentSubst
  unifier <- mostGeneralUnifier (apply subst lTy) (apply subst rTy)
  currentSubst .= unifier @@ subst
