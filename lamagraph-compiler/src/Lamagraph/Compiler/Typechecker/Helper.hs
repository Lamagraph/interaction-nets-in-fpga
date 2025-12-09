module Lamagraph.Compiler.Typechecker.Helper (
  freshTVar,
  instantiate,
  generalize,
  (@@),
  (!@@),
  tyEnvUnionDisj,
  unzip3F,
) where

import Relude
import Relude.Unsafe ((!!))

import Control.Lens
import Control.Monad.Except
import Data.HashMap.Strict qualified as HashMap
import Data.HashSet qualified as HashSet
import Data.Tuple.Extra

import Lamagraph.Compiler.Syntax
import Lamagraph.Compiler.Typechecker.TcTypes
import Lamagraph.Compiler.Utils (letters)

freshTVar :: MonadTypecheck Ty
freshTVar = do
  count <- use freshCounter
  freshCounter += 1
  pure $ TVar $ Name $ mkLongident $ pure $ letters !! count

instantiate :: TyScheme -> MonadTypecheck Ty
instantiate (Forall names ty) = do
  newNames <- mapM (const freshTVar) names
  let subst = Subst $ HashMap.fromList $ zip names newNames
  pure $ apply subst ty

generalize :: TyEnv -> Ty -> TyScheme
generalize (TyEnv tyEnv) ty = Forall freeVars ty
 where
  freeVars = toList $ ftv ty `HashSet.difference` ftv tyEnv

infixr 5 @@

-- | Compose two substitutions, left-biased
(@@) :: Subst -> Subst -> Subst
lSubst@(Subst lMap) @@ (Subst rSubst) = Subst $ fmap (apply lSubst) rSubst `HashMap.union` lMap

infixr 5 !@@

-- | Compose two substitutions, while checking that they are disjunctive
(!@@) :: Subst -> Subst -> MonadTypecheck Subst
lSubst@(Subst lMap) !@@ rSubst@(Subst rMap) = do
  let intersectionMap = HashMap.intersection lMap rMap
  case viaNonEmpty head $ HashMap.keys intersectionMap of
    Nothing -> pure $ lSubst @@ rSubst
    Just name -> throwError $ VariableClashInPattern name

tyEnvUnionDisj :: TyEnv -> TyEnv -> MonadTypecheck TyEnv
tyEnvUnionDisj (TyEnv accTyEnv) (TyEnv tyEnv) = do
  let intersectionMap = HashMap.intersection accTyEnv tyEnv
  case viaNonEmpty head $ HashMap.keys intersectionMap of
    Nothing -> pure $ TyEnv $ accTyEnv `HashMap.union` tyEnv
    Just name -> throwError $ VariableClashInPattern name

unzip3F :: (Functor f) => f (a, b, c) -> (f a, f b, f c)
unzip3F f = (fst3 <$> f, snd3 <$> f, thd3 <$> f)
