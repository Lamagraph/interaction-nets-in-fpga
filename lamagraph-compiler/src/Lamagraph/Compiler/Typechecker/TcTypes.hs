{-# LANGUAGE TemplateHaskell #-}

{- | Module with types required for typechecking

__NOTE__: Word \"Type\" is abbreviated to \"Ty\" because of clash with 'Type' from "Data.Kind".
-}
module Lamagraph.Compiler.Typechecker.TcTypes (
  Name (..),
  Ty (..),
  TyScheme (..),
  Subst (..),
  nullSubst,
  TyEnv (..),
  Tys (..),
  TypecheckError (..),
  MonadTypecheckState (..),
  freshCounter,
  currentSubst,
  defaultMonadTypecheckState,
  MonadTypecheck,
  runMonadTypecheck,
) where

import Relude

import Control.Lens
import Data.HashMap.Strict qualified as HashMap
import Data.HashSet qualified as HashSet

import Lamagraph.Compiler.Syntax

-- | Generic name, probably will be used somewhere afterwards
newtype Name = Name Longident deriving (Eq, Ord, Hashable, Show)

-- | Type representation
data Ty
  = -- | Type varible, e.g. @'a@
    TVar Name
  | -- | Type arrow, e.g. @'a -> int@
    TArrow Ty Ty
  | -- Type constructors
    TConstr Name [Ty]
  | -- Type tuple
    TTuple Ty (NonEmpty Ty)
  deriving (Eq, Show)

infixr 9 `TArrow`

-- | Type scheme: list of quantified 'Name's in 'Ty'
data TyScheme = Forall [Name] Ty deriving (Show)

-- | Map describing substitutions
newtype Subst = Subst {unSubst :: HashMap Name Ty} deriving (Show)

nullSubst :: Subst
nullSubst = Subst HashMap.empty

newtype TyEnv = TyEnv (HashMap Name TyScheme) deriving (Show)

-- | Type class with operations on type-related stuff
class Tys t where
  -- | Apply 'Subst'itution to some type
  apply :: Subst -> t -> t

  -- | Get free type variables
  ftv :: t -> HashSet Name

{- | 'Traversable' is much better choice than 'Data.List.List', because of the heavy use of 'NonEmpty' in "Lamagraph.Compiler.Syntax"

@OVERLAPPABLE@ here is required because otherwise there is a conflict between this instance and 'Located' one
-}
instance {-# OVERLAPPABLE #-} (Tys a, Traversable t) => Tys (t a) where
  apply :: Subst -> t a -> t a
  apply subst = fmap (apply subst)
  ftv :: t a -> HashSet Name
  ftv = foldr (\ty acc -> ftv ty `HashSet.union` acc) HashSet.empty

instance Tys Ty where
  apply :: Subst -> Ty -> Ty
  apply subst = \case
    tvar@(TVar name) -> HashMap.findWithDefault tvar name (coerce subst)
    lTy `TArrow` rTy -> apply subst lTy `TArrow` apply subst rTy
    TConstr name tys -> TConstr name (fmap (apply subst) tys)
    TTuple ty tys -> TTuple (apply subst ty) (fmap (apply subst) tys)
  ftv :: Ty -> HashSet Name
  ftv = \case
    TVar name -> HashSet.singleton name
    lTy `TArrow` rTy -> ftv lTy `HashSet.union` ftv rTy
    TConstr _ tys -> ftv tys
    TTuple ty tys -> ftv ty `HashSet.union` ftv tys

instance Tys TyScheme where
  apply :: Subst -> TyScheme -> TyScheme
  apply subst (Forall names ty) = Forall names (apply substOnBoundVars ty)
   where
    substOnBoundVars = Subst $ foldr HashMap.delete (coerce subst) names
  ftv :: TyScheme -> HashSet Name
  ftv (Forall names ty) = ftv ty `HashSet.difference` HashSet.fromList names

instance Tys TyEnv where
  apply :: Subst -> TyEnv -> TyEnv
  apply subst tyEnv = TyEnv $ fmap (apply subst) (coerce tyEnv)
  ftv :: TyEnv -> HashSet Name
  ftv (TyEnv tyEnv) = ftv $ HashMap.elems tyEnv

data TypecheckError
  = UnboundVariable Name
  | ConstructorDoesntExist Name
  | OccursCheck Name Ty
  | CantUnify Ty Ty
  | NonVariableInLetRec
  | VariableClashInPattern Name
  | VarMustOccurOnBothSidesOfOrPattern Name
  deriving (Show, Typeable)
instance Exception TypecheckError

data MonadTypecheckState = MonadTypecheckState {_freshCounter :: Int, _currentSubst :: Subst}

makeLenses 'MonadTypecheckState

defaultMonadTypecheckState :: MonadTypecheckState
defaultMonadTypecheckState =
  MonadTypecheckState
    { _freshCounter = 0
    , _currentSubst = nullSubst
    }

type MonadTypecheck a = ExceptT TypecheckError (State MonadTypecheckState) a

runMonadTypecheck :: MonadTypecheck a -> Either TypecheckError a
runMonadTypecheck f = evalState (runExceptT f) defaultMonadTypecheckState
