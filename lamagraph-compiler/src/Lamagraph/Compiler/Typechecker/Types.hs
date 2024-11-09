{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -Wno-orphans #-}

{- | Module with types required for typechecking

__NOTE__: Word \"Type\" is abbreviated to \"Ty\" because of clash with 'Type' from "Data.Kind".
-}
module Lamagraph.Compiler.Typechecker.Types where

import Relude

import Control.Lens
import Control.Monad.Except
import Data.HashMap.Strict qualified as HashMap
import Data.HashSet qualified as HashSet
import Data.List.Infinite (Infinite)
import Data.List.Infinite qualified as Inf
import Data.Sequences qualified
import Data.String.Interpolate
import Prettyprinter

import Lamagraph.Compiler.Syntax

-- | Generic name, probably will be used somewhere afterwards
newtype Name = Name Longident deriving (Eq, Ord, Hashable, Show)

data ConcreteType
  = CInt
  | CChar
  | CString
  deriving (Eq, Show)

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

newtype TyEnv = TyEnv (HashMap Name TyScheme) deriving (Show)

-- | Type class with operations on type-related stuff
class Tys t where
  -- | Apply 'Subst'itution to some type
  apply :: Subst -> t -> t

  -- | Get free type variables
  ftv :: t -> HashSet Name

-- | 'Traversable' is much better choice than 'Data.List.List', because of heavy use of 'NonEmpty' in "Lamagraph.Compiler.Syntax"
instance (Tys a, Traversable t) => Tys (t a) where
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
  | ConstructorDoestExist Name
  | OccursCheck Name Ty
  | CantUnify Ty Ty
  | CondNotBool Ty
  | NonVariableInLetRec
  | VariableClashInPattern Name
  | VarMustOccurOnBothSidesOfOrPattern Name
  deriving (Show)

data MonadTypecheckState = MonadTypecheckState {_freshCounter :: Word, _currentSubst :: Subst}

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

---------------------------
-- Helper functions      --
-- To be moved somewhere --
---------------------------

{- | This function generates words @a@, ..., @z@, @aa@, ..., @az@ and so on.

Code somewhat complicated but it is just used to ensure type-safety because it is really an infinite list.
I wish I knew how to write it clearer.
-}
letters :: Infinite Text
letters = Inf.cycle $ fromList [1 ..] >>= flip Data.Sequences.replicateM ('a' :| ['b' .. 'z'])

freshTVar :: MonadTypecheck Ty
freshTVar = do
  count <- use freshCounter
  freshCounter += 1
  pure $ TVar $ Name $ mkLongident $ pure $ letters Inf.!! count

instantiate :: TyScheme -> MonadTypecheck Ty
instantiate (Forall names ty) = do
  newNames <- mapM (const freshTVar) names
  let subst = Subst $ HashMap.fromList $ zip names newNames
  pure $ apply subst ty

generalize :: TyEnv -> Ty -> TyScheme
generalize (TyEnv tyEnv) ty = Forall freeVars ty
 where
  freeVars = toList $ ftv ty `HashSet.difference` ftv tyEnv

nullSubst :: Subst
nullSubst = Subst HashMap.empty

infixr 5 @@

-- | Compose two substitutions, left-biased
(@@) :: Subst -> Subst -> Subst
lSubst@(Subst lMap) @@ (Subst rSubst) = Subst $ fmap (apply lSubst) rSubst `HashMap.union` lMap

infixr 5 !@@

-- | Compose two substitutions, while checking that they are disjunctive
(!@@) :: Subst -> Subst -> MonadTypecheck Subst
lSubst@(Subst lMap) !@@ rSubst@(Subst rMap) = do
  let intersectionMap = HashMap.intersection lMap rMap
  if null intersectionMap
    then pure $ lSubst @@ rSubst
    else case viaNonEmpty head $ HashMap.keys intersectionMap of
      Nothing -> error "Unordered containers internal error!"
      Just name -> throwError $ VariableClashInPattern name

tyEnvUnionDisj :: TyEnv -> TyEnv -> MonadTypecheck TyEnv
tyEnvUnionDisj (TyEnv accTyEnv) (TyEnv tyEnv) = do
  let intersectionMap = HashMap.intersection accTyEnv tyEnv
  if null intersectionMap
    then pure $ TyEnv $ accTyEnv `HashMap.union` tyEnv
    else case viaNonEmpty head $ HashMap.keys intersectionMap of
      Nothing -> error "Unordered containers internal error!"
      Just name -> throwError $ VariableClashInPattern name

--------------------
-- Pretty printer --
--------------------

instance Pretty (Either TypecheckError TyEnv) where
  pretty :: Either TypecheckError TyEnv -> Doc ann
  pretty = \case
    Left err -> pretty err
    Right tyEnv -> pretty tyEnv

instance Pretty TypecheckError where
  pretty :: TypecheckError -> Doc ann
  pretty = \case
    UnboundVariable name -> "Error: variable" <+> pretty name <+> "is unbound"
    ConstructorDoestExist name -> [i|Error: constructor #{pretty name} isn't declared|]
    OccursCheck name ty -> "Error: variable" <+> pretty name <+> "already present in type" <+> pretty ty
    CantUnify lTy rTy -> "Error: cannot unify" <+> pretty lTy <+> "and" <+> pretty rTy
    CondNotBool ty -> [i|Error: expected: bool, got #{pretty ty} |]
    NonVariableInLetRec -> "Error: non variable pattern in let rec"
    VariableClashInPattern name -> [i|Error: variable #{pretty name} is already bound in this pattern|]
    VarMustOccurOnBothSidesOfOrPattern name -> [i|Error: variable #{pretty name} must occur on both sides of the or-pattern|]

instance Pretty TyEnv where
  pretty :: TyEnv -> Doc ann
  pretty (TyEnv tyEnv) = vsep $ fmap pretty (HashMap.toList tyEnv)

instance {-# OVERLAPS #-} Pretty (Name, TyScheme) where
  pretty :: (Name, TyScheme) -> Doc ann
  pretty (name, tyScheme) = pretty name <> ":" <+> pretty tyScheme

instance Pretty TyScheme where
  pretty :: TyScheme -> Doc ann
  pretty (Forall [] ty) = pretty ty
  pretty (Forall names ty) = "forall" <+> hsep (fmap pretty names) <> "." <+> pretty ty

instance Pretty Name where
  pretty :: Name -> Doc ann
  pretty (Name longident) = pretty longident

instance Pretty Longident where
  pretty :: Longident -> Doc ann
  pretty (Longident idents) = hsep $ punctuate comma (map pretty (toList idents))

instance Pretty Ty where
  pretty :: Ty -> Doc ann
  pretty = \case
    TVar var -> "'" <> pretty var
    lTy@(_ `TArrow` _) `TArrow` rTy -> parens (pretty lTy) <+> "->" <+> pretty rTy
    lTy `TArrow` rTy -> pretty lTy <+> "->" <+> pretty rTy
    TConstr tyConstr [] -> pretty tyConstr
    TConstr tyConstr [ty] -> pretty ty <+> pretty tyConstr
    TConstr tyConstr tys -> parens (fillSep $ punctuate comma (map pretty tys)) <+> pretty tyConstr
    TTuple ty tys -> parens $ concatWith (surround " * ") (map pretty (ty : toList tys))

-------------------------------
-- Unification related stuff --
-------------------------------

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
