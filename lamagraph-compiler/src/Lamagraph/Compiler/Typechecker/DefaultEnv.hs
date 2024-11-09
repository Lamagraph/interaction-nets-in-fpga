module Lamagraph.Compiler.Typechecker.DefaultEnv where

import Relude

import Data.HashMap.Strict qualified as HashMap

import Lamagraph.Compiler.Syntax
import Lamagraph.Compiler.Typechecker.Types

mkTConstr :: Text -> [Ty] -> Ty
mkTConstr name = TConstr (Name $ mkLongident $ pure name)

---------------------
-- Primitive types --
---------------------

tyInt :: Ty
tyInt = mkTConstr "int" []

tyChar :: Ty
tyChar = mkTConstr "char" []

tyString :: Ty
tyString = mkTConstr "string" []

---------------------
-- Algebraic types --
---------------------

tyBool :: Ty
tyBool = mkTConstr "bool" []

tyList :: Ty
tyList = mkTConstr "list" [TVar $ Name $ mkLongident $ pure "a"]

tyOption :: Ty
tyOption = mkTConstr "option" [TVar $ Name $ mkLongident $ pure "a"]

-----------------
-- Environment --
-----------------

defaultEnv :: TyEnv
defaultEnv = TyEnv env
 where
  env =
    HashMap.fromList
      [ (Name $ mkLongident $ pure "+", Forall [] $ tyInt `TArrow` tyInt `TArrow` tyInt)
      , (Name $ mkLongident $ pure "-", Forall [] $ tyInt `TArrow` tyInt `TArrow` tyInt)
      , (Name $ mkLongident $ pure "*", Forall [] $ tyInt `TArrow` tyInt `TArrow` tyInt)
      , (Name $ mkLongident $ pure "/", Forall [] $ tyInt `TArrow` tyInt `TArrow` tyInt)
      , (Name $ mkLongident $ pure ">", Forall [] $ tyInt `TArrow` tyInt `TArrow` tyBool)
      , (Name $ mkLongident $ pure ">=", Forall [] $ tyInt `TArrow` tyInt `TArrow` tyBool)
      , (Name $ mkLongident $ pure "<", Forall [] $ tyInt `TArrow` tyInt `TArrow` tyBool)
      , (Name $ mkLongident $ pure "<=", Forall [] $ tyInt `TArrow` tyInt `TArrow` tyBool)
      , (Name $ mkLongident $ pure "[]", Forall [Name $ mkLongident $ pure "a"] tyList)
      ,
        ( Name $ mkLongident $ pure "::"
        , Forall [Name $ mkLongident $ pure "a"] (TTuple (TVar (Name $ mkLongident $ pure "a")) (pure tyList) `TArrow` tyList)
        )
      , (Name $ mkLongident $ pure "None", Forall [Name $ mkLongident $ pure "a"] tyOption)
      ,
        ( Name $ mkLongident $ pure "Some"
        , Forall [Name $ mkLongident $ pure "a"] (TVar (Name $ mkLongident $ pure "a") `TArrow` tyOption)
        )
      ]
