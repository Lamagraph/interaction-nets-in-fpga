module Lamagraph.Compiler.Typechecker.DefaultEnv (
  tyInt,
  tyChar,
  tyString,
  tyBool,
  trueConstrName,
  falseConstrName,
  defaultEnv,
  defaultTyConstrEnv,
) where

import Relude

import Data.HashMap.Strict qualified as HashMap

import Lamagraph.Compiler.ModuleResolver.DefaultEnv
import Lamagraph.Compiler.Syntax
import Lamagraph.Compiler.Typechecker.TcTypes

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

tyUnit :: Ty
tyUnit = mkTConstr "()" []

---------------------
-- Algebraic types --
---------------------

tyBool :: Ty
tyBool = mkTConstr "bool" []

trueConstrName :: Name
trueConstrName = Name $ mkLongident $ stdPrefix :| ["true"]

falseConstrName :: Name
falseConstrName = Name $ mkLongident $ stdPrefix :| ["false"]

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
      [ (Name $ mkLongident $ stdPrefix :| ["+"], Forall [] $ tyInt `TArrow` tyInt `TArrow` tyInt)
      , (Name $ mkLongident $ stdPrefix :| ["-"], Forall [] $ tyInt `TArrow` tyInt `TArrow` tyInt)
      , (Name $ mkLongident $ stdPrefix :| ["*"], Forall [] $ tyInt `TArrow` tyInt `TArrow` tyInt)
      , (Name $ mkLongident $ stdPrefix :| ["/"], Forall [] $ tyInt `TArrow` tyInt `TArrow` tyInt)
      , (Name $ mkLongident $ stdPrefix :| [">"], Forall [] $ tyInt `TArrow` tyInt `TArrow` tyBool)
      , (Name $ mkLongident $ stdPrefix :| ["<"], Forall [] $ tyInt `TArrow` tyInt `TArrow` tyBool)
      , (Name $ mkLongident $ stdPrefix :| [">="], Forall [] $ tyInt `TArrow` tyInt `TArrow` tyBool)
      , (Name $ mkLongident $ stdPrefix :| ["<="], Forall [] $ tyInt `TArrow` tyInt `TArrow` tyBool)
      , (Name $ mkLongident $ stdPrefix :| ["[]"], Forall [Name $ mkLongident $ pure "a"] tyList)
      ,
        ( Name $ mkLongident $ stdPrefix :| ["::"]
        , Forall [Name $ mkLongident $ pure "a"] (TTuple (TVar (Name $ mkLongident $ pure "a")) (pure tyList) `TArrow` tyList)
        )
      , (Name $ mkLongident $ stdPrefix :| ["None"], Forall [Name $ mkLongident $ pure "a"] tyOption)
      ,
        ( Name $ mkLongident $ stdPrefix :| ["Some"]
        , Forall [Name $ mkLongident $ pure "a"] (TVar (Name $ mkLongident $ pure "a") `TArrow` tyOption)
        )
      , (trueConstrName, Forall [] tyBool)
      , (falseConstrName, Forall [] tyBool)
      , (Name $ mkLongident $ stdPrefix :| ["~-"], Forall [] $ tyInt `TArrow` tyInt)
      , (Name $ mkLongident $ stdPrefix :| ["print_int"], Forall [] $ tyInt `TArrow` tyUnit)
      ]

defaultTyConstrEnv :: TyConstrEnv
defaultTyConstrEnv = TyConstrEnv tyConstrMap
 where
  tyConstrMap =
    HashMap.fromList
      [ (Name $ mkLongident $ pure "int", DataConstr 0)
      , (Name $ mkLongident $ pure "char", DataConstr 0)
      , (Name $ mkLongident $ pure "string", DataConstr 0)
      , (Name $ mkLongident $ pure "()", DataConstr 0)
      , (Name $ mkLongident $ pure "bool", DataConstr 0)
      , (Name $ mkLongident $ pure "list", DataConstr 1)
      , (Name $ mkLongident $ pure "option", DataConstr 1)
      ]
