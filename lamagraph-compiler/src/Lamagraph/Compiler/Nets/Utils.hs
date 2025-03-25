{-# LANGUAGE RecordWildCards #-}

-- | Module with various utilities for the abstract machine.
module Lamagraph.Compiler.Nets.Utils (
  mkConfigurationWithDefault,
  mkAnnVar,
  annotateTerm,
  unAnnotateTerm,
  memberKeyValue,
  deleteFirstNothing,
  substitute,
  substituteAnnTerm,
) where

import Relude

import Data.List.Extra (delete)
import Data.Map.Strict qualified as Map

import Lamagraph.Compiler.Nets.Types

mkConfigurationWithDefault :: [(AnnTerm label, AnnTerm label)] -> Map Var Var -> [AnnTerm label] -> Configuration label
mkConfigurationWithDefault !stack !phi !iface =
  Configuration
    { heap = Map.empty
    , cycles = Map.empty
    , threadState = Delist
    , ..
    }

mkAnnVar :: Var -> AnnTerm label
mkAnnVar = annotateTerm . Var

annotateTerm :: Term label -> AnnTerm label
annotateTerm = \case
  var@Var{} -> AnnTerm [] var
  agent@Agent{} -> AnnTerm (collectVars agent <> [Nothing]) agent
 where
  collectVars = \case
    Var var -> [Just var]
    Agent _ terms -> concatMap collectVars terms

unAnnotateTerm :: AnnTerm label -> Term label
unAnnotateTerm (AnnTerm _ term) = term

memberKeyValue :: (Ord k, Eq a) => Map k a -> k -> a -> Bool
memberKeyValue map' key value = case Map.lookup key map' of
  Nothing -> False
  Just valueInMap -> value == valueInMap

deleteFirstNothing :: (Eq a) => [Maybe a] -> [Maybe a]
deleteFirstNothing = delete Nothing

substitute :: Var -> Term label -> Term label -> Term label
substitute x u = \case
  Var var -> if var == x then u else Var var
  Agent label terms -> Agent label $ map (substitute x u) terms

substituteAnnTerm :: Var -> AnnTerm label -> AnnTerm label -> AnnTerm label
substituteAnnTerm x u@(AnnTerm uVars uTerm) t@(AnnTerm tVars tTerm) = case tTerm of
  Var var -> if var == x then u else t
  Agent{} ->
    if Just x `elem` tVars
      then AnnTerm (deleteFirstNothing uVars ++ filter (/= Just x) tVars) (substitute x uTerm tTerm)
      else t
