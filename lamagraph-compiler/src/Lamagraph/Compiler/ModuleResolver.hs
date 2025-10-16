module Lamagraph.Compiler.ModuleResolver (resolveModules) where

import Control.Monad.Except
import Data.HashMap.Strict qualified as HashMap
import Lamagraph.Compiler.Extension
import Lamagraph.Compiler.Parser.SrcLoc
import Lamagraph.Compiler.Syntax
import Lamagraph.Compiler.Syntax.Longident
import Lamagraph.Compiler.Typechecker.DefaultEnv
import Lamagraph.Compiler.Typechecker.Infer
import Lamagraph.Compiler.Typechecker.TcTypes
import Relude

-- List of ast ->
-- get first, typecheck, mark with module stuff
-- get its type environment
-- get diff from defaultEnv
-- add moduleName to it
-- add defaultEnv
-- pass as defaultEnv to the second
-- resolveModules :: [LmlModule LmlcPs] -> LmlModule LmlcTc
-- resolveModulesMonadic :: [LmlModule LmlcPs] ->

getNE :: Longident -> NonEmpty Text
getNE (Longident x) = x

getModuleName :: Maybe (LLongident LmlcTc) -> NonEmpty Text
getModuleName (Just x) = getNE $ unLoc x
getModuleName Nothing = "" :| []

addName :: Maybe (Located Longident) -> Name -> Name
addName x_name (Name (Longident n1)) = Name $ Longident $ getModuleName x_name <> n1

resolveModulesMonadic :: TyEnv -> [LmlModule LmlcPs] -> MonadTypecheck (LmlModule LmlcTc)
resolveModulesMonadic (TyEnv envMap) [x] = do
  lMod <- inferLmlModule (TyEnv envMap) x
  let (LmlModule outEnv x_name _pass) = lMod
  let TyEnv outEnvMap = outEnv
  let specific = outEnvMap `HashMap.difference` envMap
  let shared = outEnvMap `HashMap.intersection` envMap
  let addedModuleName = HashMap.mapKeys (addName x_name) specific
  let ending = HashMap.union shared addedModuleName
  pure (LmlModule (TyEnv ending) x_name _pass)
resolveModulesMonadic (TyEnv envMap) (x : xs) = do
  lMod <- inferLmlModule (TyEnv envMap) x
  let (LmlModule outEnv x_name _) = lMod
  let TyEnv outEnvMap = outEnv
  let specific = outEnvMap `HashMap.difference` envMap
  let shared = outEnvMap `HashMap.intersection` envMap
  let addedModuleName = HashMap.mapKeys (addName x_name) specific
  let ending = HashMap.union shared addedModuleName
  resolveModulesMonadic (TyEnv ending) xs
resolveModulesMonadic _ _ = throwError NonVariableInLetRec

resolveModules :: [LmlModule LmlcPs] -> Either TypecheckError (LmlModule LmlcTc)
resolveModules = runMonadTypecheck . resolveModulesMonadic defaultEnv
