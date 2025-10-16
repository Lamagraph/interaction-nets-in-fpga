module Lamagraph.Compiler.ModuleResolver (
  parseLmlProgram,
  typecheckLmlProgram,
  LmlProgram (..),
) where

import Data.HashMap.Strict qualified as HashMap
import Lamagraph.Compiler.Extension
import Lamagraph.Compiler.Parser
import Lamagraph.Compiler.Parser.SrcLoc
import Lamagraph.Compiler.Syntax
import Lamagraph.Compiler.Syntax.Longident
import Lamagraph.Compiler.Typechecker.DefaultEnv
import Lamagraph.Compiler.Typechecker.Infer
import Lamagraph.Compiler.Typechecker.TcTypes
import Relude

newtype LmlProgram pass = LmlProgram [LmlModule pass]

getNE :: Longident -> NonEmpty Text
getNE (Longident x) = x

getModuleName :: Maybe (LLongident LmlcTc) -> NonEmpty Text
getModuleName (Just x) = getNE $ unLoc x
getModuleName Nothing = "" :| []

addName :: Maybe (Located Longident) -> Name -> Name
addName x_name (Name (Longident n1)) = Name $ Longident $ getModuleName x_name <> n1

parseLmlProgram :: [Text] -> Either String (LmlProgram LmlcPs)
parseLmlProgram ts = fmap LmlProgram (mapM parseLamagraphML ts)

inferLmlProgram :: TyEnv -> LmlProgram LmlcPs -> MonadTypecheck (LmlProgram LmlcTc)
inferLmlProgram _ (LmlProgram []) = do
  pure $ LmlProgram []
inferLmlProgram env@(TyEnv envMap) (LmlProgram (x : xs)) = do
  lMod <- inferLmlModule env x
  let (LmlModule (TyEnv outEnvMap) xName _pass) = lMod
  let specific = outEnvMap `HashMap.difference` envMap
  let addedModuleNames = HashMap.mapKeys (addName xName) specific
  let resEnv = TyEnv $ HashMap.union addedModuleNames envMap
  let resMod = LmlModule resEnv xName _pass
  (LmlProgram xs') <- inferLmlProgram resEnv (LmlProgram xs)
  pure $ LmlProgram $ resMod : xs'

typecheckLmlProgram :: LmlProgram LmlcPs -> Either TypecheckError (LmlProgram LmlcTc)
typecheckLmlProgram = runMonadTypecheck . inferLmlProgram defaultEnv
