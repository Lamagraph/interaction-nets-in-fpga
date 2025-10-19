module Lamagraph.Compiler.ModuleResolver (
  parseLmlProgram,
  typecheckLmlProgram,
  LmlProgram (..),
  evalLmlProgramDefEnv,
  desugarLmlProgram,
) where

import Data.HashMap.Strict qualified as HashMap
import Lamagraph.Compiler.Core
import Lamagraph.Compiler.Core.LmlToCore
import Lamagraph.Compiler.Core.MonadDesugar
import Lamagraph.Compiler.Eval
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

desugarLmlProgram :: LmlProgram LmlcTc -> MonadDesugar [(Name, [CoreBind])]
desugarLmlProgram (LmlProgram []) = pure []
desugarLmlProgram (LmlProgram (x@(LmlModule _ x_name _) : xs)) = do
  let name = Name $ Longident $ getModuleName x_name
  binds <- desugarLmlModule x
  xs' <- desugarLmlProgram (LmlProgram xs)
  pure ((name, binds) : xs')

addNameVar :: Name -> Var -> Var
addNameVar (Name (Longident x_name)) (Id (Name (Longident n1))) = Id $ Name $ Longident $ x_name <> n1

evalLmlProgram :: (MonadEval m) => EvalEnv -> [(Name, [CoreBind])] -> m EvalEnv
evalLmlProgram env@(EvalEnv envMap) (x@(x_name, binds) : xs) = do
  outEnv@(EvalEnv outEnvMap) <- evalCoreBinds env binds
  let specific = outEnvMap `HashMap.difference` envMap
  let addedModuleNames = HashMap.mapKeys (addNameVar x_name) specific
  let resEnv = EvalEnv $ HashMap.union addedModuleNames envMap
  evalLmlProgram resEnv xs
evalLmlProgram env [] = do
  pure env

evalLmlProgramDefEnv :: (MonadEval m) => [(Name, [CoreBind])] -> m EvalEnv
evalLmlProgramDefEnv = evalLmlProgram defEvalEnv
