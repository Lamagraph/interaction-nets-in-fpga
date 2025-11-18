{-# LANGUAGE TemplateHaskell #-}

module Lamagraph.Compiler.ModuleResolver.Types (
  FullName (..),
  ModulePath (..),
  ModuleRegistry (..),
  ModuleEnv (..),
  ModuleResolverError (..),
  MonadModuleResolver,
  currentModule,
  moduleRegistry,
  currentNames,
  localNames,
  opens,
  runMonadModuleResolver,
) where

import Relude

import Control.Lens
import Data.HashSet ()

import Lamagraph.Compiler.Syntax.Longident

newtype ModulePath = ModulePath Longident deriving (Eq, Ord, Show, Hashable)

newtype FullName = FullName Longident deriving (Eq, Ord, Show, Hashable)

newtype ModuleRegistry
  = ModuleRegistry
  {mrModules :: HashMap ModulePath (HashSet Text)}
  deriving (Show)

data ModuleEnv
  = ModuleEnv
  { _currentModule :: ModulePath
  , _moduleRegistry :: ModuleRegistry
  , _currentNames :: HashSet Text
  , _localNames :: HashSet Text
  , _opens :: [ModulePath]
  }
  deriving (Show)

makeLenses 'ModuleEnv

data ModuleResolverError
  = NameNotFound Longident
  | ModuleNotFound Longident
  | ConstructorNotFound Longident
  deriving (Show, Typeable)
instance Exception ModuleResolverError

-- State?
data MonadModuleResolverState = MonadModuleResolverState {}

makeLenses 'MonadModuleResolverState

defaultMonadModuleResolverState :: MonadModuleResolverState
defaultMonadModuleResolverState = MonadModuleResolverState{}

type MonadModuleResolver a = State MonadModuleResolverState a

runMonadModuleResolver :: MonadModuleResolver a -> a
runMonadModuleResolver f = evalState f defaultMonadModuleResolverState
