{-# LANGUAGE TemplateHaskell #-}

module Lamagraph.Compiler.ModuleResolver.MrTypes where

import Relude

import Control.Lens
import Data.HashMap.Strict qualified as HashMap
import Data.HashSet ()
import Data.List.NonEmpty as NonEmpty

import Lamagraph.Compiler.Syntax.Longident

-- module X.Y.Z
newtype ModulePath = ModulePath Longident deriving (Eq, Ord, Show, Hashable)

newtype FullName = FullName Longident deriving (Eq, Ord, Show, Hashable)

data QualifiedName
  = QualifiedName
  { qnModule :: ModulePath
  , qnName :: Text
  }

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
  = NameNotFound
  | ModuleNotFound
  | ConstructorNotFound
  deriving (Show, Typeable)
instance Exception ModuleResolverError

data MonadModuleResolverState = MonadModuleResolverState {}

makeLenses 'MonadModuleResolverState

defaultMonadModuleResolverState :: MonadModuleResolverState
defaultMonadModuleResolverState = MonadModuleResolverState{}

type MonadModuleResolver a = ExceptT ModuleResolverError (State MonadModuleResolverState) a

runMonadModuleResolver :: MonadModuleResolver a -> Either ModuleResolverError a
runMonadModuleResolver f = evalState (runExceptT f) defaultMonadModuleResolverState
