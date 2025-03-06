{-# LANGUAGE TemplateHaskell #-}

module Lamagraph.Compiler.Core.MonadDesugar (MonadDesugar, runMonadDesugar, freshVar) where

import Relude
import Relude.Unsafe ((!!))

import Control.Lens

import Lamagraph.Compiler.Core
import Lamagraph.Compiler.Syntax
import Lamagraph.Compiler.Typechecker.TcTypes
import Lamagraph.Compiler.Utils

newtype MonadDesugarState = MonadDesugarState {_freshDsCounter :: Int}

makeLenses 'MonadDesugarState

defaultMonadDesugarState :: MonadDesugarState
defaultMonadDesugarState =
  MonadDesugarState
    { _freshDsCounter = 0
    }

type MonadDesugar a = State MonadDesugarState a

runMonadDesugar :: MonadDesugar a -> a
runMonadDesugar f = evalState f defaultMonadDesugarState

freshVar :: MonadDesugar Var
freshVar = do
  count <- use freshDsCounter
  freshDsCounter += 1
  pure $ Id $ Name $ mkLongident $ pure $ "t#" <> letters !! count
