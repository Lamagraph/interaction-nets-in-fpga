{-# LANGUAGE TemplateHaskell #-}

module Lamagraph.Compiler.Core.MonadDesugar (MonadDesugar, runMonadDesugar, freshVar) where

import Relude
import Relude.Unsafe ((!!))

import Control.Lens
import Data.Sequences qualified

import Lamagraph.Compiler.Core
import Lamagraph.Compiler.Syntax
import Lamagraph.Compiler.Typechecker.TcTypes

newtype MonadDesugarState = MonadDesugarState {_freshDsCounter :: Int}

makeLenses 'MonadDesugarState

defaultMonadDesugarState :: MonadDesugarState
defaultMonadDesugarState =
  MonadDesugarState
    { _freshDsCounter = 0
    }

data DesugarError

type MonadDesugar a = ExceptT DesugarError (State MonadDesugarState) a

runMonadDesugar :: MonadDesugar a -> Either DesugarError a
runMonadDesugar f = evalState (runExceptT f) defaultMonadDesugarState

-- FIXME: Copied from Typechecker

-- | This function generates words @a@, ..., @z@, @aa@, ..., @az@ and so on.
letters :: [Text]
letters = [1 ..] >>= flip Data.Sequences.replicateM ['a' .. 'z']

freshVar :: MonadDesugar Var
freshVar = do
  count <- use freshDsCounter
  freshDsCounter += 1
  pure $ Id $ Name $ mkLongident $ pure $ "t#" <> letters !! count
