module Lamagraph.Compiler.MonadFresh (HasFreshCounter (..), MonadFresh (..)) where

import Relude hiding (atomically)

import UnliftIO

class HasFreshCounter a where
  getFreshCounter :: a -> TVar Int

class (Monad m) => MonadFresh m where
  fresh :: m Int

instance (HasFreshCounter env, MonadUnliftIO m) => MonadFresh (ReaderT env m) where
  fresh :: ReaderT env m Int
  fresh = do
    env <- ask
    let counterVar = getFreshCounter env
    atomically $ do
      currentCounter <- readTVar counterVar
      modifyTVar' counterVar (+ 1)
      pure currentCounter
