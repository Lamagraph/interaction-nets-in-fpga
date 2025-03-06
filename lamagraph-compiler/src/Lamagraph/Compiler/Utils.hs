module Lamagraph.Compiler.Utils (letters) where

import Relude

import Data.Sequences qualified

{- | This function generates words @a@, ..., @z@, @aa@, ..., @az@ and so on.

Currently used in "Lamagraph.Compiler.Core.MonadDesugar" and "Lamagraph.Compiler.Typechecker.Helper"
to generate fresh variable names.
-}
letters :: [Text]
letters = [1 ..] >>= flip Data.Sequences.replicateM ['a' .. 'z']
