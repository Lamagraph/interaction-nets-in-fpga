{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Eta reduce" #-}

module Lamagraph.Compiler.Syntax.Longident (Longident (..), mkLongident, LLongident) where

import Relude

import Lamagraph.Compiler.Syntax.Extension

{- | LamagraphML long identifier.

'Text' fragments are comma separated in source code.
-}
newtype Longident = Longident (NonEmpty Text)
  deriving (Show)

mkLongident :: NonEmpty Text -> Longident
mkLongident s = Longident s

type LLongident p = XLocated p Longident
