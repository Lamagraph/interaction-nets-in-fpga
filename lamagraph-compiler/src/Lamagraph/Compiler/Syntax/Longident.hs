module Lamagraph.Compiler.Syntax.Longident (Longident (..), mkLongident, LLongident) where

import Relude

import Lamagraph.Compiler.Syntax.Extension

{- | LamagraphML long identifier.

'Text' fragments are dot separated in the source code.
-}
newtype Longident = Longident (NonEmpty Text)
  deriving (Show, Eq)

mkLongident :: NonEmpty Text -> Longident
mkLongident = Longident

type LLongident pass = XLocated pass Longident
