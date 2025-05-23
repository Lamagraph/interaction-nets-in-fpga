-- | LamagraphML long identifiers
module Lamagraph.Compiler.Syntax.Longident (Longident (..), mkLongident, LLongident) where

import Relude

import Lamagraph.Compiler.Syntax.Extension

-- | This type represents 'Text' dot-separated fragments in the source code.
newtype Longident = Longident (NonEmpty Text)
  deriving (Show, Eq, Ord, Hashable)

mkLongident :: NonEmpty Text -> Longident
mkLongident = Longident

type LLongident pass = XLocated pass Longident
