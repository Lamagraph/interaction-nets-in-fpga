{-# LANGUAGE TypeFamilyDependencies #-}

-- | Module with TTG extension points type families
module Lamagraph.Compiler.Syntax.Extension where

import Relude

{- | Type to serve as a placeholder for TTG extension points, which can be constructed,
but aren't used to hold something more useful.
-}
data NoExtField = NoExtField deriving (Show)

-- | In Haskell is used to construct a term.
noExtField :: NoExtField
noExtField = NoExtField

-- | Isomorphic to 'Void'.
data DataConCantHappen deriving (Show)

dataConCanHappen :: DataConCantHappen -> a
dataConCanHappen x = case x of {}

-- | Used to add location to the tree
type family XLocated p a = r | r -> a

-- ============================================================================
-- Type families for LmlModule extension points

type family XCModule x

type family XXModule x

-- ============================================================================
-- Type families for LmlDecl extension points

type family XOpenD x

type family XXLmlDecl x

-- ============================================================================
-- Type families for OpenDecl extension points

type family XOpenDecl x

type family XXOpenDecl x
