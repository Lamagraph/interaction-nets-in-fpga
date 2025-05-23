{-# LANGUAGE TypeFamilyDependencies #-}
-- Exporting every type family from here will be too tedious
{-# OPTIONS_GHC -Wno-missing-export-lists #-}

{- | Module with TTG extension points type families

AST here is designed using Trees That Grow (<https://www.jucs.org/jucs_23_1/trees_that_grow/jucs_23_01_0042_0062_najd.pdf>) pattern.
It uses type families to contain phase-specific information.
Simpler, but more up-to-date explanations can be found here:

* <https://gitlab.haskell.org/ghc/ghc/-/wikis/implementing-trees-that-grow/trees-that-grow-guidance>
* <https://gitlab.haskell.org/ghc/ghc/-/wikis/implementing-trees-that-grow/handling-source-locations>
* <https://gitlab.haskell.org/ghc/ghc/-/wikis/implementing-trees-that-grow/instances>

Regarding directory structure, it loosely follows GHC's one.
In "Lamagraph.Compiler.Syntax".* we have the most general tree with open type families,
meaning that "Lamagraph.Compiler.Syntax".* can easily be transformed into a library.
All the specialization must be done outside (currently in "Lamagraph.Compiler.Extension").
-}
module Lamagraph.Compiler.Syntax.Extension where

import Relude

{- | Type to serve as a placeholder for TTG extension points, which can be constructed,
but aren't used to hold something more useful.
-}
data NoExtField = NoExtField deriving (Show, Eq)

-- | Is used to construct a term.
noExtField :: NoExtField
noExtField = NoExtField

-- | Isomorphic to 'Void'.
data DataConCantHappen deriving (Show, Eq)

dataConCanHappen :: DataConCantHappen -> a
dataConCanHappen x = case x of {}

-- | Used to add location to the tree
type family XLocated p a = r | r -> a

--------------------------------------------------
-- Type families for LmlModule extension points --
--------------------------------------------------

type family XCModule x
type family XXModule x

------------------------------------------------
-- Type families for LmlDecl extension points --
------------------------------------------------

type family XOpenD x
type family XValD x
type family XTyD x
type family XXDecl x

-------------------------------------------------
-- Type families for OpenDecl extension points --
-------------------------------------------------

type family XOpenDecl x
type family XXOpenDecl x

-----------------------------------------------
-- Type families for TyDecl extension points --
-----------------------------------------------

type family XAliasDecl x
type family XDataDecl x
type family XXTyDecl x

------------------------------------------------
-- Type families for ConDecl extension points --
------------------------------------------------

type family XConDecl x
type family XXConDecl x

------------------------------------------------
-- Type families for LmlType extension points --
------------------------------------------------

type family XLmlTyVar x
type family XLmlTyArrow x
type family XLmlTyTuple x
type family XLmlTyConstr x
type family XXType x

-----------------------------------------------
-- Type families for LmlLit extension points --
-----------------------------------------------

type family XLmlInt x
type family XLmlChar x
type family XLmlString x
type family XXLit x

-----------------------------------------------
-- Type families for LmlPat extension points --
-----------------------------------------------

type family XLmlPatAny x
type family XLmlPatVar x
type family XLmlPatConstant x
type family XLmlPatTuple x
type family XLmlPatConstruct x
type family XLmlPatOr x
type family XLmlPatConstraint x
type family XXPat x

------------------------------------------------
-- Type families for LmlExpr extension points --
------------------------------------------------

type family XLmlExprIdent x
type family XLmlExprConstant x
type family XLmlExprLet x
type family XLmlExprFunction x
type family XLmlExprApply x
type family XLmlExprMatch x
type family XLmlExprTuple x
type family XLmlExprConstruct x
type family XLmlExprIfThenElse x
type family XLmlExprConstraint x
type family XXExpr x

-----------------------------------------------------
-- Type families for LmlBindGroup extension points --
-----------------------------------------------------

type family XLmlBindGroup x
type family XXBindGroup x

------------------------------------------------
-- Type families for LmlBind extension points --
------------------------------------------------

type family XLmlBind x
type family XXBind x

------------------------------------------------
-- Type families for LmlCase extension points --
------------------------------------------------

type family XLmlCase x
type family XXCase x
