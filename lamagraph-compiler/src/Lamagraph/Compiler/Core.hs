module Lamagraph.Compiler.Core (
  Literal (..),
  DataCon,
  Var (..),
  Expr (..),
  MatchAlt,
  AltCon (..),
  Bind (..),
  CoreExpr,
  CoreMatchAlt,
  CoreBind,
) where

import Relude

import Lamagraph.Compiler.Typechecker.TcTypes

data Literal = LitInt Int | LitChar Char | LitString Text deriving (Show)

-- FIXME: Must change after addition of ADTs
type DataCon = Name

-- TODO: Is this wrapper really required?
newtype Var
  = Id Name -- Term variable
  deriving (Eq, Show, Hashable)

data Expr b
  = Var b
  | Lit Literal
  | App (Expr b) (Expr b)
  | Lam b (Expr b)
  | Let (Bind b) (Expr b)
  | -- | For a reason behind this signature see https://gitlab.haskell.org/ghc/ghc/-/wikis/commentary/compiler/core-syn-type#case-expressions
    Match (Expr b) b (NonEmpty (MatchAlt b))
  | -- In Haskell, tuples are just syntactic sugar for @data TupleN a1...aN = TupleN a1...aN@,
    -- but in Caml they are separate construction
    Tuple (Expr b) (NonEmpty (Expr b))
  deriving (Show)

type MatchAlt b = (AltCon, [b], Expr b)

data AltCon = DataAlt DataCon | LitAlt Literal | TupleAlt | DEFAULT deriving (Show)

data Bind b = NonRec b (Expr b) | Rec (NonEmpty (b, Expr b)) deriving (Show)

type CoreExpr = Expr Var
type CoreMatchAlt = MatchAlt Var
type CoreBind = Bind Var
