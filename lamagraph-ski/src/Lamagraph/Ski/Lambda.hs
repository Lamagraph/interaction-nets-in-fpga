{- | A minimal named-variable lambda term, used as the intermediate
representation between the LML core language and the SKI combinator
calculus, plus the standard lambda-to-SKI translation.
-}
module Lamagraph.Ski.Lambda (
  Lam (..),
  fv,
  subst,
  toSki,
  abstractSki,
  freshName,
) where

import Relude

import Common.Term qualified as Term
import Data.Set qualified as Set

-- | A lambda term with named variables.
data Lam
  = LVar String
  | LApp Lam Lam
  | LLam String Lam
  deriving (Show, Eq)

{- | Generate a fresh, unique variable name. Composing lambda terms (e.g.
applying a Church-encoded operator to Church-encoded arguments) reuses the
same binder names unless they are made unique, which breaks the SKI
reduction. Thread a counter through term construction via 'State' and call
this for every binder.
-}
freshName :: State Int String
freshName = state $ \i -> ("__v" <> show i, i + 1)

-- | Free variables of a lambda term.
fv :: Lam -> Set String
fv = \case
  LVar v -> Set.singleton v
  LApp f x -> fv f <> fv x
  LLam v body -> fv body Set.\\ Set.singleton v

{- | Substitute @repl@ for every free occurrence of @v@ in a lambda term.
(Ignores variable capture; adequate for the minimal subset, hardened in S5.)
-}
subst :: String -> Lam -> Lam -> Lam
subst v repl (LVar w) = if w == v then repl else LVar w
subst v repl (LApp f x) = LApp (subst v repl f) (subst v repl x)
subst v repl (LLam w body)
  | w == v = LLam w body
  | otherwise = LLam w (subst v repl body)

{- | Translate a lambda term to a SKI term. A bound variable is introduced as a
SKI @V@ node by the body translation and then bound by the abstraction rule
('abstractSki'), so a closed lambda term translates to a closed SKI term.
-}
toSki :: Lam -> Term.Expr
toSki = \case
  LVar v -> Term.V v
  LApp f x -> Term.A (toSki f) (toSki x)
  LLam v body -> abstractSki v (toSki body)

{- | Whether @v@ occurs as a free @V@ node anywhere in a SKI term. The SKI
constants (@S@, @K@, @I@) carry no free variables.
-}
freeIn :: String -> Term.Expr -> Bool
freeIn v = \case
  Term.V w -> w == v
  Term.A t1 t2 -> freeIn v t1 || freeIn v t2
  _ -> False

{- | @abstractSki v t@ is the SKI term for @λv. t@, where @t@ is a SKI term
viewed as a lambda term with the SKI combinators as constants. This is the
standard lambda-to-SKI abstraction rule applied to SKI terms:

> λv. t     = K t        (v not free in t)
> λv. v     = I
> λv. c     = K c        (c a constant or a free variable ≠ v)
> λv. t1 t2 = S (λv. t1) (λv. t2)

The @K t@ shortcut when @v@ is not free in @t@ is essential for efficiency:
without it the @S@ rule re-abstracts every subterm, blowing the term up
exponentially in the nesting depth (a comparison combinator grew to ~290k SKI
nodes); with it the translation stays linear in the term size. It recurses on
the subterms of @t@, so it terminates. Translating the body first and then
abstracting (rather than pattern-matching a lambda spine in the body) is what
makes the translation correct for nested abstractions such as @λv. λy. (a b)@.
-}
abstractSki :: String -> Term.Expr -> Term.Expr
abstractSki v t
  | not (freeIn v t) = Term.A Term.K t
  | otherwise = case t of
      Term.V w
        | w == v -> Term.I
        | otherwise -> Term.A Term.K (Term.V w)
      Term.S -> Term.A Term.K Term.S
      Term.K -> Term.A Term.K Term.K
      Term.I -> Term.A Term.K Term.I
      Term.A t1 t2 -> Term.A (Term.A Term.S (abstractSki v t1)) (abstractSki v t2)
