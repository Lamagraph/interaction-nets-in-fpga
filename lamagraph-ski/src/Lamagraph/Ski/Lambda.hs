{- | A minimal named-variable lambda term, used as the intermediate
representation between the LML core language and the interaction-net
backends (the SKI combinator calculus and the YALE lambda net), plus the
standard lambda-to-SKI translation.
-}
module Lamagraph.Ski.Lambda (
  Lam (..),
  fv,
  subst,
  replaceSelfRef,
  selfRefName,
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
  | {- | A self-reference to the enclosing recursive function. This is a
    placeholder that is spliced into a cycle at the net level (see
    'Lamagraph.Ski.YaleNet.buildYaleNet'). It never appears in a closed
    term: it is only produced by the recursive-binding translation and is
    resolved before the net is built.
    -}
    LSelfRef
  | {- | Marks a recursive function. The YALE net builder
    ('Lamagraph.Ski.YaleNet.buildYaleNet') expands this to the paper's
    tie-the-knot cycle (Mackie 1998, §6): the body is built once, with every
    'LSelfRef' wired to the @self@ binder, abstracted over it to get
    @F = λself. body'@, and the fixed point @F (F (...))@ is tied as a
    finite cyclic net.
    -}
    LRec Lam
  | {- | Marks a group of mutually-recursive functions. The list carries
    @(name, body)@ pairs: each body has its own self-references as 'LSelfRef'
    and cross-references to the other functions as 'LVar' (by name). The
    final 'Lam' is the surrounding body, with references to the functions as
    'LVar' (by name). The YALE net builder compiles each function to its own
    output edge and wires the self- and cross-references back to those edges
    (a cycle through 'Lamagraph.Ski.YaleNet.Copy' agents).
    -}
    LMutRec [(String, Lam)] Lam
  deriving (Show, Eq)

{- | Generate a fresh, unique variable name. Composing lambda terms (e.g.
applying a Church-encoded operator to Church-encoded arguments) reuses the
same binder names unless they are made unique, which breaks the SKI
reduction. Thread a counter through term construction via 'State' and call
this for every binder.
-}
freshName :: State Int String
freshName = state $ \i -> ("__v" <> show i, i + 1)

{- | Free variables of a lambda term. A 'LSelfRef' is a reference to the
enclosing recursive function, treated as a free variable named
'selfRefName'; an 'LRec' binds that name (along with the binder of the
wrapped lambda), so it is subtracted here.
-}
fv :: Lam -> Set String
fv = \case
  LVar v -> Set.singleton v
  LApp f x -> fv f <> fv x
  LLam v body -> fv body Set.\\ Set.singleton v
  LSelfRef -> Set.singleton selfRefName
  LRec l -> fv l Set.\\ Set.singleton selfRefName
  LMutRec groups body ->
    let groupNames = Set.fromList (map fst groups)
        allFvs = foldr ((<>) . fv . snd) Set.empty groups <> fv body
     in allFvs `Set.difference` groupNames

{- | Substitute @repl@ for every free occurrence of @v@ in a lambda term.
(Ignores variable capture; adequate for the minimal subset, hardened in S5.)
-}
subst :: String -> Lam -> Lam -> Lam
subst v repl (LVar w) = if w == v then repl else LVar w
subst v repl (LApp f x) = LApp (subst v repl f) (subst v repl x)
subst v repl (LLam w body)
  | w == v = LLam w body
  | otherwise = LLam w (subst v repl body)
subst _ _ LSelfRef = LSelfRef
subst v repl (LRec l) = LRec (subst v repl l)
subst v repl (LMutRec groups body)
  | v `Set.member` groupNames = LMutRec groups body
  | otherwise = LMutRec (map (second (subst v repl)) groups) (subst v repl body)
 where
  groupNames = Set.fromList (map fst groups)

{- | Replace every 'LSelfRef' in a lambda term with a free variable @v@. Used
by the recursive-binding handlers to convert a recursive body (with
self-references) into a plain lambda term.
-}
replaceSelfRef :: String -> Lam -> Lam
replaceSelfRef v = \case
  LSelfRef -> LVar v
  LVar w -> LVar w
  LApp f x -> LApp (replaceSelfRef v f) (replaceSelfRef v x)
  LLam w body -> LLam w (replaceSelfRef v body)
  LRec l -> LRec (replaceSelfRef v l)
  LMutRec groups body ->
    LMutRec (map (second (replaceSelfRef v)) groups) (replaceSelfRef v body)

{- | The reserved variable name for a self-reference ('LSelfRef'). The net
builder recognises this name and splices the corresponding occurrence into a
cycle (see 'Lamagraph.Ski.YaleNet.buildYaleNet').
-}
selfRefName :: String
selfRefName = "__selfref__"

{- | Translate a lambda term to a SKI term. A bound variable is introduced as a
SKI @V@ node by the body translation and then bound by the abstraction rule
('abstractSki'), so a closed lambda term translates to a closed SKI term. The
recursion markers are not part of the SKI backend's domain: 'LRec' is
transparent (the SKI translation never receives one —
'Lamagraph.Ski.Translate.translateCore' rejects recursive bindings) and
'LMutRec' is an error (the SKI backend is non-recursive-only).
-}
toSki :: Lam -> Term.Expr
toSki = \case
  LVar v -> Term.V v
  LApp f x -> Term.A (toSki f) (toSki x)
  LLam v body -> abstractSki v (toSki body)
  LSelfRef -> Term.V selfRefName
  LRec l -> toSki l
  LMutRec _ _ -> error "toSki: mutual recursion is not supported by the SKI backend"

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
