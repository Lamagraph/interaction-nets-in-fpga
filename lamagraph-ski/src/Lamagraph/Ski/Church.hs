{- | Church encodings of natural numbers and arithmetic, as lambda terms.
These are the value encodings used to represent LML integers in the SKI
combinator calculus. Binder names are generated fresh (via a 'State'
counter) so that composing encodings does not introduce name clashes.
-}
module Lamagraph.Ski.Church (
  churchLam,
  addLam,
) where

import Relude

import Lamagraph.Ski.Lambda (Lam (..), freshName)

-- | The Church numeral @n@, i.e. @\f x -> f^n x@.
churchLam :: Int -> State Int Lam
churchLam n
  | n < 0 = error "churchLam: negative numeral"
  | otherwise = do
      f <- freshName
      x <- freshName
      pure $ LLam f (LLam x (applyN n (LVar f) (LVar x)))
 where
  applyN 0 _ x = x
  applyN k f x = LApp f (applyN (k - 1) f x)

-- | Church addition: @\m n f x -> m f (n f x)@.
addLam :: State Int Lam
addLam = do
  m <- freshName
  n <- freshName
  f <- freshName
  x <- freshName
  pure $
    LLam
      m
      ( LLam
          n
          ( LLam
              f
              ( LLam
                  x
                  (LApp (LApp (LVar m) (LVar f)) (LApp (LApp (LVar n) (LVar f)) (LVar x)))
              )
          )
      )
