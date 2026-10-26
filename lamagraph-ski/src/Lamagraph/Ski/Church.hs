{- | Church encodings of natural numbers, arithmetic, and booleans, as lambda
terms. These are the value encodings used to represent LML values in the SKI
combinator calculus.

The lambda structures mirror the Core-language encodings in
'Lamagraph.Compiler.Nets.Encodings.LambdaCalculusCore' (re-expressed over the
'Lam' IR rather than duplicated). Binder names are generated fresh (via a
'State' counter) so that composing encodings never introduces a name clash.
-}
module Lamagraph.Ski.Church (
  churchLam,
  zeroLam,
  succLam,
  addLam,
  multLam,
  subLam,
  predLam,
  isZeroLam,
  trueLam,
  falseLam,
  intToChurch,
) where

import Relude

import Common.Term qualified as Term

import Lamagraph.Ski.Lambda (Lam (..), freshName, toSki)

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

-- | Church zero: @\f x. x@.
zeroLam :: State Int Lam
zeroLam = churchLam 0

-- | Church successor: @\n f x. f (n f x)@.
succLam :: State Int Lam
succLam = do
  n <- freshName
  f <- freshName
  x <- freshName
  pure $ LLam n (LLam f (LLam x (LApp (LVar f) (LApp (LApp (LVar n) (LVar f)) (LVar x)))))

-- | Church addition: @\m n f x. m f (n f x)@.
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

-- | Church multiplication: @\m n f x. m (n f) x@.
multLam :: State Int Lam
multLam = do
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
                  (LApp (LApp (LVar m) (LApp (LVar n) (LVar f))) (LVar x))
              )
          )
      )

-- | Church predecessor, saturating at 0: @\n f x. n (\g h. h (g f)) (\u. x) (\u. u)@.
predLam :: State Int Lam
predLam = do
  n <- freshName
  f <- freshName
  x <- freshName
  g <- freshName
  h <- freshName
  u1 <- freshName
  u2 <- freshName
  pure $
    LLam
      n
      ( LLam
          f
          ( LLam
              x
              ( LApp
                  ( LApp
                      (LApp (LVar n) (LLam g (LLam h (LApp (LVar h) (LApp (LVar g) (LVar f))))))
                      (LLam u1 (LVar x))
                  )
                  (LLam u2 (LVar u2))
              )
          )
      )

{- | Saturating Church subtraction: @\m n. n (\k. pred k) m@ = @max 0 (m - n)@.
Applies 'predLam' @n@ times to @m@; since 'predLam' saturates at 0, the result
is clamped at 0 (negative results are out of scope for the minimal subset).
-}
subLam :: State Int Lam
subLam = do
  m <- freshName
  n <- freshName
  k <- freshName
  predL <- predLam
  pure $ LLam m (LLam n (LApp (LApp (LVar n) (LLam k (LApp predL (LVar k)))) (LVar m)))

-- | Church boolean @true@: @\x y. x@.
trueLam :: State Int Lam
trueLam = do
  x <- freshName
  y <- freshName
  pure $ LLam x (LLam y (LVar x))

-- | Church boolean @false@: @\x y. y@.
falseLam :: State Int Lam
falseLam = do
  x <- freshName
  y <- freshName
  pure $ LLam x (LLam y (LVar y))

-- | Church @isZero@: @\n. n (\x. false) true@.
isZeroLam :: State Int Lam
isZeroLam = do
  n <- freshName
  x <- freshName
  falseL <- falseLam
  LLam n . LApp (LApp (LVar n) (LLam x falseL)) <$> trueLam

-- | The SKI term for the Church numeral @n@ (a closed term, no free @V@ nodes).
intToChurch :: Int -> Term.Expr
intToChurch n = toSki (evalState (churchLam n) 0)
