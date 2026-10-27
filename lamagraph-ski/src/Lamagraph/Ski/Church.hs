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
  notLam,
  andLam,
  orLam,
  ltLam,
  gtLam,
  leLam,
  geLam,
  pairLam,
  fstLam,
  sndLam,
  unitLam,
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

{- | The unit value (@()@). Encoded as a closed placeholder term (Church
@true@): unit carries no information, and the shape-directed decoder ignores
the term for a 0-field ADT, so any closed term works.
-}
unitLam :: State Int Lam
unitLam = trueLam

-- | Church @isZero@: @\n. n (\x. false) true@.
isZeroLam :: State Int Lam
isZeroLam = do
  n <- freshName
  x <- freshName
  falseL <- falseLam
  LLam n . LApp (LApp (LVar n) (LLam x falseL)) <$> trueLam

-- | Church @not@: @\b. b false true@.
notLam :: State Int Lam
notLam = do
  b <- freshName
  falseL <- falseLam
  LLam b . LApp (LApp (LVar b) falseL) <$> trueLam

-- | Church @and@: @\a b. a b false@.
andLam :: State Int Lam
andLam = do
  a <- freshName
  b <- freshName
  LLam a . LLam b . LApp (LApp (LVar a) (LVar b)) <$> falseLam

-- | Church @or@: @\a b. a true b@.
orLam :: State Int Lam
orLam = do
  a <- freshName
  b <- freshName
  trueL <- trueLam
  pure $ LLam a (LLam b (LApp (LApp (LVar a) trueL) (LVar b)))

{- | Church @<@ on Church numerals, returning a Church bool:
@\m n. not (isZero (sub n m))@. Since 'subLam' saturates at 0, @sub n m /= 0@
iff @n > m@.
-}
ltLam :: State Int Lam
ltLam = do
  m <- freshName
  n <- freshName
  notL <- notLam
  isZeroL <- isZeroLam
  subL <- subLam
  pure $
    LLam
      m
      ( LLam
          n
          ( LApp
              notL
              ( LApp
                  isZeroL
                  (LApp (LApp subL (LVar n)) (LVar m))
              )
          )
      )

-- | Church @>@ on Church numerals, returning a Church bool: @\m n. not (isZero (sub m n))@.
gtLam :: State Int Lam
gtLam = do
  m <- freshName
  n <- freshName
  notL <- notLam
  isZeroL <- isZeroLam
  subL <- subLam
  pure $
    LLam
      m
      ( LLam
          n
          ( LApp
              notL
              ( LApp
                  isZeroL
                  (LApp (LApp subL (LVar m)) (LVar n))
              )
          )
      )

-- | Church @<=@ on Church numerals, returning a Church bool: @\m n. isZero (sub m n)@.
leLam :: State Int Lam
leLam = do
  m <- freshName
  n <- freshName
  isZeroL <- isZeroLam
  subL <- subLam
  pure $
    LLam
      m
      ( LLam
          n
          (LApp isZeroL (LApp (LApp subL (LVar m)) (LVar n)))
      )

-- | Church @>=@ on Church numerals, returning a Church bool: @\m n. isZero (sub n m)@.
geLam :: State Int Lam
geLam = do
  m <- freshName
  n <- freshName
  isZeroL <- isZeroLam
  subL <- subLam
  pure $
    LLam
      m
      ( LLam
          n
          (LApp isZeroL (LApp (LApp subL (LVar n)) (LVar m)))
      )

-- | Church pair constructor: @pair x y = λf. f x y@.
pairLam :: State Int Lam
pairLam = do
  x <- freshName
  y <- freshName
  f <- freshName
  pure $ LLam x (LLam y (LLam f (LApp (LApp (LVar f) (LVar x)) (LVar y))))

-- | Church pair first projection: @fst p = p (λx y. x)@.
fstLam :: State Int Lam
fstLam = do
  p <- freshName
  x <- freshName
  y <- freshName
  pure $ LLam p (LApp (LVar p) (LLam x (LLam y (LVar x))))

-- | Church pair second projection: @snd p = p (λx y. y)@.
sndLam :: State Int Lam
sndLam = do
  p <- freshName
  x <- freshName
  y <- freshName
  pure $ LLam p (LApp (LVar p) (LLam x (LLam y (LVar y))))

-- | The SKI term for the Church numeral @n@ (a closed term, no free @V@ nodes).
intToChurch :: Int -> Term.Expr
intToChurch n = toSki (evalState (churchLam n) 0)
