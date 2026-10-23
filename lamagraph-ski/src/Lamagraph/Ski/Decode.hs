{- | Decoding of a Church-encoded integer carried by a reduced SKI term.

SKI interaction-net normal forms are not canonical for combinator
equivalence: two equivalent terms (e.g. @add 1 1@ and the direct Church
numeral @2@) reduce to different normal forms. So a result cannot be read
off by comparing normal forms. Instead we force the result to a canonical
unary shape: a Church numeral @n@ applied to @(λy. tick y)@ and a base
reduces to a chain of exactly @n@ @tick@ nodes, which we count.
-}
module Lamagraph.Ski.Decode (
  tickName,
  baseName,
  decodeChurch,
) where

import Relude

import Common.Term qualified as Term

import INet.Graph (SKI (..), fromTerm)

import Lamagraph.Ski.Compare (countLogical)
import Lamagraph.Ski.Run (reduceToNormalForm)

-- | Marker name for one unit in the unary decoding chain.
tickName :: String
tickName = "__tick__"

-- | Marker name for the base of the unary decoding chain.
baseName :: String
baseName = "__base__"

{- | Decode a Church-encoded integer from a SKI term. The term must denote a
Church numeral (a function of two arguments). Returns the decoded integer,
or 'Left step' if the step budget is exceeded before reaching normal form.
-}
decodeChurch :: Int -> Term.Expr -> Either Int Int
decodeChurch maxSteps term =
  let decoderFn = Term.A (Term.A Term.S (Term.A Term.K (Term.V tickName))) Term.I
      full = Term.A (Term.A term decoderFn) (Term.V baseName)
   in case reduceToNormalForm maxSteps (fromTerm full) of
        Left step -> Left step
        Right g -> Right (countLogical isTick g)
 where
  isTick :: SKI -> Bool
  isTick = \case
    V{name = n} -> n == tickName
    _ -> False
