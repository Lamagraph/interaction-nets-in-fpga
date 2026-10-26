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
  skiEval,
) where

import Relude

import Common.Term qualified as Term

import INet.Graph (SKI (..), fromTerm)
import Lamagraph.Compiler.Core (CoreBind, CoreExpr)

import Lamagraph.Ski.Compare (countLogical)
import Lamagraph.Ski.Error (SkiError (..))
import Lamagraph.Ski.Run (MaxSteps (..), StepOverflow (..), defaultMaxSteps, reduceToNormalForm)
import Lamagraph.Ski.Translate (translateCore)
import Lamagraph.Ski.Value (Value' (..))

-- | Marker name for one unit in the unary decoding chain.
tickName :: String
tickName = "__tick__"

-- | Marker name for the base of the unary decoding chain.
baseName :: String
baseName = "__base__"

{- | Decode a Church-encoded integer from a SKI term. The term must denote a
Church numeral (a function of two arguments). Returns the decoded integer,
or 'Left' with the step count at which the 'Lamagraph.Ski.Run.MaxSteps' budget
was exhausted.
-}
decodeChurch :: MaxSteps -> Term.Expr -> Either StepOverflow Int
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

{- | Evaluate a closed LML core program to a canonical value via the SKI
pipeline: translate to a SKI term, then decode the result with the default step
budget. The minimal subset only produces 'RInt' (a Church numeral); the other
'Value'' constructors are out of scope (Task C). Fails with
'UnsupportedConstruct' if the program uses a construct outside the minimal
subset, or 'StepOverflowReached' if the reduction exhausts the budget.
-}
skiEval :: [CoreBind] -> CoreExpr -> Either SkiError Value'
skiEval binds resultExpr = do
  expr <- translateCore binds resultExpr
  case decodeChurch defaultMaxSteps expr of
    Left step -> Left (StepOverflowReached step)
    Right n -> Right (RInt n)
