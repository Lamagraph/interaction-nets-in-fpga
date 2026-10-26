{- | Translator from the LML core language to the SKI combinator calculus
(L1) and a pure interaction-net runner. See the package description and the
design doc for the pipeline and encoding choices.
-}
module Lamagraph.Ski (
  encodeAndReduce,
  churchGraph,
  module Lamagraph.Ski.Church,
  module Lamagraph.Ski.Compare,
  module Lamagraph.Ski.Decode,
  module Lamagraph.Ski.Encode,
  module Lamagraph.Ski.Error,
  module Lamagraph.Ski.Lambda,
  module Lamagraph.Ski.Run,
  module Lamagraph.Ski.Translate,
  module Lamagraph.Ski.Value,
) where

import GraphRewriting (Graph)
import INet.Graph (SKI, fromTerm)
import Lamagraph.Compiler.Core (CoreBind, CoreExpr)

import Lamagraph.Ski.Church
import Lamagraph.Ski.Compare
import Lamagraph.Ski.Decode
import Lamagraph.Ski.Encode
import Lamagraph.Ski.Error
import Lamagraph.Ski.Lambda
import Lamagraph.Ski.Run
import Lamagraph.Ski.Translate
import Lamagraph.Ski.Value

import Relude

-- * Pipeline helpers

{- | Encode a closed LML core program to a SKI term and reduce it to normal
form with the default step budget. Fails with 'UnsupportedConstruct' if the
program uses a construct outside the minimal subset, or 'StepOverflowReached'
if the reduction exhausts the budget.
-}
encodeAndReduce :: [CoreBind] -> CoreExpr -> Either SkiError (Graph SKI)
encodeAndReduce binds resultExpr = do
  expr <- translateCore binds resultExpr
  case reduceToNormalForm defaultMaxSteps (fromTerm expr) of
    Right g -> Right g
    Left step -> Left (StepOverflowReached step)

{- | The SKI graph for the Church numeral @n@ (for comparing against reduced
program results).
-}
churchGraph :: Int -> Graph SKI
churchGraph n = fromTerm (toSki (evalState (churchLam n) 0))
