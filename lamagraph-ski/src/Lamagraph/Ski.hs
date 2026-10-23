{- | Translator from the LML core language to the SKI combinator calculus
(L1) and a pure interaction-net runner. See the package description and the
design doc for the pipeline and encoding choices.
-}
module Lamagraph.Ski (
  defaultMaxSteps,
  encodeAndReduce,
  churchGraph,
  module Lamagraph.Ski.Church,
  module Lamagraph.Ski.Compare,
  module Lamagraph.Ski.Decode,
  module Lamagraph.Ski.Encode,
  module Lamagraph.Ski.Lambda,
  module Lamagraph.Ski.Run,
) where

import GraphRewriting (Graph)
import INet.Graph (SKI, fromTerm)
import Lamagraph.Compiler.Core (CoreBind, CoreExpr)

import Lamagraph.Ski.Church
import Lamagraph.Ski.Compare
import Lamagraph.Ski.Decode
import Lamagraph.Ski.Encode
import Lamagraph.Ski.Lambda
import Lamagraph.Ski.Run

import Relude

-- * Pipeline helpers

-- | Default step budget for reducing a SKI net to normal form.
defaultMaxSteps :: Int
defaultMaxSteps = 100000

{- | Encode a closed LML core program to a SKI term and reduce it to normal
form. Returns 'Left step' if the step budget is exceeded.
-}
encodeAndReduce :: [CoreBind] -> CoreExpr -> Either Int (Graph SKI)
encodeAndReduce binds resultExpr =
  reduceToNormalForm defaultMaxSteps (fromTerm (encode binds resultExpr))

{- | The SKI graph for the Church numeral @n@ (for comparing against reduced
program results).
-}
churchGraph :: Int -> Graph SKI
churchGraph n = fromTerm (toSki (evalState (churchLam n) 0))
