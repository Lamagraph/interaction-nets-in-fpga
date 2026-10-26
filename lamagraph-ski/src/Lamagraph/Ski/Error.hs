{- | Errors reported by the SKI pipeline: translating an LML core program
('Lamagraph.Ski.Translate.translateCore') and evaluating it to a value
('Lamagraph.Ski.Decode.skiEval'). Kept in its own module so the translator and
the decoder can both name the error type without importing each other (which
would be a cycle).
-}
module Lamagraph.Ski.Error (
  SkiError (..),
) where

import Relude

import Lamagraph.Ski.Run (StepOverflow)

{- | An error from translating or evaluating an LML core program to a value via
SKI.
-}
data SkiError
  = StepOverflowReached StepOverflow
  | UnsupportedConstruct String
  deriving (Eq, Show)
