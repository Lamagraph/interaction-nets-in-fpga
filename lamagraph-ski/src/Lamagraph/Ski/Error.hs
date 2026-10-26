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
import Lamagraph.Ski.Value (Value')

{- | An error from translating or evaluating an LML core program to a value via
SKI, or from the differential comparison in
'Lamagraph.Ski.translateAndRun'.
-}
data SkiError
  = -- | The SKI reduction exhausted its step budget before reaching normal form.
    StepOverflowReached StepOverflow
  | -- | The program uses a construct outside the minimal subset.
    UnsupportedConstruct String
  | {- | The LML interpreter and the SKI pipeline disagree; carries the LML value
    and the SKI value, respectively, for debugging.
    -}
    Mismatch Value' Value'
  | -- | The LML interpreter threw while evaluating the program.
    LmlEvaluationFailed String
  | -- | The LML result is a closure or primitive, not a canonical value.
    LmlResultNotAValue String
  deriving (Eq, Show)
