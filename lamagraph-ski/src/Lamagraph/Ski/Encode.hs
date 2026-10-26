{- | Encoding of canonical LML values as SKI terms. The core-program
translation lives in 'Lamagraph.Ski.Translate'; this module covers the value
side of the pipeline.
-}
module Lamagraph.Ski.Encode (
  encodeValue,
) where

import Relude

import Common.Term qualified as Term

import Lamagraph.Ski.Church (intToChurch)
import Lamagraph.Ski.Value (Value' (..))

{- | Encode a canonical value as a SKI term. The minimal subset only produces
'RInt' (a Church numeral); the other 'Value'' constructors are stubbed for Task
C.
-}
encodeValue :: Value' -> Term.Expr
encodeValue (RInt n) = intToChurch n
encodeValue other = error $ "encodeValue: " <> show other <> " is not supported yet (Task C)"
