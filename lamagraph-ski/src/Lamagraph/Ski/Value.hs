{- | A canonical, comparable representation of LML runtime values, independent
of the evaluator's internal 'Lamagraph.Compiler.Eval.Value' (which carries
closures and partially-applied primitives and derives only 'Show').

'canonicalize' maps a runtime value to a 'Value''. Closures and primitives are
not legitimate final results; if one is encountered, 'canonicalize' reports it
via 'CanonicalizeError' rather than guessing.
-}
module Lamagraph.Ski.Value (
  Value' (..),
  CanonicalizeError (..),
  canonicalize,
) where

import Relude

import Lamagraph.Compiler.Eval qualified as Eval
import Lamagraph.Compiler.Typechecker.TcTypes (Name)

{- | A canonical value. 'RBool' and the non-'RInt' constructors are present now
as structure for the wider value set (Task C); the minimal subset only
produces 'RInt'.
-}
data Value'
  = RInt Int
  | RBool Bool
  | RChar Char
  | RString Text
  | RTuple [Value']
  | RAdt Name [Value']
  deriving (Eq, Show)

{- | A runtime value that is not a legitimate final result (a closure or a
primitive). Carries the offending value for a clear error message.
-}
newtype CanonicalizeError = NonResultValue Eval.Value deriving (Show)

{- | Map a runtime value to its canonical form. Fails with 'NonResultValue' if
the value is a closure or a (partially applied) primitive.
-}
canonicalize :: Eval.Value -> Either CanonicalizeError Value'
canonicalize = \case
  Eval.VInt n -> Right (RInt n)
  Eval.VChar c -> Right (RChar c)
  Eval.VString s -> Right (RString s)
  Eval.VTuple v vs -> RTuple <$> traverse canonicalize (v : toList vs)
  Eval.VAdt dc vs -> RAdt dc <$> traverse canonicalize vs
  other -> Left (NonResultValue other)
