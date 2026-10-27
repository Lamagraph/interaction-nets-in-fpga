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
  decodeValue,
  skiEval,
) where

import Relude

import Common.Term qualified as Term

import INet.Graph (SKI (..), fromTerm)
import Lamagraph.Compiler.Core (CoreBind, CoreExpr)
import Lamagraph.Compiler.Typechecker.TcTypes (Name)

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

-- | Marker name for the @true@ branch of a Church-boolean decode.
tickTName :: String
tickTName = "__tickT__"

-- | Marker name for the @false@ branch of a Church-boolean decode.
tickFName :: String
tickFName = "__tickF__"

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

{- | Decode a Church-encoded boolean from a SKI term. The term must denote a
Church boolean (@\\t. \\f. t@ or @\\t. \\f. f@). It is applied to two distinct
markers ('tickTName' and 'tickFName'); whichever marker survives in the normal
form indicates the boolean value. Returns the decoded boolean, or 'Left' with
the step count at which the 'Lamagraph.Ski.Run.MaxSteps' budget was exhausted.
-}
decodeBool :: MaxSteps -> Term.Expr -> Either StepOverflow Bool
decodeBool maxSteps term =
  let full = Term.A (Term.A term (Term.V tickTName)) (Term.V tickFName)
   in case reduceToNormalForm maxSteps (fromTerm full) of
        Left step -> Left step
        Right g -> Right (countLogical (isMarker tickTName) g > 0)
 where
  isMarker :: String -> SKI -> Bool
  isMarker m = \case
    V{name = n} -> n == m
    _ -> False

{- | Decode a SKI term to a canonical value, directed by the shape (the
structure of the expected value, taken from the LML interpreter's result). The
shape supplies the type information the SKI normal form lacks (SKI normal forms
are not canonical for combinator equivalence); the actual values are extracted
from the reduced term. Returns 'Left' with the step count at which the
'Lamagraph.Ski.Run.MaxSteps' budget was exhausted.
-}
decodeValue :: MaxSteps -> Value' -> Term.Expr -> Either StepOverflow Value'
decodeValue maxSteps shape term = case shape of
  RInt _ -> decodeChurch maxSteps term <&> RInt
  RBool _ -> decodeBool maxSteps term <&> RBool
  RTuple vs -> decodeTuple maxSteps vs term
  RAdt con vs -> decodeAdt maxSteps con vs term
  _ -> Left (StepOverflow 0)

-- | The SKI selector @λa b. a@ (first projection of a Church pair).
fstSel :: Term.Expr
fstSel = Term.A (Term.A Term.S (Term.A Term.K Term.K)) Term.I

-- | The SKI selector @λa b. b@ (second projection of a Church pair).
sndSel :: Term.Expr
sndSel = Term.A Term.K Term.I

{- | Decode a nested-Church-pair term to its elements, directed by the shape
(the expected list of element shapes). The term is the right-associated nesting
@pair e1 (pair e2 (... (pair e(n-1) en)))@; this walks that nesting, projecting
@fst@/@snd@ at each level and decoding each element with 'decodeValue'. The base
case (a single remaining element) decodes the leaf directly. Returns 'Left' with
the step count at which the 'Lamagraph.Ski.Run.MaxSteps' budget was exhausted.
-}
decodePairFields :: MaxSteps -> [Value'] -> Term.Expr -> Either StepOverflow [Value']
decodePairFields _ [] _ = Left (StepOverflow 0)
decodePairFields maxSteps [x] term = decodeValue maxSteps x term <&> (: [])
decodePairFields maxSteps (x : xs) term = do
  v1 <- decodeValue maxSteps x (Term.A term fstSel)
  rest <- decodePairFields maxSteps xs (Term.A term sndSel)
  pure (v1 : rest)

{- | Decode a nested-Church-pair term to a tuple, directed by the shape (the
expected list of element shapes).
-}
decodeTuple :: MaxSteps -> [Value'] -> Term.Expr -> Either StepOverflow Value'
decodeTuple maxSteps shapes term = RTuple <$> decodePairFields maxSteps shapes term

{- | Decode a tagged-ADT term to an ADT value, directed by the shape (the
constructor name and the expected field shapes). The term is the pair
@(tag, args)@; the tag is supplied by the shape (the constructor is known from
the LML interpreter's result), so only @args@ is projected: a single field is
decoded directly, several fields as a nested-Church-pair tuple. Returns 'Left'
with the step count at which the 'Lamagraph.Ski.Run.MaxSteps' budget was
exhausted.
-}
decodeAdt :: MaxSteps -> Name -> [Value'] -> Term.Expr -> Either StepOverflow Value'
decodeAdt _ con [] _ = pure (RAdt con [])
decodeAdt maxSteps con fieldShapes term =
  RAdt con <$> decodePairFields maxSteps fieldShapes (Term.A term sndSel)

{- | Evaluate a closed LML core program to a canonical value via the SKI
pipeline: translate to a SKI term, then decode the result (directed by the
shape, the LML interpreter's result) with the default step budget. Fails with
'UnsupportedConstruct' if the program uses a construct outside the supported
subset, or 'StepOverflowReached' if the reduction exhausts the budget.
-}
skiEval :: Value' -> [CoreBind] -> CoreExpr -> Either SkiError Value'
skiEval shape binds resultExpr = do
  expr <- translateCore binds resultExpr
  case decodeValue defaultMaxSteps shape expr of
    Left step -> Left (StepOverflowReached step)
    Right v -> Right v
