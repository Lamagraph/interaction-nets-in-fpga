{- | Translator from the LML core language to the SKI combinator calculus
(L1) and a pure interaction-net runner. See the package description and the
design doc for the pipeline and encoding choices.
-}
module Lamagraph.Ski (
  encodeAndReduce,
  translateAndRun,
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

import Control.Exception (try)
import Data.HashMap.Lazy qualified as HashMap
import GraphRewriting (Graph)
import INet.Graph (SKI, fromTerm)
import Lamagraph.Compiler.Core (Bind (NonRec), CoreBind, CoreExpr, Var (Id))
import Lamagraph.Compiler.Eval (EvalEnv (..), EvalException, evalCoreBindsDefEnv)
import Lamagraph.Compiler.Syntax.Longident (mkLongident)
import Lamagraph.Compiler.Typechecker.TcTypes (Name (..))

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

{- | Evaluate a closed LML core program with BOTH the LML interpreter and the
SKI pipeline, and check they agree. Returns the common value, or 'Left' with
the first error: an LML evaluation\/canonicalization failure, a SKI translation
or reduction failure, or 'Mismatch' (carrying both values) if the two disagree.
-}
translateAndRun :: [CoreBind] -> CoreExpr -> IO (Either SkiError Value')
translateAndRun binds resultExpr = do
  lmlRes <- lmlValue binds resultExpr
  case lmlRes of
    Left e -> return (Left e)
    Right a ->
      let skiRes = skiEval binds resultExpr
       in case skiRes of
            Left e -> return (Left e)
            Right b
              | a == b -> return (Right a)
              | otherwise -> return (Left (Mismatch a b))

{- | Evaluate a core program (with its enclosing bindings) with the LML
interpreter by binding the result expression to a fresh variable and reading it
back from the environment, then canonicalizing the runtime value.
-}
lmlValue :: [CoreBind] -> CoreExpr -> IO (Either SkiError Value')
lmlValue binds e = do
  let resultVar = Id (Name (mkLongident (pure "__result__")))
  res <- try (runIdentityT (evalCoreBindsDefEnv (binds ++ [NonRec resultVar e])))
  case res of
    Left (exc :: EvalException) -> return (Left (LmlEvaluationFailed (show exc)))
    Right (EvalEnv hm) ->
      case HashMap.lookup resultVar hm of
        Nothing -> return (Left (LmlEvaluationFailed "result variable missing from environment"))
        Just v ->
          case canonicalize v of
            Right v' -> return (Right v')
            Left ce -> return (Left (LmlResultNotAValue (show ce)))

{- | The SKI graph for the Church numeral @n@ (for comparing against reduced
program results).
-}
churchGraph :: Int -> Graph SKI
churchGraph n = fromTerm (toSki (evalState (churchLam n) 0))
