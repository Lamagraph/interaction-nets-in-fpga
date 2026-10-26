{- | Translation from the LML core language to the SKI combinator calculus.
The core term is expanded to a lambda term (inlining non-recursive lets and
swapping the standard arithmetic operators for their Church encodings), then
converted to SKI via the standard lambda-to-SKI rule. All binders are given
fresh, unique names (via a 'State' counter) so that composing terms never
introduces a name clash.
-}
module Lamagraph.Ski.Translate (
  translateCore,
  stdPlus,
  stdMinus,
  stdTimes,
) where

import Control.Monad.Except
import Relude

import Common.Term qualified as Term
import Data.Text qualified as T

import Lamagraph.Compiler.Core
import Lamagraph.Compiler.ModuleResolver.DefaultEnv (stdPrefix)
import Lamagraph.Compiler.Syntax.Longident (Longident (..), mkLongident)
import Lamagraph.Compiler.Typechecker.TcTypes (Name (..))
import Lamagraph.Ski.Church (addLam, churchLam, multLam, subLam)
import Lamagraph.Ski.Error (SkiError (..))
import Lamagraph.Ski.Lambda (Lam (..), freshName, subst, toSki)

{- | The translation monad: thread a fresh-name counter and report the first
construct outside the minimal subset.
-}
type TranslateM = ExceptT SkiError (State Int)

{- | Translate a closed LML core program (top-level bindings + result
expression) to a closed SKI term (no free @V@ nodes). Minimal subset: integer
literals, '+', '-', '*', let, lambda, application. Constructs outside the
subset are reported via 'UnsupportedConstruct'.
-}
translateCore :: [CoreBind] -> CoreExpr -> Either SkiError Term.Expr
translateCore binds resultExpr =
  fmap toSki (evalState (runExceptT (buildLam binds resultExpr)) 0)

{- | Build the lambda term for a program: convert the result expression, then
inline the top-level (non-recursive) bindings.
-}
buildLam :: [CoreBind] -> CoreExpr -> TranslateM Lam
buildLam binds resultExpr = do
  resultLam <- coreExprToLam resultExpr
  inlineBinds binds resultLam

inlineBinds :: [CoreBind] -> Lam -> TranslateM Lam
inlineBinds [] lam = pure lam
inlineBinds (NonRec v e : bs) lam = do
  repl <- coreExprToLam e
  inlineBinds bs (subst (varName v) repl lam)
inlineBinds (Rec _ : _) _ = throwError (UnsupportedConstruct "recursive let")

coreExprToLam :: CoreExpr -> TranslateM Lam
coreExprToLam = \case
  Var v
    | v == stdPlus -> lift addLam
    | v == stdMinus -> lift subLam
    | v == stdTimes -> lift multLam
    | otherwise -> pure (LVar (varName v))
  Lit (LitInt n) -> lift (churchLam n)
  Lit other -> throwError (UnsupportedConstruct ("literal " <> show other))
  App f x -> LApp <$> coreExprToLam f <*> coreExprToLam x
  Lam v body -> do
    bodyLam <- coreExprToLam body
    fresh <- lift freshName
    pure (LLam fresh (subst (varName v) (LVar fresh) bodyLam))
  Let (NonRec v e) body -> do
    repl <- coreExprToLam e
    bodyLam <- coreExprToLam body
    pure (subst (varName v) repl bodyLam)
  Let (Rec _) _ -> throwError (UnsupportedConstruct "recursive let")
  other -> throwError (UnsupportedConstruct ("construct " <> show other))

-- | Render a core 'Var' as a plain string (dot-joined long identifier).
varName :: Var -> String
varName (Id (Name (Longident texts))) = T.unpack (T.intercalate "." (toList texts))

-- | The @#std.+@ operator variable (as bound by 'Lamagraph.Compiler.Eval.defEvalEnv').
stdPlus :: Var
stdPlus = Id (Name (mkLongident (stdPrefix :| ["+"])))

-- | The @#std.-@ operator variable.
stdMinus :: Var
stdMinus = Id (Name (mkLongident (stdPrefix :| ["-"])))

-- | The @#std.*@ operator variable.
stdTimes :: Var
stdTimes = Id (Name (mkLongident (stdPrefix :| ["*"])))
