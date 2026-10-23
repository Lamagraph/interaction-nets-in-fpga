{- | Translation from the LML core language to the SKI combinator calculus.
The core term is expanded to a lambda term (inlining non-recursive lets and
swapping the standard arithmetic operators for their Church encodings), then
converted to SKI via the standard lambda-to-SKI rule. All binders are given
fresh, unique names (via a 'State' counter) so that composing terms never
introduces a name clash.
-}
module Lamagraph.Ski.Encode (
  encode,
  stdPlus,
  stdMinus,
  stdTimes,
) where

import Relude

import Common.Term qualified as Term
import Data.Text qualified as T

import Lamagraph.Compiler.Core
import Lamagraph.Compiler.ModuleResolver.DefaultEnv (stdPrefix)
import Lamagraph.Compiler.Syntax.Longident (Longident (..), mkLongident)
import Lamagraph.Compiler.Typechecker.TcTypes (Name (..))
import Lamagraph.Ski.Church (addLam, churchLam)
import Lamagraph.Ski.Lambda (Lam (..), freshName, subst, toSki)

{- | Translate a closed LML core program (top-level bindings + result
expression) to a SKI term. Minimal subset: integer literals, '+', let,
lambda, application.
-}
encode :: [CoreBind] -> CoreExpr -> Term.Expr
encode binds resultExpr =
  let lam = evalState (buildLam binds resultExpr) 0
   in toSki lam

{- | Build the lambda term for a program: convert the result expression, then
inline the top-level (non-recursive) bindings.
-}
buildLam :: [CoreBind] -> CoreExpr -> State Int Lam
buildLam binds resultExpr = do
  resultLam <- coreExprToLam resultExpr
  inlineBinds binds resultLam

inlineBinds :: [CoreBind] -> Lam -> State Int Lam
inlineBinds [] lam = pure lam
inlineBinds (NonRec v e : bs) lam = do
  repl <- coreExprToLam e
  inlineBinds bs (subst (varName v) repl lam)
inlineBinds (Rec _ : _) _ = error "encode: recursive let is not supported in the minimal subset"

coreExprToLam :: CoreExpr -> State Int Lam
coreExprToLam = \case
  Var v
    | v == stdPlus -> addLam
    | v == stdMinus -> error "encode: '-' is not supported yet (added in S4)"
    | v == stdTimes -> error "encode: '*' is not supported yet (added in S4)"
    | otherwise -> pure (LVar (varName v))
  Lit (LitInt n) -> churchLam n
  Lit _ -> error "encode: non-integer literal not supported in the minimal subset"
  App f x -> LApp <$> coreExprToLam f <*> coreExprToLam x
  Lam v body -> do
    bodyLam <- coreExprToLam body
    fresh <- freshName
    pure (LLam fresh (subst (varName v) (LVar fresh) bodyLam))
  Let (NonRec v e) body -> do
    repl <- coreExprToLam e
    bodyLam <- coreExprToLam body
    pure (subst (varName v) repl bodyLam)
  Let (Rec _) _ -> error "encode: recursive let is not supported in the minimal subset"
  _ -> error "encode: construct not supported in the minimal subset"

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
