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
  stdTrue,
  stdFalse,
  stdGreater,
  stdLess,
  stdGreaterEq,
  stdLessEq,
) where

import Control.Monad.Except
import Relude

import Common.Term qualified as Term
import Data.Text qualified as T

import Lamagraph.Compiler.Core
import Lamagraph.Compiler.ModuleResolver.DefaultEnv (stdPrefix)
import Lamagraph.Compiler.Syntax.Longident (Longident (..), mkLongident)
import Lamagraph.Compiler.Typechecker.TcTypes (Name (..))
import Lamagraph.Ski.Church (
  addLam,
  churchLam,
  falseLam,
  geLam,
  gtLam,
  leLam,
  ltLam,
  multLam,
  pairLam,
  subLam,
  trueLam,
 )
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
inline the top-level (non-recursive) bindings. The bindings are inlined in
reverse source order: a later binding's right-hand side may reference an earlier
binding, so the earlier binding must still be a live variable when the later one
is inlined (otherwise the reference would be left free).
-}
buildLam :: [CoreBind] -> CoreExpr -> TranslateM Lam
buildLam binds resultExpr = do
  resultLam <- coreExprToLam resultExpr
  inlineBinds (reverse binds) resultLam

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
    | v == stdTrue -> lift trueLam
    | v == stdFalse -> lift falseLam
    | v == stdGreater -> lift gtLam
    | v == stdLess -> lift ltLam
    | v == stdGreaterEq -> lift geLam
    | v == stdLessEq -> lift leLam
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
  Match scrutinee _ alts
    | isBoolMatch alts -> do
        scrutLam <- coreExprToLam scrutinee
        let (trueAlt, falseAlt) = boolMatchAlts alts
        trueBranch <- coreExprToLam (altBody trueAlt)
        falseBranch <- coreExprToLam (altBody falseAlt)
        pure (LApp (LApp scrutLam trueBranch) falseBranch)
    | otherwise -> throwError (UnsupportedConstruct "match")
  Tuple hd tl -> do
    hdLam <- coreExprToLam hd
    tlLams <- mapM coreExprToLam (toList tl)
    pairL <- lift pairLam
    pure (nestPairs pairL (hdLam :| tlLams))

{- | Nest a non-empty list of elements into right-associated Church pairs:
@[e1, e2, e3]@ becomes @pair e1 (pair e2 e3)@. A single element is returned
as-is (the base of the nesting). A Core tuple is a head plus a non-empty tail,
so the element list always has length ≥ 2.
-}
nestPairs :: Lam -> NonEmpty Lam -> Lam
nestPairs _ (x :| []) = x
nestPairs pair (x :| (y : ys)) = LApp (LApp pair x) (nestPairs pair (y :| ys))

-- | Render a core variable as a plain string (dot-joined long identifier).
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

-- | The @#std.true@ constructor variable.
stdTrue :: Var
stdTrue = Id (Name (mkLongident (stdPrefix :| ["true"])))

-- | The @#std.false@ constructor variable.
stdFalse :: Var
stdFalse = Id (Name (mkLongident (stdPrefix :| ["false"])))

-- | The @#std.>@ operator variable.
stdGreater :: Var
stdGreater = Id (Name (mkLongident (stdPrefix :| [">"])))

-- | The @#std.<@ operator variable.
stdLess :: Var
stdLess = Id (Name (mkLongident (stdPrefix :| ["<"])))

-- | The @#std.>=@ operator variable.
stdGreaterEq :: Var
stdGreaterEq = Id (Name (mkLongident (stdPrefix :| [">="])))

-- | The @#std.<=@ operator variable.
stdLessEq :: Var
stdLessEq = Id (Name (mkLongident (stdPrefix :| ["<="])))

-- | Whether a match is a boolean (if-then-else) match: exactly two @DataAlt@
-- alternatives for @#std.true@ and @#std.false@, in that order.
isBoolMatch :: NonEmpty CoreMatchAlt -> Bool
isBoolMatch alts =
  case toList alts of
    [(DataAlt d1, _, _), (DataAlt d2, _, _)] ->
      d1 == nameOf stdTrue && d2 == nameOf stdFalse
    _ -> False

-- | Split a boolean match into its (true, false) alternatives. The caller must
-- ensure 'isBoolMatch' holds.
boolMatchAlts :: NonEmpty CoreMatchAlt -> (CoreMatchAlt, CoreMatchAlt)
boolMatchAlts alts =
  case toList alts of
    [t, f] -> (t, f)
    _ -> error "boolMatchAlts: not a boolean match"

-- | The body expression of a match alternative.
altBody :: CoreMatchAlt -> CoreExpr
altBody (_, _, body) = body

-- | The Name carried by a core variable.
nameOf :: Var -> Name
nameOf (Id n) = n
