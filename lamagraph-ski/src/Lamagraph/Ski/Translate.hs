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
import Data.Map.Strict qualified as Map
import Data.Text qualified as T

import Lamagraph.Compiler.Core
import Lamagraph.Compiler.ModuleResolver.DefaultEnv (stdPrefix)
import Lamagraph.Compiler.Syntax.Longident (Longident (..), mkLongident)
import Lamagraph.Compiler.Typechecker.TcTypes (Name (..))
import Lamagraph.Ski.Church (
  addLam,
  churchLam,
  falseLam,
  fstLam,
  geLam,
  gtLam,
  isZeroLam,
  leLam,
  ltLam,
  multLam,
  pairLam,
  sndLam,
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
  let tags = collectConTags binds
  resultLam <- coreExprToLam tags resultExpr
  inlineBinds tags (reverse binds) resultLam

inlineBinds :: ConTags -> [CoreBind] -> Lam -> TranslateM Lam
inlineBinds _ [] lam = pure lam
-- A constructor self-binding (@let C = C@) is a self-reference; it is not
-- inlined (a constructor is a function of its fields, not a 0-arg value). The
-- constructor is instead recognised at its use-sites by 'coreExprToLam'.
inlineBinds tags (NonRec v (Var w) : bs) lam
  | v == w = inlineBinds tags bs lam
inlineBinds tags (NonRec v e : bs) lam = do
  repl <- coreExprToLam tags e
  inlineBinds tags bs (subst (varName v) repl lam)
inlineBinds _ (Rec _ : _) _ = throwError (UnsupportedConstruct "recursive let")

coreExprToLam :: ConTags -> CoreExpr -> TranslateM Lam
coreExprToLam tags = \case
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
    | Map.member (varName v) tags -> conValueLam tags v
    | otherwise -> pure (LVar (varName v))
  Lit (LitInt n) -> lift (churchLam n)
  Lit other -> throwError (UnsupportedConstruct ("literal " <> show other))
  App (Var v) x
    | Map.member (varName v) tags -> conAppLam tags v x
    | otherwise -> LApp <$> coreExprToLam tags (Var v) <*> coreExprToLam tags x
  App f x -> LApp <$> coreExprToLam tags f <*> coreExprToLam tags x
  Lam v body -> do
    bodyLam <- coreExprToLam tags body
    fresh <- lift freshName
    pure (LLam fresh (subst (varName v) (LVar fresh) bodyLam))
  Let (NonRec v e) body -> do
    repl <- coreExprToLam tags e
    bodyLam <- coreExprToLam tags body
    pure (subst (varName v) repl bodyLam)
  Let (Rec _) _ -> throwError (UnsupportedConstruct "recursive let")
  Match scrutinee _ alts
    | isBoolMatch alts -> do
        scrutLam <- coreExprToLam tags scrutinee
        let (trueAlt, falseAlt) = boolMatchAlts alts
        trueBranch <- coreExprToLam tags (altBody trueAlt)
        falseBranch <- coreExprToLam tags (altBody falseAlt)
        pure (LApp (LApp scrutLam trueBranch) falseBranch)
    | isAdtMatch alts -> adtMatchLam tags scrutinee alts
    | otherwise -> throwError (UnsupportedConstruct "match")
  Tuple hd tl -> do
    hdLam <- coreExprToLam tags hd
    tlLams <- mapM (coreExprToLam tags) (toList tl)
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

{- | The tag table for ADT constructors: a map from a constructor's name (its
'varName') to a unique Church-numeral tag. Core loses the ADT grouping (which
constructors form one sum type) and the arities, so each constructor is given a
globally-unique tag in source order; a value is the pair @(tag, args)@ and a
match dispatches on the tag. This mirrors the interpreter's tagged @VAdt@.
-}
type ConTags = Map.Map String Int

{- | Collect the constructor self-bindings (@let C = C@) and assign each a tag
in source order.
-}
collectConTags :: [CoreBind] -> ConTags
collectConTags binds = Map.fromList (zip names [0 ..])
 where
  names = [varName v | NonRec v (Var w) <- binds, v == w]

-- | The tag of a constructor.
conTag :: ConTags -> Var -> Int
conTag tags v =
  fromMaybe (error (toText ("conTag: unknown constructor " <> varName v))) (Map.lookup (varName v) tags)

-- | A 0-arity constructor value: @pair (tag C) unit@.
conValueLam :: ConTags -> Var -> TranslateM Lam
conValueLam tags v = do
  pairL <- lift pairLam
  tagL <- lift (churchLam (conTag tags v))
  unitL <- lift trueLam
  pure (LApp (LApp pairL tagL) unitL)

-- | A constructor application @C args@: @pair (tag C) (encodeArgs args)@.
conAppLam :: ConTags -> Var -> CoreExpr -> TranslateM Lam
conAppLam tags v x = do
  pairL <- lift pairLam
  tagL <- lift (churchLam (conTag tags v))
  argsLam <- encodeArgs tags x
  pure (LApp (LApp pairL tagL) argsLam)

{- | Encode the field arguments of a constructor application. A tuple of fields
is encoded as nested Church pairs; a single field is encoded directly.
-}
encodeArgs :: ConTags -> CoreExpr -> TranslateM Lam
encodeArgs tags (Tuple hd tl) = do
  pairL <- lift pairLam
  hdLam <- coreExprToLam tags hd
  tlLams <- mapM (coreExprToLam tags) (toList tl)
  pure (nestPairs pairL (hdLam :| tlLams))
encodeArgs tags x = coreExprToLam tags x

{- | Whether a match is an ADT match: every alternative is a @DataAlt@
(constructor) or a @DEFAULT@.
-}
isAdtMatch :: NonEmpty CoreMatchAlt -> Bool
isAdtMatch alts = all isAdtAlt (toList alts)
 where
  isAdtAlt (c, _, _) = case c of
    DataAlt _ -> True
    DEFAULT -> True
    _ -> False

{- | Translate an ADT match: the scrutinee is the pair @(tag, args)@; dispatch
by testing @tag == (tag C)@ for each constructor alternative, binding the
alternative's fields to the projections of @args@ and evaluating its body.
-}
adtMatchLam :: ConTags -> CoreExpr -> NonEmpty CoreMatchAlt -> TranslateM Lam
adtMatchLam tags scrutinee alts = do
  scrutLam <- coreExprToLam tags scrutinee
  fstL <- lift fstLam
  sndL <- lift sndLam
  let tagVal = LApp fstL scrutLam
      argsVal = LApp sndL scrutLam
      -- The final else-branch is unreachable for an exhaustive match (LML
      -- matches are exhaustive); a placeholder value stands in for it.
      dispatch [] = lift (churchLam 0)
      dispatch ((DataAlt con, fields, body) : rest) = do
        condLam <- eqTagLam tags tagVal (varName (Id con))
        thenLam <- altThenLam tags argsVal fstL sndL fields body
        elseLam <- dispatch rest
        pure (LApp (LApp condLam thenLam) elseLam)
      dispatch ((DEFAULT, _, body) : _) = coreExprToLam tags body
      dispatch ((other, _, _) : _) = throwError (UnsupportedConstruct ("match alt " <> show other))
  dispatch (toList alts)

-- | The Church bool @(tagVal == (tag con))@, i.e. @isZero (sub tagVal (tag con))@.
eqTagLam :: ConTags -> Lam -> String -> TranslateM Lam
eqTagLam tags tagVal conName = do
  subL <- lift subLam
  isZeroL <- lift isZeroLam
  tagL <-
    lift (churchLam (fromMaybe (error (toText ("eqTagLam: unknown constructor " <> conName))) (Map.lookup conName tags)))
  pure (LApp isZeroL (LApp (LApp subL tagVal) tagL))

{- | The then-branch of an ADT alternative: wrap the body in lambdas for the
fields and apply the projections of @args@. For @n@ fields, field @i@ (1-indexed)
is @fst (snd^(i-1) args)@ for @i < n@ and @snd^(n-1) args@ for @i == n@; a single
field is @args@ itself.
-}
altThenLam :: ConTags -> Lam -> Lam -> Lam -> [Var] -> CoreExpr -> TranslateM Lam
altThenLam tags argsVal fstL sndL fields body = do
  bodyLam <- coreExprToLam tags body
  -- The first field is the outermost binder, matching the left-to-right
  -- order of the projections applied below.
  let n = length fields
      wrapped = foldr (LLam . varName) bodyLam fields
      projs = [projectField n i argsVal fstL sndL | i <- [1 .. n]]
  pure (foldl' LApp wrapped projs)

{- | Project the @i@-th field (1-indexed) of a nested-Church-pair tuple of @n@
fields, using the lifted @fst@/@snd@ selectors.
-}
projectField :: Int -> Int -> Lam -> Lam -> Lam -> Lam
projectField n i x fstL sndL
  | i < n = LApp fstL (sndChain (i - 1) x)
  | otherwise = sndChain (n - 1) x
 where
  sndChain 0 y = y
  sndChain k y = LApp sndL (sndChain (k - 1) y)

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

{- | Whether a match is a boolean (if-then-else) match: exactly two @DataAlt@
alternatives for @#std.true@ and @#std.false@, in that order.
-}
isBoolMatch :: NonEmpty CoreMatchAlt -> Bool
isBoolMatch alts =
  case toList alts of
    [(DataAlt d1, _, _), (DataAlt d2, _, _)] ->
      d1 == nameOf stdTrue && d2 == nameOf stdFalse
    _ -> False

{- | Split a boolean match into its (true, false) alternatives. The caller must
ensure 'isBoolMatch' holds.
-}
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
