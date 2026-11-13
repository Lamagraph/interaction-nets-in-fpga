{- | Translation from the LML core language to the lambda intermediate
representation, and from there to the SKI combinator calculus. The core term
is expanded to a lambda term (inlining non-recursive lets, converting
recursive bindings to 'LRec'\/'LMutRec' markers, and swapping the standard
arithmetic operators for their Church encodings). 'translateCore' converts the
lambda term to SKI via the standard lambda-to-SKI rule (the SKI backend is
non-recursive-only and rejects recursive bindings); 'translateLam' returns the
lambda term with the recursion markers intact, for the YALE net builder
('Lamagraph.Ski.YaleNet.buildYaleNet'). All binders are given fresh, unique
names (via a 'State' counter) so that composing terms never introduces a name
clash.
-}
module Lamagraph.Ski.Translate (
  translateCore,
  translateLam,
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
  andLam,
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
  unitLam,
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
subset are reported via 'UnsupportedConstruct'. Recursive bindings are
rejected: the SKI backend is non-recursive-only (translating a recursive
program would leave a free 'Lamagraph.Ski.Lambda.selfRefName' variable in the
SKI term).
-}
translateCore :: [CoreBind] -> CoreExpr -> Either SkiError Term.Expr
translateCore binds resultExpr =
  fmap toSki (evalState (runExceptT (rejectRec binds resultExpr >> buildLam binds resultExpr)) 0)

{- | Translate a closed LML core program to a 'Lam' term (the intermediate
representation), keeping the 'LRec'\/'LMutRec' markers that the YALE net
builder ('Lamagraph.Ski.YaleNet.buildYaleNet') needs to splice self-references
into cycles. For a non-recursive program, 'toSki' of the result equals
'translateCore'.
-}
translateLam :: [CoreBind] -> CoreExpr -> Either SkiError Lam
translateLam binds resultExpr =
  evalState (runExceptT (buildLam binds resultExpr)) 0

{- | Reject recursive bindings before translation: the SKI backend is
non-recursive-only, and translating a recursive program would leave a free
'selfRefName' variable in the SKI term.
-}
rejectRec :: [CoreBind] -> CoreExpr -> TranslateM ()
rejectRec binds resultExpr =
  mapM_ rejectBind binds >> rejectRecExpr resultExpr
 where
  rejectBind (Rec _) = throwError (UnsupportedConstruct "recursive let")
  rejectBind (NonRec _ e) = rejectRecExpr e
  rejectRecExpr = \case
    Let (Rec _) _ -> throwError (UnsupportedConstruct "recursive let")
    Let (NonRec _ e) body -> rejectRecExpr e >> rejectRecExpr body
    App f x -> rejectRecExpr f >> rejectRecExpr x
    Lam _ body -> rejectRecExpr body
    Match s _ alts -> rejectRecExpr s >> mapM_ (rejectRecExpr . altBody) alts
    Tuple hd tl -> rejectRecExpr hd >> mapM_ rejectRecExpr (toList tl)
    _ -> pure ()

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
inlineBinds tags (Rec recs : bs) lam = do
  lam' <- recBindsToLam tags recs lam
  inlineBinds tags bs lam'

{- | Inline a group of recursive bindings into the surrounding term @bodyLam@,
returning the result.

For a single recursive function @f@, the body is translated with each
self-reference @f@ replaced by 'LSelfRef', and the whole function is wrapped in
'LRec' and inlined into @bodyLam@ (the body's references to @f@ become the
'LRec'). The net builder ('Lamagraph.Ski.YaleNet.buildYaleNet') compiles the
'LRec' to the paper's tie-the-knot cycle (Mackie 1998, §6).

For mutual recursion (more than one binding in the group), each function's body
is translated with its own self-references as 'LSelfRef' and cross-references
to the other functions as 'LVar' (by name); the whole group and @bodyLam@ are
wrapped in 'LMutRec'. The net builder compiles each function to its own output
edge and wires the self- and cross-references back to those edges (a cycle
through 'Lamagraph.Ski.YaleNet.Copy' agents).
-}
recBindsToLam :: ConTags -> NonEmpty (Var, CoreExpr) -> Lam -> TranslateM Lam
recBindsToLam tags recs bodyLam = case toList recs of
  [(f, e)] -> do
    innerLam <- coreExprToLam tags e
    -- Wrap the function in a self-reference abstraction: the knot construction
    -- ('Lamagraph.Ski.YaleNet.buildYaleNet') expects 'LRec (LLam self body)'
    -- where the OUTER lambda is the self-reference (F = λself. body, so that
    -- Y(F) = F (Y F)). Without this wrapper the function's own parameter would
    -- be mistaken for the self-reference, turning e.g. 'let rec f n = n' into
    -- Y (λn. n) (a divergent fixed point of identity) instead of
    -- Y (λself. λn. n) (the identity function).
    selfName <- lift freshName
    let innerLam' = subst (varName f) LSelfRef innerLam
    pure (subst (varName f) (LRec (LLam selfName innerLam')) bodyLam)
  _ -> do
    groups <- recGroupsLam tags recs
    pure (LMutRec groups bodyLam)

{- | Translate the bodies of a mutual-recursion group to @(name, body)@ pairs,
where each body has its own self-references as 'LSelfRef' and cross-references
to the other functions as 'LVar' (by name).
-}
recGroupsLam :: ConTags -> NonEmpty (Var, CoreExpr) -> TranslateM [(String, Lam)]
recGroupsLam tags recs = mapM go (toList recs)
 where
  go (f, e) = do
    bodyLam <- coreExprToLam tags e
    let bodyLam' = subst (varName f) LSelfRef bodyLam
    pure (varName f, bodyLam')

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
    | v == stdPrintInt -> do
        x <- lift freshName
        unitL <- lift unitLam
        pure (LLam x unitL)
    | Map.member (varName v) tags -> conValueLam tags v
    | otherwise -> pure (LVar (varName v))
  Lit (LitInt n) -> lift (churchLam n)
  Lit (LitChar c) -> lift (churchLam (ord c))
  Lit (LitString s) -> litStringLam s
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
  Let (Rec recs) body -> do
    bodyLam <- coreExprToLam tags body
    recBindsToLam tags recs bodyLam
  Match scrutinee _ alts
    | isBoolMatch alts -> do
        scrutLam <- coreExprToLam tags scrutinee
        let (trueAlt, falseAlt) = boolMatchAlts alts
        trueBranch <- coreExprToLam tags (altBody trueAlt)
        falseBranch <- coreExprToLam tags (altBody falseAlt)
        pure (LApp (LApp scrutLam trueBranch) falseBranch)
    | isAdtMatch alts -> adtMatchLam tags scrutinee alts
    | isLitMatch alts -> litMatchLam tags scrutinee alts
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

{- | Encode a string literal as the nested-Church-pair tuple of its characters'
code points (@'\"ab\"' -> pair 97 98@); the empty string is encoded as the unit
placeholder. The shape-directed decoder recovers the string from the code-point
tuple (see 'Lamagraph.Ski.Decode.decodeString').
-}
litStringLam :: Text -> TranslateM Lam
litStringLam s
  | T.null s = lift unitLam
  | otherwise = do
      pairL <- lift pairLam
      cps <- mapM (lift . churchLam . ord) (T.unpack s)
      case nonEmpty cps of
        Nothing -> lift unitLam
        Just ne -> pure (nestPairs pairL ne)

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

{- | Whether a match is a numeric-literal match: every alternative is a
@LitAlt@ on an integer or character literal (both Church-numeral encodings) or
a @DEFAULT@. String-literal alternatives (tuple encodings) are not matched here.
-}
isLitMatch :: NonEmpty CoreMatchAlt -> Bool
isLitMatch alts = all isNumLitAlt (toList alts)
 where
  isNumLitAlt (c, _, _) = case c of
    LitAlt (LitInt _) -> True
    LitAlt (LitChar _) -> True
    DEFAULT -> True
    _ -> False

{- | Translate a numeric-literal match: the scrutinee is a Church numeral (an
integer or a character's code point); dispatch by testing
@isZero (sub scrutinee (lit))@ for each literal alternative and evaluating its
body. Analogous to the ADT tag dispatch, but the scrutinee is the value itself
rather than a @(tag, args)@ pair.
-}
litMatchLam :: ConTags -> CoreExpr -> NonEmpty CoreMatchAlt -> TranslateM Lam
litMatchLam tags scrutinee alts = do
  scrutLam <- coreExprToLam tags scrutinee
  let dispatch [] = lift (churchLam 0)
      dispatch ((LitAlt lit, _, body) : rest) = do
        condLam <- litEqLam scrutLam lit
        thenLam <- coreExprToLam tags body
        elseLam <- dispatch rest
        pure (LApp (LApp condLam thenLam) elseLam)
      dispatch ((DEFAULT, _, body) : _) = coreExprToLam tags body
      dispatch ((other, _, _) : _) = throwError (UnsupportedConstruct ("match alt " <> show other))
  dispatch (toList alts)

{- | The Church bool @(m == n)@. Because 'subLam' saturates at 0,
@isZero (sub m n)@ tests @m <= n@, not @m == n@; true equality is
@isZero (sub m n) AND isZero (sub n m)@.
-}
churchEqLam :: Lam -> Lam -> TranslateM Lam
churchEqLam m n = do
  subL <- lift subLam
  isZeroL <- lift isZeroLam
  andL <- lift andLam
  let le1 = LApp isZeroL (LApp (LApp subL m) n)
      le2 = LApp isZeroL (LApp (LApp subL n) m)
  pure (LApp (LApp andL le1) le2)

-- | The Church bool @(scrutinee == lit)@, via 'churchEqLam'.
litEqLam :: Lam -> Literal -> TranslateM Lam
litEqLam scrutLam lit = do
  litLam <- case lit of
    LitInt n -> lift (churchLam n)
    LitChar c -> lift (churchLam (ord c))
    other -> throwError (UnsupportedConstruct ("literal match on " <> show other))
  churchEqLam scrutLam litLam

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

-- | The Church bool @(tagVal == (tag con))@, via 'churchEqLam'.
eqTagLam :: ConTags -> Lam -> String -> TranslateM Lam
eqTagLam tags tagVal conName = do
  tagL <-
    lift (churchLam (fromMaybe (error (toText ("eqTagLam: unknown constructor " <> conName))) (Map.lookup conName tags)))
  churchEqLam tagVal tagL

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

{- | The @#std.print_int@ variable (a no-op in the SKI pipeline: it returns
unit, and the printed side-effect is ignored by the value comparison).
-}
stdPrintInt :: Var
stdPrintInt = Id (Name (mkLongident (stdPrefix :| ["print_int"])))

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
