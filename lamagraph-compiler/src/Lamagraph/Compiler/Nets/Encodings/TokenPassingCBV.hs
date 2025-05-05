module Lamagraph.Compiler.Nets.Encodings.TokenPassingCBV (
  TokenPassingCBV,
  coreBindsToTokenPassingCBV,
  coreExprToTokenPassingCBV,
  tokenPassingCBVRule,
  tokenPassingCBVRuleParallel,
) where

import Relude hiding (one)

import Data.Foldable.Extra
import Data.HashMap.Strict qualified as HashMap
import Data.List.NonEmpty.Extra qualified as NE
import Data.Map.Strict qualified as Map
import UnliftIO.Exception

import Lamagraph.Compiler.Core qualified as C
import Lamagraph.Compiler.MonadFresh
import Lamagraph.Compiler.Nets.Types qualified as N
import Lamagraph.Compiler.Nets.Utils
import Lamagraph.Compiler.Syntax
import Lamagraph.Compiler.Typechecker.TcTypes

data TokenPassingCBV
  = {- | ⇓

    Arity: 1
    -}
    DownArrow
  | {- | ⇑

    Arity: 1
    -}
    UpArrow
  | {- | λ

    Arity: 2

    Invariant: left port is the variable bound by abstraction, right port is the body of abstraction
    -}
    Lambda
  | {- | a

    Arity: 2

    Invariant: left port is the left term in the application, right port is the right term in the application
    -}
    A
  | {- | @

    Intermediate version of a.
    -}
    At
  | {- | @'

    Intermediate version of @.
    -}
    At'
  | {- | ϵ

    Arity: 0
    -}
    Epsilon
  | {- | c

    Arity: 2
    -}
    C
  | {- | δ

    Intermediate version of c.
    -}
    Delta
  deriving (Show)

-- Internal aliases, DO NOT export
type Term = N.Term TokenPassingCBV

----------------------------------------------
-- Constructor functions with correct arity --
----------------------------------------------

downArrow :: Term -> Term
downArrow aux = N.Agent DownArrow [aux]

upArrow :: Term -> Term
upArrow aux = N.Agent UpArrow [aux]

lambda :: Term -> Term -> Term
lambda var body = N.Agent Lambda [var, body]

a :: Term -> Term -> Term
a left right = N.Agent A [left, right]

at :: Term -> Term -> Term
at left right = N.Agent At [left, right]

at' :: Term -> Term -> Term
at' left right = N.Agent At' [left, right]

epsilon :: Term
epsilon = N.Agent Epsilon []

c :: Term -> Term -> Term
c left right = N.Agent C [left, right]

delta :: Term -> Term -> Term
delta left right = N.Agent Delta [left, right]

-------------------------------------------
-- Main Core to INs conversion functions --
-------------------------------------------

coreExprToTokenPassingCBV ::
  HashMap C.Var (NonEmpty Term) -> C.CoreExpr -> N.INsMachine (HashMap C.Var (NonEmpty Term), Term)
coreExprToTokenPassingCBV varToNet = \case
  C.Var var -> case NE.uncons (varToNet HashMap.! var) of -- It's okay to use unsafe lookup, because variable must be bound, otherwise that a bug
    (inVar, Just inVars) -> pure (HashMap.insert var inVars varToNet, inVar)
    (inVar, Nothing) -> pure (HashMap.delete var varToNet, inVar)
  C.Lit _ -> throwString "Literals aren't supported by this translation scheme"
  C.App lExpr rExpr -> do
    (newVarToNet, leftSubtree) <- coreExprToTokenPassingCBV varToNet lExpr
    (newVarToNet', rightSubtree) <- coreExprToTokenPassingCBV newVarToNet rExpr
    pure (newVarToNet', a leftSubtree rightSubtree)
  C.Lam var body ->
    case countVarInCoreExpr var body of
      0 -> do
        (newVarToNet, lamBody) <- coreExprToTokenPassingCBV varToNet body
        pure (newVarToNet, lambda epsilon lamBody)
      1 -> do
        lamVar <- fresh
        let maybeOldVar = HashMap.lookup var varToNet
            newVarToNet = HashMap.insert var (pure $ N.Var lamVar) varToNet
        (newVarToNet', lamBody) <- coreExprToTokenPassingCBV newVarToNet body
        case maybeOldVar of
          Nothing -> pure (newVarToNet', lambda (N.Var lamVar) lamBody)
          Just oldVar -> pure (HashMap.insert var oldVar newVarToNet', lambda (N.Var lamVar) lamBody)
      n -> do
        (cTree, cVars) <- generateCTree n
        let maybeOldVar = HashMap.lookup var varToNet
            newVarToNet = HashMap.insert var (fmap N.Var cVars) varToNet
        (newVarToNet', lamBody) <- coreExprToTokenPassingCBV newVarToNet body
        case maybeOldVar of
          Nothing -> pure (newVarToNet', lambda cTree lamBody)
          Just oldVar -> pure (HashMap.insert var oldVar newVarToNet', lambda cTree lamBody)
  C.Let{} -> throwString "Lets aren't supported by this translation scheme"
  C.Match{} -> throwString "Match isn't supported by this translation scheme"
  C.Tuple{} -> throwString "Tuples aren't supported by this translation scheme"

coreBindsToTokenPassingCBV ::
  HashMap C.Var (NonEmpty Term) ->
  [Term] ->
  [(Term, Term)] ->
  [C.CoreBind] ->
  N.INsMachine ([Term], [(Term, Term)])
coreBindsToTokenPassingCBV varToNet interface activePairs = \case
  [bind] -> case bind of
    C.NonRec _ body -> do
      (_, term) <- coreExprToTokenPassingCBV varToNet body
      v <- fresh
      pure (N.Var v : interface, (downArrow (N.Var v), term) : activePairs)
    C.Rec{} -> throwString "Let-rec's aren't supported"
  (bind : binds) -> case bind of
    C.NonRec var body -> helper var body binds
    C.Rec ((var, body) :| []) -> do
      let lams = collectLams body
          baseInner = C.App zCombinator (C.Lam var body)
          appliedInner = foldl (\acc x -> C.App acc (C.Var x)) baseInner lams
          newBody = foldr C.Lam appliedInner lams
      helper var newBody binds
    C.Rec{} -> throwString "Mutual let-recs aren't supported"
  [] -> pure (interface, activePairs)
 where
  helper var body binds = case countVarInCoreBinds var binds of
    0 -> do
      (vars, term) <- coreExprToTokenPassingCBV varToNet body
      coreBindsToTokenPassingCBV vars interface ((epsilon, term) : activePairs) binds
    1 -> do
      (vars, term) <- coreExprToTokenPassingCBV varToNet body
      let newVarToNet = HashMap.insert var (pure term) vars
      coreBindsToTokenPassingCBV newVarToNet interface activePairs binds
    n -> do
      (cTree, cVars) <- generateCTree n
      (vars, term) <- coreExprToTokenPassingCBV varToNet body
      let newVarToNet = HashMap.insert var (fmap N.Var cVars) vars
      coreBindsToTokenPassingCBV newVarToNet interface ((cTree, term) : activePairs) binds

-------------
-- Helpers --
-------------

-- | Generates binary tree with N copy agents
generateCTree :: Word -> N.INsMachine (Term, NonEmpty N.Var)
generateCTree n
  | n == 2 = do
      out1 <- fresh
      out2 <- fresh
      pure (c (N.Var out1) (N.Var out2), fromList [out1, out2])
  | n == 3 = do
      out1 <- fresh
      out2 <- fresh
      out3 <- fresh
      pure (c (c (N.Var out1) (N.Var out2)) (N.Var out3), fromList [out1, out2, out3])
  | even n && n > 2 = do
      (leftSubtree, leftVars) <- generateCTree (n `quot` 2)
      (rightSubtree, rightVars) <- generateCTree (n `quot` 2)
      pure (c leftSubtree rightSubtree, leftVars <> rightVars)
  | odd n && n > 3 = do
      (leftSubtree, leftVars) <- generateCTree (n `quot` 2)
      (rightSubtree, rightVars) <- generateCTree (n `quot` 2)
      out <- fresh
      pure (c (c leftSubtree rightSubtree) (N.Var out), pure out <> leftVars <> rightVars)
  | otherwise = throwString "Cannot generate copy"

countVarInCoreExpr :: C.Var -> C.CoreExpr -> Word
countVarInCoreExpr var expr = getSum $ countVarInCoreExprSum var expr

countVarInCoreExprSum :: C.Var -> C.CoreExpr -> Sum Word
countVarInCoreExprSum var = \case
  C.Var v -> if v == var then Sum 1 else Sum 0
  C.Lit _ -> Sum 0
  C.App lExpr rExpr -> countVarInCoreExprSum var lExpr <> countVarInCoreExprSum var rExpr
  C.Lam v body -> if v == var then Sum 0 else countVarInCoreExprSum var body
  C.Let (C.NonRec v binding) body -> if v == var then Sum 0 else countVarInCoreExprSum var binding <> countVarInCoreExprSum var body
  _ -> impureThrow $ stringException "Unsupported"

countVarInCoreBindSum :: C.Var -> C.CoreBind -> Sum Word
countVarInCoreBindSum var = \case
  C.NonRec v body -> if v == var then Sum 0 else countVarInCoreExprSum var body
  C.Rec binds -> foldMap (\(v, body) -> if v == var then Sum 0 else countVarInCoreExprSum var body) binds

countVarInCoreBinds :: C.Var -> [C.CoreBind] -> Word
countVarInCoreBinds var binds = getSum $ countVarInCoreBindsSum var binds

countVarInCoreBindsSum :: C.Var -> [C.CoreBind] -> Sum Word
countVarInCoreBindsSum var = foldMap (countVarInCoreBindSum var)

zCombinator :: C.CoreExpr
zCombinator = C.Lam f (C.App inner inner)
 where
  f = C.Id $ Name $ mkLongident $ pure "f#"
  x = C.Id $ Name $ mkLongident $ pure "x#"
  v = C.Id $ Name $ mkLongident $ pure "v#"
  inner = C.Lam x $ C.App (C.Var f) (C.Lam v (C.App (C.App (C.Var x) (C.Var x)) (C.Var v)))

collectLams :: C.CoreExpr -> [C.Var]
collectLams = \case
  C.Lam var body -> var : collectLams body
  _ -> []

-------------------------------------
-- Abstract machine reduction rule --
-------------------------------------

tokenPassingCBVRule :: N.Rule TokenPassingCBV
tokenPassingCBVRule leftLabel leftAux rightLabel rightAux = case (leftLabel, rightLabel) of
  -- Rule 2: ⇓(x) ⋈ a(⇓(@(y, x)), y)
  --
  -- TeX version: \Downarrow(x) \bowtie a(\Downarrow(@(y, x)), y)
  -- Split variable version: ⇓(x₁) ⋈ a(⇓(@(y₁, x₂)), y₂)
  (DownArrow, A) -> do
    x1 <- fresh
    x2 <- fresh
    y1 <- fresh
    y2 <- fresh
    let leftPairs = zip (annotateTerm <$> leftAux) [mkAnnVar x1]
        rightPairs = zip (annotateTerm <$> rightAux) [annotateTerm $ downArrow $ at (N.Var y1) (N.Var x2), mkAnnVar y2]
    pure (leftPairs <> rightPairs, Map.fromList [(x1, x2), (x2, x1), (y1, y2), (y2, y1)])
  -- Rule 2 (symmetric)
  (A, DownArrow) -> tokenPassingCBVRule rightLabel rightAux leftLabel leftAux
  _ -> tokenPassingCBVCommon leftLabel leftAux rightLabel rightAux

tokenPassingCBVRuleParallel :: N.Rule TokenPassingCBV
tokenPassingCBVRuleParallel leftLabel leftAux rightLabel rightAux = case (leftLabel, rightLabel) of
  -- Rule 2 (parallel version): ⇓(x) ⋈ a(⇓(y), ⇓(@'(x, y)))
  --
  -- TeX version: \Downarrow(x) \bowtie a(\Downarrow(y), \Downarrow(@'(x, y)))
  -- Split variable version: ⇓(x₁) ⋈ a(⇓(y₁), ⇓(@'(x₂, y₂)))
  (DownArrow, A) -> do
    x1 <- fresh
    x2 <- fresh
    y1 <- fresh
    y2 <- fresh
    let leftPairs = zip (annotateTerm <$> leftAux) [mkAnnVar x1]
        rightPairs =
          zip
            (annotateTerm <$> rightAux)
            [annotateTerm $ downArrow $ N.Var y1, annotateTerm $ downArrow $ at' (N.Var x2) (N.Var y2)]
    pure (leftPairs <> rightPairs, Map.fromList [(x1, x2), (x2, x1), (y1, y2), (y2, y1)])
  -- Rule 2 (symmetric)
  (A, DownArrow) -> tokenPassingCBVRuleParallel rightLabel rightAux leftLabel leftAux
  _ -> tokenPassingCBVCommon leftLabel leftAux rightLabel rightAux

tokenPassingCBVCommon :: N.Rule TokenPassingCBV
tokenPassingCBVCommon leftLabel leftAux rightLabel rightAux = case (leftLabel, rightLabel) of
  -- Rule 1: ⇓(⇑(λ(x, y))) ⋈ λ(x, y)
  --
  -- TeX version: \Downarrow(\Uparrow(\lambda(x, y))) \bowtie \lambda(x, y)
  -- Split variable version: ⇓(⇑(λ(x₁, y₁))) ⋈ λ(x₂, y₂)
  (DownArrow, Lambda) -> do
    x1 <- fresh
    x2 <- fresh
    y1 <- fresh
    y2 <- fresh
    let leftPairs = zip (annotateTerm <$> leftAux) [annotateTerm $ upArrow $ lambda (N.Var x1) (N.Var y1)]
        rightPairs = zip (annotateTerm <$> rightAux) [mkAnnVar x2, mkAnnVar y2]
    pure (leftPairs <> rightPairs, Map.fromList [(x1, x2), (x2, x1), (y1, y2), (y2, y1)])
  -- Rule 1 (symmetric)
  (Lambda, DownArrow) -> tokenPassingCBVCommon rightLabel rightAux leftLabel leftAux
  -- Rule 3: λ(x, ⇓(z)) ⋈ @(y, z)
  --
  -- TeX version: \lambda(x, \Downarrow(z)) \bowtie @(y, z)
  -- Split variable version: λ(x, ⇓(z₁)) ⋈ @(y, z₂)
  (Lambda, At) -> do
    x <- fresh
    y <- fresh
    z1 <- fresh
    z2 <- fresh
    let leftPairs = zip (annotateTerm <$> leftAux) [mkAnnVar x, annotateTerm $ downArrow (N.Var z1)]
        rightPairs = zip (annotateTerm <$> rightAux) [mkAnnVar y, mkAnnVar z2]
    pure (leftPairs <> rightPairs, Map.fromList [(x, y), (y, x), (z1, z2), (z2, z1)])
  -- Rule 3 (symmetric)
  (At, Lambda) -> tokenPassingCBVCommon rightLabel rightAux leftLabel leftAux
  -- Rule 4: ⇑(x) ⋈ @(⇓(@'(y, x)), y)
  --
  -- TeX version: \Uparrow(x) \bowtie @(\Downarrow(@'(y, x)), y)
  -- Split variable version: ⇑(x₁) ⋈ @(⇓(@'(y₁, x₂)), y₂)
  (UpArrow, At) -> do
    x1 <- fresh
    x2 <- fresh
    y1 <- fresh
    y2 <- fresh
    let leftPairs = zip (annotateTerm <$> leftAux) [mkAnnVar x1]
        rightPairs = zip (annotateTerm <$> rightAux) [annotateTerm $ downArrow $ at' (N.Var y1) (N.Var x2), mkAnnVar y2]
    pure (leftPairs <> rightPairs, Map.fromList [(x1, x2), (x2, x1), (y1, y2), (y2, y1)])
  -- Rule 4 (symmetric)
  (At, UpArrow) -> tokenPassingCBVCommon rightLabel rightAux leftLabel leftAux
  -- Rule 5: @'(x, @(y, x)) ⋈ ⇑(y)
  --
  -- TeX version: @'(x, @(y, x)) \bowtie \Uparrow(y)
  -- Split variable version: @'(x₁, @(y₁, x₂)) ⋈ ⇑(y₂)
  (At', UpArrow) -> do
    x1 <- fresh
    x2 <- fresh
    y1 <- fresh
    y2 <- fresh
    let leftPairs = zip (annotateTerm <$> leftAux) [mkAnnVar x1, annotateTerm $ at (N.Var y1) (N.Var x2)]
        rightPairs = zip (annotateTerm <$> rightAux) [mkAnnVar y2]
    pure (leftPairs <> rightPairs, Map.fromList [(x1, x2), (x2, x1), (y1, y2), (y2, y1)])
  -- Rule 5 (symmetric)
  (UpArrow, At') -> tokenPassingCBVCommon rightLabel rightAux leftLabel leftAux
  -- Rule 6: ϵ ⋈ α(ϵ, ..., ϵ)
  --
  -- TeX version: \epsilonup \bowtie \alpha(\epsilonup, ..., \epsilonup)
  (Epsilon, _) ->
    let rightPairs = zip (annotateTerm <$> rightAux) $ map (\_ -> annotateTerm epsilon) rightAux
     in pure (rightPairs, Map.empty)
  -- Rule 6 (symmetric)
  (_, Epsilon) -> tokenPassingCBVCommon rightLabel rightAux leftLabel leftAux
  -- Rule 7: c(λ(x, y), λ(u, v)) ⋈ λ(δ(u, x), δ(v, y))
  --
  -- TeX version: c(\lambda(x, y), \lambda(u, v)) \bowtie \lambda(\delta(u, x), \delta(v, y))
  -- Split variable version: c(λ(x₁, y₁), λ(u₁, v₁)) ⋈ λ(δ(u₂, x₂), δ(v₂, y₂))
  (C, Lambda) -> do
    x1 <- fresh
    x2 <- fresh
    y1 <- fresh
    y2 <- fresh
    u1 <- fresh
    u2 <- fresh
    v1 <- fresh
    v2 <- fresh
    let leftPairs =
          zip
            (annotateTerm <$> leftAux)
            [annotateTerm $ lambda (N.Var x1) (N.Var y1), annotateTerm $ lambda (N.Var u1) (N.Var v1)]
        rightPairs =
          zip
            (annotateTerm <$> rightAux)
            [annotateTerm $ delta (N.Var u2) (N.Var x2), annotateTerm $ delta (N.Var v2) (N.Var y2)]
    pure
      (leftPairs <> rightPairs, Map.fromList [(x1, x2), (x2, x1), (y1, y2), (y2, y1), (u1, u2), (u2, u1), (v1, v2), (v2, v1)])
  -- Rule 7 (symmetric)
  (Lambda, C) -> tokenPassingCBVCommon rightLabel rightAux leftLabel leftAux
  -- Rule 8
  (C, label) ->
    case length rightAux of
      -- c(α, α) ⋈ α
      --
      -- TeX version: c(\alpha, \alpha) \bowtie \alpha
      0 -> do
        let agent = annotateTerm $ N.Agent label []
            leftPairs = zip (annotateTerm <$> leftAux) [agent, agent]
        pure (leftPairs, Map.empty)
      -- c(α(y), α(x)) ⋈ α(c(y, x))
      --
      -- TeX version: c(\alpha(y), \alpha(x)) \bowtie \alpha(c(y,x))
      -- Split variable version: c(α(y₁), α(x₁)) ⋈ α(c(y₂,x₂))
      1 -> do
        x1 <- fresh
        x2 <- fresh
        y1 <- fresh
        y2 <- fresh
        let agent out = annotateTerm $ N.Agent label [out]
            leftPairs = zip (annotateTerm <$> leftAux) [agent (N.Var y1), agent (N.Var x1)]
            rightPairs = zip (annotateTerm <$> rightAux) [annotateTerm $ c (N.Var y2) (N.Var x2)]
        pure (leftPairs <> rightPairs, Map.fromList [(x1, x2), (x2, x1), (y1, y2), (y2, y1)])
      -- c(α(u, v), α(x, y)) ⋈ α(c(u, x), c(v, y))
      --
      -- TeX version: c(\alpha(u, v), \alpha(x, y)) \bowtie \alpha(c(u, x), c(v, y))
      -- Split variable version: c(α(u₁, v₁), α(x₁, y₁)) ⋈ α(c(u₂, x₂), c(v₂, y₂))
      2 -> do
        x1 <- fresh
        x2 <- fresh
        y1 <- fresh
        y2 <- fresh
        u1 <- fresh
        u2 <- fresh
        v1 <- fresh
        v2 <- fresh
        let agent left right = annotateTerm $ N.Agent label [left, right]
            leftPairs = zip (annotateTerm <$> leftAux) [agent (N.Var u1) (N.Var v1), agent (N.Var x1) (N.Var y1)]
            rightPairs = zip (annotateTerm <$> rightAux) [annotateTerm $ c (N.Var u2) (N.Var x2), annotateTerm $ c (N.Var v2) (N.Var y2)]
        pure
          (leftPairs <> rightPairs, Map.fromList [(x1, x2), (x2, x1), (y1, y2), (y2, y1), (u1, u2), (u2, u1), (v1, v2), (v2, v1)])
      _ -> throwString "Arities other than 0-1-2 for a C agent aren't supported."
  -- Rule 8 (symmetric)
  (_, C) -> tokenPassingCBVCommon rightLabel rightAux leftLabel leftAux
  -- Rule 9: δ(y, x) ⋈ δ(u,v)
  --
  -- TeX version: \delta(y, x) \bowtie \delta(u,v)
  -- Split variable version: δ(y, x) ⋈ δ(u,v)
  (Delta, Delta) -> do
    y <- fresh
    x <- fresh
    u <- fresh
    v <- fresh
    let leftPairs = zip (annotateTerm <$> leftAux) [mkAnnVar y, mkAnnVar x]
        rightPairs = zip (annotateTerm <$> rightAux) [mkAnnVar u, mkAnnVar v]
    pure (leftPairs <> rightPairs, Map.fromList [(x, v), (v, x), (u, y), (y, u)])
  -- Rule 10
  (Delta, label) -> case length rightAux of
    -- δ(α, α) ⋈ α
    --
    -- TeX version: c(\alpha, \alpha) \bowtie \alpha
    0 -> do
      let agent = annotateTerm $ N.Agent label []
          leftPairs = zip (annotateTerm <$> leftAux) [agent, agent]
      pure (leftPairs, Map.empty)
    -- δ(α(y), α(x)) ⋈ δ(c(y, x))
    --
    -- TeX version: \delta(\alpha(y), \alpha(x)) \bowtie \alpha(\delta(y,x))
    -- Split variable version: δ(α(y₁), α(x₁)) ⋈ α(δ(y₂,x₂))
    1 -> do
      x1 <- fresh
      x2 <- fresh
      y1 <- fresh
      y2 <- fresh
      let agent out = annotateTerm $ N.Agent label [out]
          leftPairs = zip (annotateTerm <$> leftAux) [agent (N.Var y1), agent (N.Var x1)]
          rightPairs = zip (annotateTerm <$> rightAux) [annotateTerm $ delta (N.Var y2) (N.Var x2)]
      pure (leftPairs <> rightPairs, Map.fromList [(x1, x2), (x2, x1), (y1, y2), (y2, y1)])
    -- δ(α(u, v), α(x, y)) ⋈ α(δ(u, x), δ(v, y))
    --
    -- TeX version: \delta(\alpha(u, v), \alpha(x, y)) \bowtie \alpha(\delta(u, x), \delta(v, y))
    -- Split variable version: δ(α(u₁, v₁), α(x₁, y₁)) ⋈ α(δ(u₂, x₂), δ(v₂, y₂))
    2 -> do
      x1 <- fresh
      x2 <- fresh
      y1 <- fresh
      y2 <- fresh
      u1 <- fresh
      u2 <- fresh
      v1 <- fresh
      v2 <- fresh
      let agent left right = annotateTerm $ N.Agent label [left, right]
          leftPairs = zip (annotateTerm <$> leftAux) [agent (N.Var u1) (N.Var v1), agent (N.Var x1) (N.Var y1)]
          rightPairs =
            zip (annotateTerm <$> rightAux) [annotateTerm $ delta (N.Var u2) (N.Var x2), annotateTerm $ delta (N.Var v2) (N.Var y2)]
      pure
        (leftPairs <> rightPairs, Map.fromList [(x1, x2), (x2, x1), (y1, y2), (y2, y1), (u1, u2), (u2, u1), (v1, v2), (v2, v1)])
    _ -> throwString "Arities other than 0-1-2 for a Delta agent aren't supported."
  -- Rule 10 (symmetric)
  (_, Delta) -> tokenPassingCBVCommon rightLabel rightAux leftLabel leftAux
  (l, r) -> throwString $ "Cant reduce " <> show l <> " and " <> show r
