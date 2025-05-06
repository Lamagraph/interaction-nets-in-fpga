{-# LANGUAGE PartialTypeSignatures #-}

module Lamagraph.Compiler.Nets.Encodings.TokenPassingCBV where

import Relude hiding (one)

import Data.HashMap.Strict qualified as HashMap
import Data.List.NonEmpty.Extra qualified as NE
import Data.Map.Strict qualified as Map

import Lamagraph.Compiler.Core qualified as C
import Lamagraph.Compiler.MonadFresh
import Lamagraph.Compiler.Nets.Processing
import Lamagraph.Compiler.Nets.Reduction
import Lamagraph.Compiler.Nets.Types qualified as N
import Lamagraph.Compiler.Nets.Utils
import Lamagraph.Compiler.Syntax
import Lamagraph.Compiler.Typechecker.TcTypes
import UnliftIO.Exception

data TokenPassingCBV
  = -- | ⇓
    --
    -- Arity: 1
    DownArrow
  | -- | ⇑
    --
    -- Arity: 1
    UpArrow
  | -- | λ
    --
    -- Arity: 2
    --
    -- Invariant: left port is the variable bound by abstraction, right port is the body of abstraction
    Lambda
  | -- | a
    --
    -- Arity: 2
    --
    -- Invariant: left port is the left term in the application, right port is the right term in the application
    A
  | -- | @
    --
    -- Intermediate version of a.
    At
  | -- | @'
    --
    -- Intermediate version of @.
    At'
  | -- | ϵ
    --
    -- Arity: 0
    Epsilon
  | -- | c
    --
    -- Arity: 2
    C
  | -- | δ
    --
    -- Intermediate version of c.
    Delta
  deriving (Show)

-- Internal aliases, DO NOT export
type Term = N.Term TokenPassingCBV
type AnnTerm = N.AnnTerm TokenPassingCBV

epsilon :: Term
epsilon = N.Agent Epsilon []

copy :: Term -> Term -> Term
copy left right = N.Agent C [left, right]

coreExprToTokenPassingCBV ::
  HashMap C.Var (NonEmpty N.Var) -> C.CoreExpr -> N.INsMachine (HashMap C.Var (NonEmpty N.Var), Term)
coreExprToTokenPassingCBV varToVar = \case
  C.Var var -> case NE.uncons (varToVar HashMap.! var) of -- It's okay to use unsafe lookup, because variable must be bound, otherwise that a bug
    (inVar, Just inVars) -> pure (HashMap.insert var inVars varToVar, N.Var inVar)
    (inVar, Nothing) -> pure (HashMap.delete var varToVar, N.Var inVar)
  C.Lit _ -> error "Literals aren't supported by this translation scheme"
  C.App lExpr rExpr -> do
    (newVarToVar, leftSubtree) <- coreExprToTokenPassingCBV varToVar lExpr
    (newVarToVar', rightSubtree) <- coreExprToTokenPassingCBV newVarToVar rExpr
    pure (newVarToVar', N.Agent A [leftSubtree, rightSubtree])
  C.Lam var body ->
    case countVarInCoreExpr var body of
      0 -> do
        (newVarToVar, lamBody) <- coreExprToTokenPassingCBV varToVar body
        pure (newVarToVar, N.Agent Lambda [epsilon, lamBody])
      1 -> do
        lamVar <- fresh
        let newVarToVar = HashMap.insert var (pure lamVar) varToVar
        (newVarToVar', lamBody) <- coreExprToTokenPassingCBV newVarToVar body
        pure (newVarToVar', N.Agent Lambda [N.Var lamVar, lamBody])
      n -> do
        (cTree, cVars) <- generateCTree n
        let newVarToVar = HashMap.insert var cVars varToVar
        (newVarToVar', lamBody) <- coreExprToTokenPassingCBV newVarToVar body
        pure (newVarToVar', N.Agent Lambda [cTree, lamBody])
  C.Let{} -> error "Lets aren't supported by this translation scheme"
  C.Match{} -> error "Match isn't supported by this translation scheme"
  C.Tuple{} -> error "Tuples aren't supported by this translation scheme"

coreBindsToTokenPassingCBV :: HashMap C.Var (NonEmpty Term) -> [C.CoreBind] -> N.INsMachine _
coreBindsToTokenPassingCBV varToNet binds = undefined
 where
  go (bind : binds) = case bind of
    C.NonRec var body -> do
      undefined
  go [] = undefined

-- | Generates binary tree with N copy agents
generateCTree :: Word -> N.INsMachine (Term, NonEmpty N.Var)
generateCTree n
  | n == 2 = do
      out1 <- fresh
      out2 <- fresh
      pure (copy (N.Var out1) (N.Var out2), fromList [out1, out2])
  | n == 3 = do
      out1 <- fresh
      out2 <- fresh
      out3 <- fresh
      pure (copy (copy (N.Var out1) (N.Var out2)) (N.Var out3), fromList [out1, out2, out3])
  | even n && n > 2 = do
      (leftSubtree, leftVars) <- generateCTree (n `quot` 2)
      (rightSubtree, rightVars) <- generateCTree (n `quot` 2)
      pure (copy leftSubtree rightSubtree, leftVars <> rightVars)
  | odd n && n > 3 = do
      (leftSubtree, leftVars) <- generateCTree (n `quot` 2)
      (rightSubtree, rightVars) <- generateCTree (n `quot` 2)
      out <- fresh
      pure (copy (copy leftSubtree rightSubtree) (N.Var out), pure out <> leftVars <> rightVars)
  | otherwise = error "Cannot generate copy"

countVarInCoreExpr :: C.Var -> C.CoreExpr -> Word
countVarInCoreExpr var expr = getSum $ countVarInCoreExprSum var expr

countVarInCoreExprSum :: C.Var -> C.CoreExpr -> Sum Word
countVarInCoreExprSum var = \case
  C.Var v -> if v == var then Sum 1 else Sum 0
  C.Lit _ -> Sum 0
  C.App lExpr rExpr -> countVarInCoreExprSum var lExpr <> countVarInCoreExprSum var rExpr
  C.Lam v body -> if v == var then Sum 0 else countVarInCoreExprSum var body
  C.Let bind body -> undefined
  _ -> error "Unsupported"

countVarInCoreBind :: C.Var -> C.CoreBind -> Word
countVarInCoreBind var bind = getSum $ countVarInCoreBindSum var bind

countVarInCoreBindSum :: C.Var -> C.CoreBind -> Sum Word
countVarInCoreBindSum var = \case
  C.NonRec v body -> if v == var then Sum 0 else countVarInCoreExprSum var body
  C.Rec{} -> error "let-rec isn't supported."

-- coreExprToTokenPassingCBV :: HashMap C.Var N.Var -> C.CoreExpr -> N.INsMachine (N.Term TokenPassingCBV)
-- coreExprToTokenPassingCBV vars = \case
--   C.Lam var body ->
--     if findVariableInCoreExpr var body
--       then do
--         lamVar <- fresh
--         let newVars = HashMap.insert var lamVar vars
--         lamBody <- coreExprToTokenPassingCBV newVars body
--         pure $ N.Agent Lambda [N.Var lamVar, lamBody]
--       else do
--         lamBody <- coreExprToTokenPassingCBV vars body
--         pure $ N.Agent Lambda [epsilon, lamBody]
--   C.Var var -> pure $ N.Var (vars HashMap.! var)
--   C.App lTerm rTerm -> do
--     let lTermFV = coreFV lTerm
--         rTermFV = coreFV rTerm
--         commonFV = HashSet.toList $ lTermFV `HashSet.intersection` rTermFV
--     freshCommonVars <- mapM (\x -> (x,) <$> fresh) commonFV
--     lInTerm <- coreExprToTokenPassingCBV vars lTerm
--     rInTerm <- coreExprToTokenPassingCBV vars rTerm
--     pure $ N.Agent A [lInTerm, rInTerm]

-- findVariableInCoreExpr :: C.Var -> C.CoreExpr -> Bool
-- findVariableInCoreExpr var = HashSet.member var . coreFV

-- coreFV :: C.CoreExpr -> HashSet C.Var
-- coreFV = \case
--   C.Var var -> HashSet.singleton var
--   C.Lam var body -> HashSet.delete var (coreFV body)
--   C.App lTerm rTerm -> coreFV lTerm `HashSet.union` coreFV rTerm
--   C.Lit _ -> HashSet.empty
--   _ -> error "Unsupported!"

tokenPassingCBVRule :: N.Rule TokenPassingCBV
tokenPassingCBVRule leftLabel leftAux rightLabel rightAux = case (leftLabel, rightLabel) of
  -- Rule 1: ⇓(⇑(λ(x, y))) ⋈ λ(x, y)
  --
  -- TeX version: \Downarrow(\Uparrow(\lambda(x, y))) \bowtie \lambda(x, y)
  -- Split variable version: ⇓(⇑(λ(x₁, y₁))) ⋈ λ(x₂, y₂)
  (DownArrow, Lambda) -> do
    traceM "fired rule 1"
    x1 <- fresh
    x2 <- fresh
    y1 <- fresh
    y2 <- fresh
    let leftPairs =
          zip
            (annotateTerm <$> leftAux)
            [annotateTerm $ N.Agent UpArrow [N.Agent Lambda [N.Var x1, N.Var y1]]]
        rightPairs = zip (annotateTerm <$> rightAux) [mkAnnVar x2, mkAnnVar y2]
    pure (leftPairs <> rightPairs, Map.fromList [(x1, x2), (x2, x1), (y1, y2), (y2, y1)])
  -- Rule 1 (symmetric)
  (Lambda, DownArrow) -> tokenPassingCBVRule rightLabel rightAux leftLabel leftAux
  -- Rule 2: ⇓(x) ⋈ a(⇓(@(y, x)), y)
  --
  -- TeX version: \Downarrow(x) \bowtie a(\Downarrow(@(y, x)), y)
  -- Split variable version: ⇓(x₁) ⋈ a(⇓(@(y₁, x₂)), y₂)
  (DownArrow, A) -> do
    traceM "fired rule 2"
    x1 <- fresh
    x2 <- fresh
    y1 <- fresh
    y2 <- fresh
    let leftPairs = zip (annotateTerm <$> leftAux) [mkAnnVar x1]
        rightPairs = zip (annotateTerm <$> rightAux) [annotateTerm $ N.Agent DownArrow [N.Agent At [N.Var y1, N.Var x2]], mkAnnVar y2]
    pure (leftPairs <> rightPairs, Map.fromList [(x1, x2), (x2, x1), (y1, y2), (y2, y1)])
  -- Rule 2 (symmetric)
  (A, DownArrow) -> tokenPassingCBVRule rightLabel rightAux leftLabel leftAux
  -- Rule 3: λ(x, ⇓(z)) ⋈ @(y, z)
  --
  -- TeX version: \lambda(x, \Downarrow(z)) \bowtie @(y, z)
  -- Split variable version: λ(x, ⇓(z₁)) ⋈ @(y, z₂)
  (Lambda, At) -> do
    traceM "fired rule 3"
    x <- fresh
    y <- fresh
    z1 <- fresh
    z2 <- fresh
    let leftPairs = zip (annotateTerm <$> leftAux) [mkAnnVar x, annotateTerm $ N.Agent DownArrow [N.Var z1]]
        rightPairs = zip (annotateTerm <$> rightAux) [mkAnnVar y, mkAnnVar z2]
    pure (leftPairs <> rightPairs, Map.fromList [(x, y), (y, x), (z1, z2), (z2, z1)])
  -- Rule 3 (symmetric)
  (At, Lambda) -> tokenPassingCBVRule rightLabel rightAux leftLabel leftAux
  -- Rule 4: ⇑(x) ⋈ @(⇓(@'(y, x)), y)
  --
  -- TeX version: \Uparrow(x) \bowtie @(\Downarrow(@'(y, x)), y)
  -- Split variable version: ⇑(x₁) ⋈ @(⇓(@'(y₁, x₂)), y₂)
  (UpArrow, At) -> do
    traceM "fired rule 4"
    x1 <- fresh
    x2 <- fresh
    y1 <- fresh
    y2 <- fresh
    let leftPairs = zip (annotateTerm <$> leftAux) [mkAnnVar x1]
        rightPairs = zip (annotateTerm <$> rightAux) [annotateTerm $ N.Agent DownArrow [N.Agent At' [N.Var y1, N.Var x2]], mkAnnVar y2]
    pure (leftPairs <> rightPairs, Map.fromList [(x1, x2), (x2, x1), (y1, y2), (y2, y1)])
  -- Rule 4 (symmetric)
  (At, UpArrow) -> tokenPassingCBVRule rightLabel rightAux leftLabel leftAux
  -- Rule 5: @'(x, @(y, x)) ⋈ ⇑(y)
  --
  -- TeX version: @'(x, @(y, x)) \bowtie \Uparrow(y)
  -- Split variable version: @'(x₁, @(y₁, x₂)) ⋈ ⇑(y₂)
  (At', UpArrow) -> do
    traceM "fired rule 5"
    x1 <- fresh
    x2 <- fresh
    y1 <- fresh
    y2 <- fresh
    let leftPairs = zip (annotateTerm <$> leftAux) [mkAnnVar x1, annotateTerm $ N.Agent At [N.Var y1, N.Var x2]]
        rightPairs = zip (annotateTerm <$> rightAux) [mkAnnVar y2]
    pure (leftPairs <> rightPairs, Map.fromList [(x1, x2), (x2, x1), (y1, y2), (y2, y1)])
  -- Rule 5 (symmetric)
  (UpArrow, At') -> tokenPassingCBVRule rightLabel rightAux leftLabel leftAux
  -- Rule 6: ϵ ⋈ α(ϵ, ..., ϵ)
  --
  -- TeX version: \epsilonup \bowtie \alpha(\epsilonup, ..., \epsilonup)
  (Epsilon, _) ->
    let rightPairs = zip (annotateTerm <$> rightAux) $ map (\_ -> annotateTerm $ N.Agent Epsilon []) rightAux
     in pure (rightPairs, Map.empty)
  -- Rule 6 (symmetric)
  (_, Epsilon) -> tokenPassingCBVRule rightLabel rightAux leftLabel leftAux
  -- Rule 7: c(λ(x, y), λ(u, v)) ⋈ λ(δ(u, x), δ(v, y))
  --
  -- TeX version: c(\lambda(x, y), \lambda(u, v)) \bowtie \lambda(\delta(u, x), \delta(v, y))
  -- Split variable version: c(λ(x₁, y₁), λ(u₁, v₁)) ⋈ λ(δ(u₂, x₂), δ(v₂, y₂))
  (C, Lambda) -> do
    traceM "fired rule 7"
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
            [annotateTerm $ N.Agent Lambda [N.Var x1, N.Var y1], annotateTerm $ N.Agent Lambda [N.Var u1, N.Var v1]]
        rightPairs =
          zip
            (annotateTerm <$> rightAux)
            [annotateTerm $ N.Agent Delta [N.Var u2, N.Var x2], annotateTerm $ N.Agent Delta [N.Var v2, N.Var y2]]
    pure
      (leftPairs <> rightPairs, Map.fromList [(x1, x2), (x2, x1), (y1, y2), (y2, y1), (u1, u2), (u2, u1), (v1, v2), (v2, v1)])
  -- Rule 7 (symmetric)
  (Lambda, C) -> tokenPassingCBVRule rightLabel rightAux leftLabel leftAux
  -- Rule 9: δ(y, x) ⋈ δ(u,v)
  --
  -- TeX version: \delta(y, x) \bowtie \delta(u,v)
  -- Split variable version: δ(y, x) ⋈ δ(u,v)
  (Delta, Delta) -> do
    traceM "fired rule 9"
    y <- fresh
    x <- fresh
    u <- fresh
    v <- fresh
    let leftPairs = zip (annotateTerm <$> leftAux) [mkAnnVar y, mkAnnVar x]
        rightPairs = zip (annotateTerm <$> rightAux) [mkAnnVar u, mkAnnVar v]
    pure (leftPairs <> rightPairs, Map.fromList [(x, v), (v, x), (u, y), (y, u)])
  (l, r) -> throwString $ "Cant reduce " <> show l <> " and " <> show r

--------------
-- Examples --
--------------

x :: C.Var
x = C.Id $ Name $ mkLongident $ pure "x"
y :: C.Var
y = C.Id $ Name $ mkLongident $ pure "y"
z :: C.Var
z = C.Id $ Name $ mkLongident $ pure "z"

t1 :: C.Var
t1 = C.Id $ Name $ mkLongident $ pure "t1"
t2 :: C.Var
t2 = C.Id $ Name $ mkLongident $ pure "t2"

m = C.Id $ Name $ mkLongident $ pure "m"
n = C.Id $ Name $ mkLongident $ pure "n"

true :: C.CoreExpr
true = C.Lam x $ C.Lam y (C.Var x)

idCore :: C.CoreExpr
idCore = C.Lam x (C.Var x)

idToIdCore :: C.CoreExpr
idToIdCore = C.App idCore idCore

one :: C.CoreExpr
one = C.Lam x $ C.Lam y $ C.App (C.Var x) (C.Var y)

two :: C.CoreExpr
two = C.Lam x $ C.Lam y $ C.App (C.Var x) $ C.App (C.Var x) (C.Var y)

three :: C.CoreExpr
three = C.Lam x $ C.Lam y $ C.App (C.Var x) $ C.App (C.Var x) $ C.App (C.Var x) (C.Var y)

plus :: C.CoreExpr
plus =
  C.Lam m $ C.Lam n $ C.Lam y $ C.Lam x $ C.App (C.App (C.Var m) (C.Var y)) (C.App (C.App (C.Var n) (C.Var y)) (C.Var x))

twoPlusThree :: C.CoreExpr
twoPlusThree = C.App (C.App (C.App (C.App plus two) three) idCore) idCore

onePlusOne :: C.CoreExpr
onePlusOne = C.App (C.App plus one) one

testBinds :: [(C.Var, C.CoreExpr)]
testBinds = [(t1, idCore), (t2, C.App true idCore)]

coreToIN core = snd <$> coreExprToTokenPassingCBV HashMap.empty core

coreToNet core = do
  t <- coreToIN core
  outVar <- fresh
  pure N.Net{N.terms = [N.Var outVar], N.equations = [(N.Agent DownArrow [N.Var outVar], t)]}

coreToResNet core = coreToNet core >>= netToConfiguration >>= reduce tokenPassingCBVRule >>= update >>= configurationToNet

r = reduceStep tokenPassingCBVRule
