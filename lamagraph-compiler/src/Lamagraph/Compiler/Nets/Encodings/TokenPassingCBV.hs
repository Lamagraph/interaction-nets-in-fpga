{-# LANGUAGE PartialTypeSignatures #-}

module Lamagraph.Compiler.Nets.Encodings.TokenPassingCBV where

import Relude

import Data.HashMap.Strict qualified as HashMap
import Data.HashSet qualified as HashSet
import Data.List.NonEmpty.Extra qualified as NE

import Lamagraph.Compiler.Core qualified as C
import Lamagraph.Compiler.MonadFresh
import Lamagraph.Compiler.Nets.Types qualified as N
import Lamagraph.Compiler.Syntax
import Lamagraph.Compiler.Typechecker.TcTypes

data TokenPassingCBV
  = -- | ⇓
    --
    -- Arity: 1
    ArrowDown
  | -- | ⇑
    --
    -- Arity: 1
    ArrowUp
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
    At
  | -- | @'
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
    Delta
  deriving (Show)

type Term = N.Term TokenPassingCBV
type AnnTerm = N.AnnTerm TokenPassingCBV

epsilon :: Term
epsilon = N.Agent Epsilon []

copy :: Term -> Term -> Term
copy left right = N.Agent C [left, right]

x :: C.Var
x = C.Id $ Name $ mkLongident $ pure "x"
y :: C.Var
y = C.Id $ Name $ mkLongident $ pure "y"

true :: C.CoreExpr
true = C.Lam x $ C.Lam y (C.Var x)

idCore :: C.CoreExpr
idCore = C.Lam x (C.Var x)

idToIdCore :: C.CoreExpr
idToIdCore = C.App idCore idCore

three :: C.CoreExpr
three = C.Lam x $ C.Lam y $ C.App (C.Var x) $ C.App (C.Var x) $ C.App (C.Var x) (C.Var y)

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
countVarInCoreExpr var expr = getSum $ helper expr
 where
  helper = \case
    C.Var v -> if v == var then Sum 1 else Sum 0
    C.Lit _ -> Sum 0
    C.App lExpr rExpr -> helper lExpr <> helper rExpr
    C.Lam v body -> if v == var then Sum 0 else helper body
    _ -> error "Unsupported"

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
