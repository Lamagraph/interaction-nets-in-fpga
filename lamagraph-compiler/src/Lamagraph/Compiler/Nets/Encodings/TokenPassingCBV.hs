module Lamagraph.Compiler.Nets.Encodings.TokenPassingCBV where

import Relude

import Data.HashMap.Strict qualified as HashMap
import Data.HashSet qualified as HashSet

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
    C
  | -- | δ
    Delta
  deriving (Show)

epsilon :: N.Term TokenPassingCBV
epsilon = N.Agent Epsilon []

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

coreExprToTokenPassingCBV :: HashMap C.Var N.Var -> C.CoreExpr -> N.INsMachine (N.Term TokenPassingCBV)
coreExprToTokenPassingCBV vars = \case
  C.Lam var body ->
    if findVariableInCoreExpr var body
      then do
        lamVar <- fresh
        let newVars = HashMap.insert var lamVar vars
        lamBody <- coreExprToTokenPassingCBV newVars body
        pure $ N.Agent Lambda [N.Var lamVar, lamBody]
      else do
        lamBody <- coreExprToTokenPassingCBV vars body
        pure $ N.Agent Lambda [epsilon, lamBody]
  C.Var var -> pure $ N.Var (vars HashMap.! var)
  C.App lTerm rTerm -> do
    let lTermFV = coreFV lTerm
        rTermFV = coreFV rTerm
        commonFV = HashSet.toList $ lTermFV `HashSet.intersection` rTermFV
    freshCommonVars <- mapM (\x -> (x,) <$> fresh) commonFV
    lInTerm <- coreExprToTokenPassingCBV vars lTerm
    rInTerm <- coreExprToTokenPassingCBV vars rTerm
    pure $ N.Agent A [lInTerm, rInTerm]
  C.Lit _ -> error "Literals aren't supported by this translation scheme"
  C.Let{} -> error "Lets aren't supported by this translation scheme"
  C.Match{} -> error "Match isn't supported by this translation scheme"
  C.Tuple{} -> error "Tuples aren't supported by this translation scheme"

findVariableInCoreExpr :: C.Var -> C.CoreExpr -> Bool
findVariableInCoreExpr var = HashSet.member var . coreFV

coreFV :: C.CoreExpr -> HashSet C.Var
coreFV = \case
  C.Var var -> HashSet.singleton var
  C.Lam var body -> HashSet.delete var (coreFV body)
  C.App lTerm rTerm -> coreFV lTerm `HashSet.union` coreFV rTerm
  C.Lit _ -> HashSet.empty
  _ -> error "Unsupported!"
