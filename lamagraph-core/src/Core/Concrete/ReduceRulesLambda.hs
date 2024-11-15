{- | All modules from Concrete are
1a. Compiled from some dsl in separate file
1b. Compiled from embedded dsl in INet module
2.  Written by hands for tests
-}
module Core.Concrete.ReduceRulesLambda where

import Clash.Prelude
import Control.Lens
import Core.MemoryManager
import Core.Node
import Core.Reducer
import INet.Net

(|><|) ::
  -- forall portsNumber nodesNumber edgesNumber.
  -- (KnownNat portsNumber, KnownNat nodesNumber, KnownNat edgesNumber, edgesNumber ~ portsNumber) =>
  -- Node portsNumber ->
  -- Node portsNumber ->
  -- ReduceRuleResult nodesNumber edgesNumber portsNumber
  Node 2 ->
  Node 2 ->
  ReduceRuleResult 0 2 2
lNode |><| rNode = case (lNode ^. nodeType, rNode ^. nodeType) of
  (Apply, Abs) -> applyToLambdaRule lNode rNode
  (Abs, Apply) -> applyToLambdaRule lNode rNode
  _ -> error "There is no rule for this active pair in the reduction rules"
 where
  applyToLambdaRule n1 n2 =
    let arisingNodes = def
        portToEdgeEnd p = case p ^. nodeAddress of
          Nothing -> error "Port must to be connected"
          Just addr -> case addr of
            ActualAddress addrNum -> EdgeEnd addrNum (p ^. portConnectedToId)
            LocalAddress addrNum -> EdgeEnd addrNum (p ^. portConnectedToId) -- Maybe this is should be more complicated
        portsToEdgeEnds node = map (maybe (error "All Ports must to be presented") portToEdgeEnd) (node ^. secondaryPorts)
        lE = portsToEdgeEnds n1
        rE = portsToEdgeEnds n2
        arisingEdges = zipWith (\l r -> Just $ Edge l r) lE (reverse rE)
     in ReduceRuleResult arisingEdges arisingNodes
