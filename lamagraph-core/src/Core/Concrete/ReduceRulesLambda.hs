{- | All modules from Concrete are

1. Compiled from some dsl in separate file

2. Compiled from embedded dsl in `INet.Net` module

3. Written by hands for tests
-}
module Core.Concrete.ReduceRulesLambda where

import Clash.Prelude
import Control.Lens hiding (Index, imap)
import Core.MemoryManager
import Core.Node
import Core.Reducer
import INet.Net

-- | One reduce step
(|><|) ::
  Node 2 ->
  Node 2 ->
  ReduceRuleResult 2 2 2
lNode |><| rNode = case (lNode ^. nodeType, rNode ^. nodeType) of
  (Apply, Abs) -> applyToLambdaRule lNode rNode
  (Abs, Apply) -> applyToLambdaRule lNode rNode
  (Eps, _) -> epsToAnyRule lNode rNode
  (_, Eps) -> epsToAnyRule rNode lNode
  _ -> error "There is no rule for this active pair in the reduction rules"

{- | Reduce rule for `Apply` and `Abs`

<<docs/apply_to_lambda_rule.svg>>
-}
applyToLambdaRule ::
  (KnownNat nodesNumber) =>
  Node 2 ->
  Node 2 ->
  ReduceRuleResult nodesNumber 2 2
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

{- | Reduce rule for `Eps` and everything else.

<<docs/eps_apply_rule.svg>>
-}
epsToAnyRule ::
  (KnownNat portsNumber, KnownNat edgesNumber, CLog 2 portsNumber <= BitSize AddressNumber, 1 <= portsNumber) =>
  Node portsNumber ->
  Node portsNumber ->
  ReduceRuleResult portsNumber edgesNumber portsNumber
epsToAnyRule _ nSome =
  let arisingEdges = def
      genNewEpsNode port = Node port def Eps
      arisingNodes =
        imap
          (\i maybePort -> flip LocalNode (indexToUnsigned i) . genNewEpsNode <$> maybePort)
          (nSome ^. secondaryPorts)
   in ReduceRuleResult arisingEdges arisingNodes
