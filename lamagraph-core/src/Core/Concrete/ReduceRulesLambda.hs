{-# OPTIONS_GHC -Wno-unused-imports #-}

{- | All modules from Concrete are

1. Compiled from some dsl in separate file

2. Compiled from embedded dsl in `INet.Net` module

3. Written by hands for tests
-}
module Core.Concrete.ReduceRulesLambda where

import Clash.Prelude
import Control.Lens hiding (Index, imap)
import Core.MemoryManager.NodeChanges
import Core.Node
import Core.Reducer
import Data.Maybe
import INet.Net

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
      portsToEdgeEnds node = map (fromMaybe (error "All Ports must to be presented")) (node ^. secondaryPorts)
      lE = portsToEdgeEnds n1
      rE = portsToEdgeEnds n2
      arisingEdges = zipWith (\l r -> Just $ Edge l r) lE (reverse rE)
   in ReduceRuleResult arisingEdges arisingNodes
