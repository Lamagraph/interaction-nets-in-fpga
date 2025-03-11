{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}

{- | All modules from Concrete are

1. Compiled from some dsl in separate file

2. Compiled from embedded dsl in `INet.Net` module

3. Written by hands for tests
-}
module Core.Concrete.ReduceRulesLambda where

import Clash.Prelude
import Control.Lens hiding (Index, imap, (:>))
import Core.Concrete.TypesTH
import Core.MemoryManager.NodeChanges
import Core.Node
import Data.Maybe
import INet.Net

data AgentSimpleLambda
  = Apply
  | Abstract
  | Erase
  deriving (NFDataX, Generic, Show, Eq, ShowX)

{- | Reduce rule for `Apply` and `Abs`

<<docs/apply_to_lambda_rule.svg>>
-}
applyToLambdaRule ::
  Vec 2 (Maybe AddressNumber) ->
  LoadedNode 2 AgentSimpleLambda ->
  LoadedNode 2 AgentSimpleLambda ->
  ReduceRuleResult 2 2 2 AgentSimpleLambda
applyToLambdaRule _ loadedNodeLeft loadedNodeRight =
  let arisingNodes = def
      portsToEdgeEnds node = map (fromMaybe (error "Agent type of the node is incorrect")) (node ^. containedNode . secondaryPorts)
      lE = portsToEdgeEnds loadedNodeLeft
      rE = portsToEdgeEnds loadedNodeRight
      arisingEdges = zipWith (\l r -> Just $ Edge l r) lE rE
   in ReduceRuleResult arisingEdges arisingNodes

{- | Reduce rule fot `Erase` and `Abstract` or `Apply`

<<docs/eps_apply_rule.svg>>

__Note__ this rule is not symmetric!! Erasure node is first
-}
eraseToAbstractOrApplyRule ::
  Vec 2 (Maybe AddressNumber) ->
  LoadedNode 2 AgentSimpleLambda ->
  LoadedNode 2 AgentSimpleLambda ->
  ReduceRuleResult 2 2 2 AgentSimpleLambda
eraseToAbstractOrApplyRule ((Just address1) :> (Just address2) :> Nil) _ loadedNodeAny =
  let arisingEdges = def :: Vec 2 (Maybe (Edge 2))
      genEraseNodeByConnection address connection = LoadedNode (Node connection def Erase) address
      arisingNodes =
        zipWith
          ( \address maybeConnection ->
              Just $
                maybe
                  (error "Agent type of the node is incorrect")
                  (genEraseNodeByConnection address)
                  maybeConnection
          )
          (address1 :> address2 :> Nil)
          (loadedNodeAny ^. containedNode . secondaryPorts)
   in ReduceRuleResult arisingEdges arisingNodes
eraseToAbstractOrApplyRule _ _ _ = error "The amount of required memory is incorrectly specified"

-- | Reduce rule for `Erase` and `Erase`
eraseToErase ::
  Vec 2 (Maybe AddressNumber) ->
  LoadedNode 2 AgentSimpleLambda ->
  LoadedNode 2 AgentSimpleLambda ->
  ReduceRuleResult 2 2 2 AgentSimpleLambda
eraseToErase _ _ _ = ReduceRuleResult def def

instance INet AgentSimpleLambda 65536 2 2 2 where
  getReduceRuleInfo agent1 agent2 = case (agent1, agent2) of
    (Apply, Abstract) -> ReduceFunctionInfo applyToLambdaRule 0
    (Abstract, Apply) -> ReduceFunctionInfo applyToLambdaRule 0
    (Erase, Erase) -> ReduceFunctionInfo eraseToErase 0
    (Erase, _) -> ReduceFunctionInfo eraseToAbstractOrApplyRule 2
    (_, Erase) -> ReduceFunctionInfo (\addresses nodeAny nodeErase -> eraseToAbstractOrApplyRule addresses nodeErase nodeAny) 2
    (_, _) -> errorX "There is no rule for this pair of agent in reduction rules"

$(makeAgentsType "FulledLambda" ["Apply1", "Abstract1", "Erase1", "Duplicator"] (Just 64))
