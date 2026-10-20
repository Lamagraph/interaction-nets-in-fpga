{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}

{- |
Module      : Core.Concrete.ReduceRulesLambda
Description : Hand-written reduction rules for the 3-agent lambda set

The concrete agent set used by @topEntity@ and most tests: the label type
`AgentSimpleLambda` with three constructors and the instance
@INet AgentSimpleLambda 2 2 2@ — beta, erasure and erase-erase rules written
by hand, each allocating at most 2 fresh cells.

__Note__ the name @AgentSimpleLambda@ is used twice in the project for two
/different/ types: this module's three-constructor type (@Apply@, @Abstract@,
@Erase@) and the four-constructor type of the same name in "INet.Define".
@topEntity@ is fixed to the type declared here.
-}
module Core.Concrete.ReduceRulesLambda where

import Clash.Prelude
import Control.Lens hiding (Index, imap, (:>))
import Core.Node
import Data.Maybe
import INet.Net

data AgentSimpleLambda
  = Apply
  | Abstract
  | Erase
  deriving (NFDataX, Generic, Show, Eq, ShowX)

{- | Reduce rule for `Apply` and `Abstract` (beta): the two secondary ports are
wired pairwise, no fresh nodes arise (the free-address vector is ignored).

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

{- | Reduce rule for `Erase` and `Abstract` or `Apply`: the eraser is replaced
by one fresh eraser per secondary port of the surviving agent, taking over
its connections; the surviving node itself is not copied. Requires exactly
two free addresses — fewer aborts with an error, more is not checked (the
instance's `getAddressesAllocationCount` supplies exactly two).

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

instance INet AgentSimpleLambda 2 2 2 where
  getAddressesAllocationCount :: AgentSimpleLambda -> AgentSimpleLambda -> Index CellsNumber
  getAddressesAllocationCount left right = case (left, right) of
    (Apply, Abstract) -> 0
    (Abstract, Apply) -> 0
    (Erase, Erase) -> 0
    (Erase, _) -> 2
    (_, Erase) -> 2
    _ -> errorX "No rule for this pair"

  makeReduction ::
    Vec 2 (Maybe AddressNumber) ->
    LoadedNode 2 AgentSimpleLambda ->
    LoadedNode 2 AgentSimpleLambda ->
    ReduceRuleResult 2 2 2 AgentSimpleLambda
  makeReduction freeAddresses lN rN = case (lN ^. containedNode . nodeType, rN ^. containedNode . nodeType) of
    (Apply, Abstract) -> applyToLambdaRule freeAddresses lN rN
    (Abstract, Apply) -> applyToLambdaRule freeAddresses rN lN
    (Erase, Erase) -> eraseToErase freeAddresses lN rN
    (Erase, _) -> eraseToAbstractOrApplyRule freeAddresses lN rN
    (_, Erase) -> eraseToAbstractOrApplyRule freeAddresses rN lN
    _ -> errorX "No rule for this pair"
