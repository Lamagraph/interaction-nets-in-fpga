{- | Rule-level helpers shared by the EDSL-vs-hand-written rule tests.

`rwPair` relabels a fixture pair from "Core.Concrete.ReduceRulesLambda" to the
EDSL label set; `edsOf` runs the EDSL `makeReduction` on such a pair and
projects the result; `rwEdgesOf` does the same for the hand-written rule.
Labels do not appear in `Edge`, so only the edge list needs the projection.
-}
module Tests.Core.Unit.Common.Rule (
  rwPair,
  edsOf,
  rwEdgesOf,
) where

import qualified Clash.Prelude as C
import qualified Core.Concrete.ReduceRulesLambda as RW
import Core.Node
import qualified INet.Define as Def
import INet.Net (INet (makeReduction), ReduceRuleResult)
import Tests.Core.Unit.Common.Normalize (normEdges, normNodesDef, toDef)
import Prelude

-- | Active pair extracted from a fixture, relabeled to the EDSL label set.
rwPair ::
  (LoadedNode 2 RW.AgentSimpleLambda, LoadedNode 2 RW.AgentSimpleLambda) ->
  (LoadedNode 2 Def.AgentSimpleLambda, LoadedNode 2 Def.AgentSimpleLambda)
rwPair (LoadedNode (Node pr secs lbl) a, LoadedNode (Node pr' secs' lbl') a') =
  ( LoadedNode (Node pr secs (toDef lbl)) a
  , LoadedNode (Node pr' secs' (toDef lbl')) a'
  )

-- | EDSL rule result on a fixture pair with the given pool.
edsOf ::
  C.Vec 4 (Maybe AddressNumber) ->
  (LoadedNode 2 RW.AgentSimpleLambda, LoadedNode 2 RW.AgentSimpleLambda) ->
  ([Edge 2], [LoadedNode 2 Def.AgentSimpleLambda])
edsOf freePool (lN, rN) = (edges, nodes)
 where
  (lN', rN') = rwPair (lN, rN)
  res :: ReduceRuleResult 4 2 2 Def.AgentSimpleLambda
  res = makeReduction freePool lN' rN'
  edges = normEdges res
  nodes = normNodesDef res

-- | Hand-written rule result on the same raw pair, edges only.
rwEdgesOf ::
  C.Vec 2 (Maybe AddressNumber) ->
  LoadedNode 2 RW.AgentSimpleLambda ->
  LoadedNode 2 RW.AgentSimpleLambda ->
  [Edge 2]
rwEdgesOf free lN rN = normEdges res
 where
  res :: ReduceRuleResult 2 2 2 RW.AgentSimpleLambda
  res = makeReduction free lN rN
