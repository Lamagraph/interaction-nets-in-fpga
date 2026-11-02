{- | Shared projections bridging the two rule sets.

"Core.Concrete.ReduceRulesLambda" (hand-written, arity-2 result) and
"INet.Define" (EDSL, arity-4 result) are compared through the list projections
`normEdges` \/ `normNodesDef` \/ `normNodesRW`, which drop the @Nothing@ padding
of the fixed-size result vectors; `toDef` maps a hand-written label onto the
EDSL label set. The small constructors at the bottom (`freeVec2`, `freeVec4`,
`freeVec4Full`, `mkNode`) are the pool\/node shapes every rule-level test needs.
-}
module Tests.Core.Unit.Common.Normalize (
  normEdges,
  normNodesDef,
  normNodesRW,
  toDef,
  freeVec2,
  freeVec4,
  freeVec4Full,
  mkNode,
) where

import Clash.Prelude (Vec)
import qualified Clash.Prelude as C
import qualified Core.Concrete.ReduceRulesLambda as RW
import Core.Node
import Data.Maybe (catMaybes, mapMaybe)
import qualified INet.Define as Def
import INet.Net (ReduceRuleResult (..))
import Prelude

-- | Project a `ReduceRuleResult` onto the list of its real edges.
normEdges :: ReduceRuleResult n e p l -> [Edge p]
normEdges = catMaybes . C.toList . _edges

-- | Project a `ReduceRuleResult` onto the list of its real nodes (EDSL labels).
normNodesDef :: ReduceRuleResult n e p Def.AgentSimpleLambda -> [LoadedNode p Def.AgentSimpleLambda]
normNodesDef = catMaybes . C.toList . _nodes

-- | Project a `ReduceRuleResult` onto its real nodes, relabeled to EDSL labels.
normNodesRW :: ReduceRuleResult n e p RW.AgentSimpleLambda -> [LoadedNode p Def.AgentSimpleLambda]
normNodesRW = mapMaybe (fmap conv) . C.toList . _nodes
 where
  conv (LoadedNode (Node prim secs lbl) addr) = LoadedNode (Node prim secs (toDef lbl)) addr

-- | Map a hand-written label onto the EDSL label.
toDef :: RW.AgentSimpleLambda -> Def.AgentSimpleLambda
toDef = \case
  RW.Apply -> Def.ApplyLabel
  RW.Abstract -> Def.AbstractLabel
  RW.Erase -> Def.EraseLabel

-- | Hand-written rules require a fully-populated free-address pool.
freeVec2 :: AddressNumber -> AddressNumber -> Vec 2 (Maybe AddressNumber)
freeVec2 a0 a1 = Just a0 C.:> Just a1 C.:> C.Nil

-- | EDSL pool: two free addresses, remaining budget unused.
freeVec4 :: AddressNumber -> AddressNumber -> Vec 4 (Maybe AddressNumber)
freeVec4 a0 a1 = Just a0 C.:> Just a1 C.:> Nothing C.:> Nothing C.:> C.Nil

-- | EDSL pool with all four addresses present.
freeVec4Full :: AddressNumber -> AddressNumber -> AddressNumber -> AddressNumber -> Vec 4 (Maybe AddressNumber)
freeVec4Full a0 a1 a2 a3 = Just a0 C.:> Just a1 C.:> Just a2 C.:> Just a3 C.:> C.Nil

{- | A `LoadedNode` whose primary port is wired to @partner@'s primary port,
with the given secondary connections and address.
-}
mkNode ::
  agentType ->
  Vec 2 (Maybe (Connection 2)) ->
  AddressNumber ->
  AddressNumber ->
  LoadedNode 2 agentType
mkNode lbl secs addr partner =
  LoadedNode (Node (Connected (Port partner Primary)) secs lbl) addr
