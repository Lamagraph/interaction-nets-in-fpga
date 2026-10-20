{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE StarIsType #-}
{-# LANGUAGE TypeFamilies #-}

{- |
Module      : INet.Net
Description : Agent-set interface: reduction-rule result and the `INet` class

Defines what a concrete agent set must provide to drive the generic reducer
in "Core.Reducer": how many free cell addresses a rule pair will consume and
how to build the reduction result for an active pair. Two rule sets exist:
the hand-written instance @INet AgentSimpleLambda 2 2 2@ in
"Core.Concrete.ReduceRulesLambda" (used by @topEntity@ and most tests) and
the EDSL-generated instance @INet AgentSimpleLambda 4 2 2@ in "INet.Define".
-}
module INet.Net where

import Clash.Prelude
import Control.Lens (makeLenses)
import Core.Node

{- | Result of an abstract reduction rule, in fixed-size slots.

Both vectors are indexed slot-wise and carry `Nothing` in unused slots:
- `_edges`: the wires the rule has produced (including wires with a dangling
  @NotConnected@ end);
- `_nodes`: the fresh nodes with the cell addresses that were handed to the
  rule (an entry may reuse the address of a node of the reduced pair).

"Core.Reducer.toDelta" post-processes both vectors to redirect connections
that still point into the reduced active pair.
-}
data ReduceRuleResult (nodesNumber :: Nat) (edgesNumber :: Nat) (portsNumber :: Nat) agentType = ReduceRuleResult
  { _edges :: Vec edgesNumber (Maybe (Edge portsNumber))
  , _nodes :: Vec nodesNumber (Maybe (LoadedNode portsNumber agentType))
  }
  deriving (Generic, NFDataX, Show)

$(makeLenses ''ReduceRuleResult)

{- | Agent-set interface of the generic reducer.

Laws expected of an instance (checked by the test suite, not enforced here):
- the count returned by `getAddressesAllocationCount` must match the number of
  addresses actually consumed from the free-address vector by `makeReduction`;
- rule pairs without a rule must yield `errorX`, never a silent default.
-}
class INet agentType nodesNumber edgesNumber portsNumber where
  -- | How many free cell addresses the rule for this agent pair consumes (as an `Index CellsNumber`).
  getAddressesAllocationCount :: agentType -> agentType -> Index CellsNumber

  {- | Build the reduction result for the active pair from the free addresses
  handed out according to `getAddressesAllocationCount`. Left argument is the
  loaded node on the left of the pair, right one — on the right; rules that
  are not symmetric must pattern-match on both.
  -}
  makeReduction ::
    Vec nodesNumber (Maybe AddressNumber) -> -- vec of free addresses
    LoadedNode portsNumber agentType ->
    LoadedNode portsNumber agentType ->
    ReduceRuleResult nodesNumber edgesNumber portsNumber agentType
