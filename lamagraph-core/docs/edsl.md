# EDSL for Interaction Net Rules — External API Reference

A domain-specific language in Haskell for writing interaction-net reduction
rules in `lamagraph-core`. A rule is a statement block over `fresh`/`wire`
primitives that describes how an active pair is rewritten: which fresh nodes
are created and which ports they are wired to.

This document is a **user-facing reference**: the public types, the
primitives, the `INet` instance contract, example bodies, and the conventions
a rule author must respect. Design rationale lives in
[`decisions.md`](decisions.md) and the requirements docs
([attempt 2](edsl-requirements-2.md), [attempt 3](edsl-requirements-3.md)).

## 1. Modules and layering

| Module          | Role                                                          |
|-----------------|---------------------------------------------------------------|
| `Core.Node`     | Foundation types: `Node`, `LoadedNode`, `Connection`, `Port`, `Edge`, address space |
| `INet.Net`      | `INet` class and `ReduceRuleResult` — the contract a rule set implements |
| `INet.Edsl`     | Rule-body machinery: builder state, `fresh`/`wire`, materialization; plus the agent-kind declaration skeleton |
| `INet.Define`   | Concrete usage: `{Apply, Abstract, Erase, Duplicate}` runtime labels, rule bodies, `INet` instance |

The **machinery** in `INet.Edsl` is agent-set agnostic: `RuleBuilder`,
`fresh`, `wire`, `nodeSecondaries`, and `runReduce` are parameterized over an
agent kind (`k`) and label type (`l`) and work for any agent set without
change (decision **D-Modules**). The **declaration skeleton** — `AgentKind`,
`AgentArity`, and the `Agent` singleton — is *currently* defined in `INet.Edsl`
but carries the lambda-calculus set (`Apply`, `Abstract`, `Erase`,
`Duplicate`); a different agent set replaces these declarations (or the
skeleton is parameterized) while the machinery stays untouched.
`INet.Define` is the lambda-calculus-specific layer that declares runtime
labels and writes the rules.

## 2. Core types

### 2.1 Port and connection

```haskell
type Connection (portsNumber :: Nat) = Maybe (Port portsNumber)

pattern Connected :: Port portsNumber -> Connection portsNumber   -- ^ = Just
pattern NotConnected :: Connection portsNumber                   -- ^ = Nothing

data IdOfPort (portsNumber :: Nat) = Id (Index portsNumber) | Primary

data Port (portsNumber :: Nat) = Port
  { _nodeAddress        :: AddressNumber
  , _portConnectedToId  :: IdOfPort portsNumber
  }
```

A `Connection` is `Maybe (Port)` with pattern synonyms. Combined with the
*outer* `Maybe` of a secondary slot, a secondary port has three states:

| Representation                       | Meaning                          |
|--------------------------------------|----------------------------------|
| `Nothing` (outer)                    | **void** — slot does not exist (node arity < max arity) |
| `Just NotConnected` (= `Just Nothing`) | **free** — real slot, uncoupled |
| `Just (Connected (Port a pid))`        | **wired** to `Port a pid`        |

### 2.2 Address space

```haskell
type AddressBitSize = 4
type AddressNumber  = Unsigned AddressBitSize      -- 0..15
type CellsNumber    = 2 ^ AddressBitSize           -- 16
```

The node RAM has `CellsNumber` cells; free addresses are handed to the rule as
a `Vec` and must never collide with addresses the surrounding net references.

### 2.3 Node and edge

```haskell
data Node (portsNumber :: Nat) agentType = Node
  { _primaryPort     :: Connection portsNumber
  , _secondaryPorts  :: Vec portsNumber (Maybe (Connection portsNumber))
  , _nodeType        :: agentType
  }

data LoadedNode (portsNumber :: Nat) agentType = LoadedNode
  { _containedNode   :: Node portsNumber agentType
  , _originalAddress :: AddressNumber
  }

data Edge (portsNumber :: Nat) = Edge
  { _leftEnd   :: Connection portsNumber
  , _rightEnd  :: Connection portsNumber
  }
```

`portsNumber` is the **maximum** secondary arity in the agent set; lower-arity
agents pad their slots with `Nothing` (decision **D-AgentArity**).

## 3. The `INet` class contract

```haskell
class INet agentType nodesNumber edgesNumber portsNumber where
  getAddressesAllocationCount :: agentType -> agentType -> Index CellsNumber
  makeReduction ::
    Vec nodesNumber (Maybe AddressNumber) ->        -- free-address pool
    LoadedNode portsNumber agentType ->             -- left  agent of the pair
    LoadedNode portsNumber agentType ->             -- right agent of the pair
    ReduceRuleResult nodesNumber edgesNumber portsNumber agentType

data ReduceRuleResult (nodesNumber :: Nat) (edgesNumber :: Nat) (portsNumber :: Nat) agentType
  = ReduceRuleResult
  { _edges :: Vec edgesNumber (Maybe (Edge portsNumber))
  , _nodes :: Vec nodesNumber (Maybe (LoadedNode portsNumber agentType))
  }
```

Contract:

- `makeReduction` is the **only** entry point a user (or the scheduler) calls.
- Counts `nodesNumber`/`edgesNumber` are the instance-level **ceiling** over
  all rules of the agent set; every result is padded with `Nothing` (decision
  **D-Vec**). A body that overflows its vector raises `errorX` — explicit
  error, never silent truncation.
- The pool `Vec nodesNumber (Maybe AddressNumber)` may be partially filled
  (`Nothing` = unused budget); `fresh` consumes entries left to right.
- `getAddressesAllocationCount` reports how many pool addresses a given pair
  will consume, so the memory manager can reserve them before reduction.
- Dispatch is **symmetric**: rule bodies are written once; both orientations
  reuse them (decision **D-Symmetry**).

## 4. Agent declaration skeleton

The declaration *pattern* is generic — a type-level kind tag, a type family
for the arity, a GADT singleton, and a label-mapping class. The concrete kinds
below are the **lambda-calculus set**, currently declared in `INet.Edsl`
(see §1 for the layering caveat).

```haskell
data AgentKind = ApplyKind | AbstractKind | EraseKind | DuplicateKind

type family AgentArity (k :: AgentKind) :: Nat where
  AgentArity 'ApplyKind    = 2
  AgentArity 'AbstractKind = 2
  AgentArity 'EraseKind    = 0
  AgentArity 'DuplicateKind = 2

data Agent (k :: AgentKind) where
  Apply    :: Agent 'ApplyKind
  Abstract :: Agent 'AbstractKind
  Erase    :: Agent 'EraseKind
  Duplicate :: Agent 'DuplicateKind

class (KnownNat (AgentArity k)) => AgentLabel k l | k -> l where
  agentLabel :: Agent k -> l
```

The arity is a **type-level** number (`AgentArity`), so `fresh` returns a
secondary-port vector whose length GHC derives (decision **D-Agent**,
**D-AgentArity**). `AgentLabel` maps a singleton tag to the flat runtime label
stored in `Node._nodeType`.

## 5. Rule-body primitives

A rule body is a `State` action over the builder state:

```haskell
type RuleBuilder edgesNumber nodesNumber portsNumber l =
  State (RuleState edgesNumber nodesNumber portsNumber l)

data PortPointer = Free | Main AddressNumber | Pointer AddressNumber Int
```

`PortPointer` names a wire end inside the rule: a head-binder port of the
active pair, or a port of a fresh node.

### 5.1 Reading the pair's secondary ports

```haskell
nodeSecondaries :: (KnownNat portsNumber) => LoadedNode portsNumber l -> Vec portsNumber PortPointer
```

Converts a pair node's secondary slots into port pointers:

| Slot content                      | Pointer            |
|-----------------------------------|--------------------|
| `Nothing` (void)                  | `Free`             |
| `Just NotConnected`               | `Free`             |
| `Just (Connected (Port a Primary))` | `Main a`         |
| `Just (Connected (Port a (Id x)))`  | `Pointer a x`     |

### 5.2 Creating fresh nodes

```haskell
fresh :: (AgentLabel k l, KnownNat (AgentArity k), KnownNat p, KnownNat n) =>
  Agent k -> RuleBuilder e n p l (AddressNumber, Vec (AgentArity k) PortPointer)
```

Allocates the next address from the free pool, creates the node accumulator
with that label, and returns the node's **address** (for wiring its principal
port, `Main address`) plus its bindable secondary ports (for wiring `Pointer
address i`). Pool exhaustion is `errorX`.

### 5.3 Wiring

```haskell
wire :: PortPointer -> PortPointer -> RuleBuilder e n p l ()
```

Connects two wire ends. If either end names a **fresh** node's port, the
connection is accumulated into that node directly (`writeAcc`); if both ends
are external, an `Edge` is appended to the edge vector (`addWire`). Wires into
the free pool or off a void slot are no-ops.

### 5.4 Materialization

```haskell
runReduce :: (KnownNat e, KnownNat n, KnownNat p) =>
  Vec n (Maybe AddressNumber) -> RuleBuilder e n p l () -> ReduceRuleResult n e p l
```

Runs a body over a free-address pool and materializes every accumulator into
an `LoadedNode` in the result.

## 6. Example rules (`INet.Define`)

Lambda-calculus labels:

```haskell
data AgentSimpleLambda = ApplyLabel | AbstractLabel | EraseLabel | DuplicateLabel
```

### 6.1 Beta: `Apply >< Abstract`

```haskell
betaBody :: Vec 2 PortPointer -> Vec 2 PortPointer -> RuleBuilder 2 4 2 AgentSimpleLambda ()
betaBody leftPorts rightPorts = do
  wire (leftPorts !! sec2Port0) (rightPorts !! sec2Port0)
  wire (leftPorts !! sec2Port1) (rightPorts !! sec2Port1)
```

Creates nothing; crosses the two secondary pairs. `MakeReduction` for
`(ApplyLabel, AbstractLabel)` runs it as `betaBody lp rp` and for the mirror
`(AbstractLabel, ApplyLabel)` as `betaBody rp lp`.

### 6.2 Erase: `Erase >< X`

```haskell
eraseAnyBody :: Vec 2 PortPointer -> RuleBuilder 2 4 2 AgentSimpleLambda ()
eraseAnyBody ports = do
  (primA, _) <- fresh Erase
  (primB, _) <- fresh Erase
  wire (ports !! sec2Port0) (Main primA)
  wire (ports !! sec2Port1) (Main primB)
```

Each secondary port of the non-erasure agent becomes an `Erase` at a fresh
address. `Erase >< Erase` is `eraseEraseBody = pure ()` — nothing arises.

### 6.3 Duplicate: `Duplicate >< X`

```haskell
dupAnyBody ::
  (AgentArity a ~ 2, AgentLabel a AgentSimpleLambda) =>
  Agent a -> Vec 2 PortPointer -> Vec 2 PortPointer ->
  RuleBuilder 2 4 2 AgentSimpleLambda ()
dupAnyBody copyAgent branchPorts auxPorts = do
  paPorts <- fresh copyAgent
  pbPorts <- fresh copyAgent
  pkPorts <- fresh Duplicate
  plPorts <- fresh Duplicate
  let (primA, copyA) = paPorts
      (primB, copyB) = pbPorts
      (primK, fanK)  = pkPorts
      (primL, fanL)  = plPorts
  wire (branchPorts !! sec2Port0) (Main primA)
  wire (branchPorts !! sec2Port1) (Main primB)
  wire (auxPorts    !! sec2Port0) (Main primK)
  wire (auxPorts    !! sec2Port1) (Main primL)
  wire (fanK !! sec2Port0) (copyA !! sec2Port0)
  wire (fanK !! sec2Port1) (copyB !! sec2Port0)
  wire (fanL !! sec2Port0) (copyA !! sec2Port1)
  wire (fanL !! sec2Port1) (copyB !! sec2Port1)
```

Two copies of `X` (`pa`, `pb`), two fan `Duplicate`s (`pk`, `pl`) that
cross-connect each copied secondary pair. `copyAgent` is `Apply` or
`Abstract`, whichever the pair matched.

### 6.4 `INet AgentSimpleLambda 4 2 2` instance

```haskell
instance INet AgentSimpleLambda 4 2 2 where
  getAddressesAllocationCount left right = case (left, right) of
    (ApplyLabel, AbstractLabel) -> 0
    (AbstractLabel, ApplyLabel) -> 0
    (EraseLabel, EraseLabel)    -> 0
    (EraseLabel, _)             -> 2
    (_, EraseLabel)             -> 2
    (DuplicateLabel, DuplicateLabel) -> errorX "No rule for this pair"
    (DuplicateLabel, _)         -> 4
    (_, DuplicateLabel)         -> 4
    _ -> errorX "No rule for this pair"
```

`Duplicates >< Duplicate` has no rule and is rejected at dispatch, before any
allocations.

## 7. Worked example — one reduction trace

Fixture: active pair `Apply @5, Erase @1`, the Apply's slots wired to
`@4.Id1` and `@4.Id0` (context node `@4`), free pool `{0, 7}`.

```haskell
lN = LoadedNode (Node (Connected (Port 1 Primary)) [@4.Id1, @4.Id0] ApplyLabel) 5
rN = LoadedNode (Node (Connected (Port 5 Primary)) [void, void]      EraseLabel) 1
freeAddrs = [Just 0, Just 7, Nothing, Nothing]

>> makeReduction freeAddrs lN rN
-- Dispatch (EraseLabel, _) -> eraseAnyBody rp
-- rp = secondaries of the NON-erasure node (Apply) = [Pointer 4 1, Pointer 4 0]
--  fresh Erase -> address 0   (primA)
--  fresh Erase -> address 7   (primB)
--  wire (Pointer 4 1) (Main 0)   -> node @0 primary = @4.Id1
--  wire (Pointer 4 0) (Main 7)   -> node @7 primary = @4.Id0

ReduceRuleResult { _edges = [Nothing, Nothing]
                 , _nodes = [ Just (LoadedNode (Node (Connected (@4.Id1)) [void, void] EraseLabel) 0)
                            , Just (LoadedNode (Node (Connected (@4.Id0)) [void, void] EraseLabel) 7)
                            , Nothing, Nothing ] }
```

The erase-to-apply reduction consumes **two** free addresses and rewrites the
Apply's secondary wires into the two fresh Erase nodes — matching the
hand-written `eraseToAbstractOrApplyRule` output node-for-node (this is what
`Tests.Core.Unit.EdslRules` checks by property).

## 8. Conventions and gotchas

- **Erase has arity 0.** Its two runtime slots are outer-layer `Nothing`
  (void), whether as an actor or a fresh node. `fresh Erase` returns an empty
  port vector; the body must not wire its non-existent ports.
- **Not-connected ≠ void.** A real slot that is uncoupled is
  `Just NotConnected`; a slot beyond the agent's arity is `Nothing`. EDSL
  `initSecAcc arity` sets real slots `Just NotConnected` and padding `Nothing`.
- **Wires have two ends.** A pair slot `@L.Id j` wired to `@ctx.p` implies the
  mirror `Just (Connected (Port L (Id j)))` on `@ctx.p`. Rules only see the
  pair, but generated test nets must keep this symmetry.
- **No refs into the pair.** A pair member's primary is busy (it is the pair
  wire) and its secondaries *are* the interface — referencing them double-wires
  a port. Valid targets are open ports of out-of-pair nodes.
- **Free-pool hygiene.** Free addresses must be disjoint from every address the
  surrounding net references, and used at most once per result.
- **`[AI-WIP]` markers.** Rule-body comments in `INet.Define.hs` currently carry
  interim `[AI-WIP]` markers from development commits; they are editorial and
  may be stripped in a cleanup commit.

## 9. Reading order

1. [`edsl-requirements-2.md`](edsl-requirements-2.md) — problem statement and
   why the triple-representation prototype failed.
2. [`edsl-requirements-3.md`](edsl-requirements-3.md) — surface requirements
   for the imperative rule-body style.
3. [`decisions.md`](decisions.md) — the D-* decisions this API implements.
4. This document — what the API looks like from the outside.