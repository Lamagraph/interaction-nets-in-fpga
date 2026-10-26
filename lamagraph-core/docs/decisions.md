# EDSL Design Decisions with Motivation

Design decisions for the interaction-net rule EDSL in `lamagraph-core`.
Each decision records: what, why, and how it relates to the requirement docs
(`edsl-requirements-2.md`, `edsl-requirements-3.md`).
Decisions superseded by later discussion are marked and linked to their successors.

---

## D-Agent. Agent as type tag, not node constructor

**Decision.** An agent kind is a singleton **type tag** (`data Agent a where Dup :: Agent 'DupTag`),
not a constructor that directly carries a vector of secondary ports.

**Motivation.** In the first prototype (`INet.Define.hs`) agents were triple-represented
(plain enum `AgentSimpleLambda` + GADT `AgentWithArity n` + existential wrapper), with
three hand-written conversions (`agentsToAgent`, `agentToAgents`, `getSecondaries`) that must
stay in sync (problem P1 of requirement docs). Lifting the agent to a type tag makes the arity
a **type-level** property (`AgentArity :: AgentTag -> Nat`) computed by GHC, not a field.
This is decision D1 (type-level as much as possible) of `edsl-requirements-2.md` §4.
A separate materialization step later converts the tag into the flat runtime label stored in
`Node._nodeType` (R1 of `edsl-requirements-2.md` §5).

---

## D-Fresh. Creation primitive returns address + secondary ports

**Decision.** `fresh :: AgentLabel s l => s -> RuleBuilder (AddressNumber, Vec (AgentArityOf s) PortPointer)`
(where `s` is the agent-set singleton, e.g. `Agent 'ApplyKind`)
returning the new node's address and its bindable secondary ports.

**Motivation.** `fresh Dup` where `Dup` is a plain constructor of port vectors is malformed —
you cannot "build" a constructor. With the type-tag model (D-Agent), `fresh` is well-defined:
allocate an address, create the node slot with the tag, return the address plus the arity-typed
port vector. Returning `PortPointer`s (rather than a pre-built `Node`) lets the caller refer to
the new node's ports for wiring before the node is materialized (SF8, SF10 of
`edsl-requirements-3.md` §3).

---

## D-Wire. Separate construction and wiring statements

**Decision.** Creation and principal-port connection are **separate** statements, not fused
into one `setPrim (freshAgent X) p` call.

**Motivation.** The fused form `setPrim (freshAgent Dup) d1` is verbose and couples two
concerns (allocate + connect) in one expression, and breaks down when the same node must be
connected at multiple points. Splitting into `fresh` (allocate, D-Fresh) + `wire p q`
(connect) gives a uniform, composable vocabulary: a node is created once, then any of its ports
(primary via its returned address, secondary via the returned `PortPointer`s) yield to `wire`.
This satisfies SF6/SF7 of `edsl-requirements-3.md` §2 — every wiring combination expressible,
most common shape (create + attach principal) still a short statement pair.

---

## D-Builder. `RuleBuilder` accumulates wires and node connections in a State monad

**Decision.** Rules run in a `State` monad, `RuleBuilder`, whose state accumulates a vector of
wires (`Vec edgesNumber (Maybe (Edge portsNumber))`) and a vector of node accumulators
(`Vec nodesNumber (Maybe (agentType, NodeAccumulator portsNumber))`), plus address bookkeeping.
At the end, the state is **materialized** into `ReduceRuleResult` by turning each node
accumulator into a `Node`.

**Motivation.** Mirrors the existing `Changes` / `ChangesAccumulator` mechanism in
`Core.MemoryManager.NodeChanges` / `ChangesAccumulator` (connection accumulation → whole-node
construction), reused for *construction* instead of *update*. Accumulating connections first
lets wires to secondary ports and the primary port land in one place per node, then a single
materialization pass builds the `Node` records (SF14 of `edsl-requirements-3.md` §5). The
`do`-notation (SF5, `edsl-requirements-3.md` §2) is the surface; `State` is the implementation.

`NodeAccumulator` is:

```haskell
data NodeAccumulator (portsNumber :: Nat) = NodeAccumulator
  { _primeAcc :: Connection portsNumber
  , _secAcc   :: Vec portsNumber (Maybe (Connection portsNumber))
  }
```

---

## D-Vec. Fixed-size `Vec`, counts bound at the instance level

**Decision.** The builder accumulates into **fixed-size `Vec`s** whose sizes
(`edgesNumber`, `nodesNumber`) are the instance-level maximum over all rules; individual rules
pad with `def`. The rule body does not restructure its own counts into the type.

**Motivation.** Direct do-notation mutation conflicts with deriving per-rule counts into the
type (incrementing type-level Nats across `bind` needs an indexed monad — complex, and reverses
D1's "no manual bookkeeping" by forcing the *body* to state counts). Direction D of the
discussion is the easiest to understand and work with: the developer fixes one maximum per
agent set at the `INet` instance boundary (D2, `edsl-requirements-2.md` §4), and each rule body
merely fills up to the vector, padding with `def` (the existing padding convention from current
code). The counts checked against the body at materialization; a rule that overflows its
vector is a compile-time error, never silent truncation (SF15, `edsl-requirements-3.md` §5).
This consciously trades R3/SF14 "derive counts from body" for simplicity — counts are stated
once at the instance ceiling. *(TODO: revisit if automatic derivation becomes necessary.)*

---

## D-AgentArity. Secondary arity is a type-level map; `NodeAccumulator` sized by max arity

**Decision.** Agent arity (`AgentArity :: AgentTag -> Nat`) is type-level; the builder's
`NodeAccumulator` and the runtime `Node._secondaryPorts` are sized by the **maximum** arity
across the agent set (padding lower-arity agents).

**Motivation.** Type-level arity gives GHC the fan-out length for traversals (SF12/SF13 of
`edsl-requirements-3.md` §4) and keeps the secondary-port vector length derived, not written.
Sizing the runtime `Node` by the max arity matches the existing `portsNumber` parameter in
`Core.Node` and the `INet` class (agents must share one fixed `portsNumber`). Lower-arity
agents leave unused secondary slots as `def`.

---

## D-Dup. Duplicate is an ordinary arity-2 agent

**Decision.** `Duplicate` is a declared agent with arity 2, not built-in machinery; its rules
are written like any other (D6 of `edsl-requirements-2.md` §4, SF carry-over §6 of
`edsl-requirements-3.md`).

**Motivation.** No special-case duplicator logic: a `Duplicate`/`Agent` active pair is one
ordinary rule that fans out over the paired agent's secondary ports (SF12). Keeps the EDSL
surface minimal and uniform; duplication rules derive from the same primitives as everything
else.

---

## D-Symmetry. One rule body, two dispatch entries

**Decision.** A rule is written once; the materialized `makeReduction` dispatches on the
(unordered) active pair in both orientations, reusing the same body (R5/D5 of
`edsl-requirements-2.md` §4-5).

**Motivation.** Mirrored cases (`Apply,Abstract` / `Abstract,Apply`) are the same reduction;
writing the body once avoids the copy-paste duplication flagged as P6. The dispatch entry is
the only per-orientation artifact.

---

## D-ForAncestry. Principal ports never bound in the rule head

**Decision.** Rule-head patterns bind only **secondary** ports; principal ports of the two
active-pair agents are consumed by the pair itself (SF2 of `edsl-requirements-3.md` §1).

**Motivation.** The active-pair principal ports are already wired to each other (that is what
makes the pair active); re-binding them is meaningless. Binding only secondaries matches the
existing `applyToLambdaRule` / `eraseToAbstractOrApplyRule` shapes and Inpla
`Dup(p1,p2) >< SomeAgent(sp)` notation.

---

## D-Modules. EDSL machinery in `INet.Edsl`, agent-set usage in `INet.Define`

**Decision.** The generic EDSL machinery (builder state, `fresh`/`wire` primitives,
materialization, the generic `AgentLabel` class) lives in `INet.Edsl`. The agent-set
declaration skeleton is generated by `INet.Edsl.Agents.declareAgents`, and the
agent-set-specific code (the splice call, rule bodies, `INet` instance dispatch) lives
in `INet.Define`.

**Motivation.** Separating the generic mechanism from the concrete usage keeps each
module focused: `INet.Edsl` can be reused for different agent sets without change;
`INet.Define` contains only the lambda-calculus-specific glue. To keep `INet.Edsl`
free of any concrete set, `AgentLabel` is indexed by the agent *singleton type* and
exposes its arity through the associated family `AgentArityOf`; generated instances
bridge that family to the set's kind-indexed `AgentArity`. The class keeps the
`fresh` primitive monomorphic in its call-site shape (one argument) while allowing
the label type to vary per agent set. This follows D-Agent's "materialize label
later" philosophy: the builder accumulates labels generically; the usage module
decides what the label type is.

---

## D-TH. One declaration list generates the agent-set skeleton

**Decision.** `INet.Edsl.Agents.declareAgents :: String -> [AgentDecl] -> Q [Dec]`
(with `AgentDecl = (String, Int)`) emits the whole skeleton of an agent set from
`(constructor name, secondary arity)` pairs: the flat runtime label enum, the
promoted-tag kind enum, the kind-indexed arity type family, the singleton GADT,
and one `AgentLabel` instance per agent. Per-agent names are derived by suffix:
`(Apply, 2)` yields the singleton `Apply`, the tag `ApplyKind`, and the runtime
label `ApplyLabel`. The type names `AgentKind`, `AgentArity`, and `Agent` are
fixed; only the runtime label type name is a splice argument.

**Motivation.** This is requirement F10/R1 of `edsl-requirements-2.md` and D4's "TH for
mechanical, derivable artifacts". The five declarations are mechanically determined by
the same per-agent tuple (name, arity), so keeping them hand-written (or asking the
caller to repeat tag/singleton/label names) invites drift. Generating them from one list
gives a single source of truth while the core layer stays ordinary typed Haskell: rule
bodies and the `INet` instance are not generated (Q1 of `edsl-requirements-3.md` stays
resolved as "helpers, not whole instances"). The arity family keeps its kind-indexed
shape (`AgentArity :: AgentKind -> Nat`) so documented signatures are unchanged; the
bridge to the singleton-indexed class is one generated equation per agent.

**Alternatives considered.** The first version took an `AgentSet` record with four
type-name strings and an `AgentDecl` record with four fields per agent; rejected as
redundant because the extra names follow mechanically from the constructor name.
Deriving every type name from a set prefix (`LambdaKind`, `LambdaArity`, `LambdaAgent`,
`LambdaLabel`) was also rejected for now: it churns all documented signatures for no
gain. Merging the runtime label enum into `AgentKind` would remove the remaining
label-type argument, but changes `Node._nodeType` and the test normalizer; left open.
