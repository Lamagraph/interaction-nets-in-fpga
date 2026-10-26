# EDSL for Interaction Net Rules — Requirements (attempt 2)

Design requirements for a Haskell-embedded DSL that defines interaction net
reduction rules, based on the [Inpla](https://github.com/inpla/inpla) language,
compiled down to the existing Clash hardware interface in this repository.

This document supersedes the approach tried in `src/INet/Define.hs`
(analysis in §2). Decisions confirmed with the project author are marked
**[DECIDED]**; unresolved points are collected in §8. See `decisions.md` for
the final decision record and `edsl-requirements-3.md` for the surface
refinements.

---

## 1. Purpose

Interaction net rules in `lamagraph-core` are currently written by hand as raw
manipulation of `LoadedNode` / `Edge` / `Vec` structures
(see `Core.Concrete.ReduceRulesLambda`). This is verbose, error-prone, and
duplicates information (e.g. node allocation count vs. actual created nodes).

Goal: an EDSL where a developer writes rules close to Inpla notation, and the
compiler (GHC + TH) derives everything the hardware interface needs:

* the reduction itself (edges and nodes produced by a rule),
* the static allocation count for the memory manager,
* the plumbing between the human-readable agent names and the flat
  `agentType` labels used inside `Node`.

The EDSL lives entirely in `lamagraph-core`; its output must be synthesizable
by Clash without changes to build flags (§6).

---

## 2. Prior attempt: what `INet.Define` got right and wrong

The file `src/INet/Define.hs` contains a partial prototype. Its features are
the baseline vocabulary for this design; its problems are the reason for a
second attempt.

### 2.1 Features to keep (extracted)

| # | Feature | Code |
|---|---------|------|
| F1 | Rule operator `\|><\|` mirroring Inpla's `LHS >< RHS =>` | `(\|><\|)` |
| F2 | Type-level arity: GADT `AgentWithArity n`, where `n` is the number of *secondary* ports (`Erase'` has 0); existential wrapper for heterogeneous use | `AgentWithArity`, `AgentSimpleLambda'` |
| F3 | Port variables as `PortPointer = Free \| Main addr \| Pointer addr i`, referencing ports of the two agents of the active pair | `PortPointer` |
| F4 | Builder monad (State) accumulating edges and nodes into fixed-size `Vec`s with type-level sizes | `ClashContext`, `addToEdges`, `addToNodes` |
| F5 | Wiring primitive between two port pointers | `makeEdge` |
| F6 | Node-construction primitive from an agent spec plus primary-port pointer, secondary ports padded to max arity | `makeNode` |
| F7 | Dispatch on the pair of agent constructors | body of `(\|><\|)` |
| F8 | Static per-pair allocation count consumed by the memory manager | `getAddressesAllocationCount` |
| F9 | Target class: vector of free addresses in, `ReduceRuleResult` (edges + loaded nodes) out | `INet.Net.INet` |
| F10 | TemplateHaskell generates the agent skeleton: label enum, kind tags, arity family, GADT constructors, `AgentLabel` instances (dispatch glue stays hand-written) | `INet.Edsl.Agents.declareAgents`, used in `INet.Define` |

### 2.2 Problems identified

P1. **Triple representation of agents.** The same agent exists three times:
    plain enum `AgentSimpleLambda` (what `Node` stores), GADT
    `AgentWithArity n` (rule level), existential wrapper (dispatch level).
    Three hand-written conversions (`agentsToAgent`, `agentToAgents`,
    `getSecondaries`) must be kept in sync.

P2. **Manual type-level bookkeeping.** `portsNumber`, `edgesNumber`,
    `nodesNumber` are free `KnownNat` parameters threaded through
    `ClashContext`. Nothing forces them to match what the rule body actually
    builds; mismatch surfaces as runtime failure or silent truncation.

P3. **Incomplete wiring.** `makeEdge` covers only `Main/Main` and
    `Free/Main`. The most common case — connecting two *secondary* ports of
    the two active-pair agents (`Pointer`/`Pointer`, e.g. argument-to-body
    wire in the β-rule) — falls into `undefined`. The hand-written
    `applyToLambdaRule` does this correctly by zipping secondary connections;
    the EDSL abstraction never did.

P4. **Dispatch totality.** Unknown constructor pairs hit runtime
    `undefined`; nothing checks coverage of the declared agent set.

P5. **Allocation-count desync.** `getAddressesAllocationCount` is written by
    hand next to rules that create nodes; the two can diverge. The mapping of
    freshly created nodes to allocated addresses was left as
    `"stupid. TODO: redo"` (`imap` over `Maybe AddressNumber`).

P6. **Symmetry handled by copy-paste**: mirrored cases
    (`Abstract', Apply'` / `Apply', Abstract'`) enumerated manually per rule.

---

## 3. Inpla reference: features in and out of scope

Inpla rule shape (from upstream samples):

```inpla
insert(ret, int x) >< [] => ret ~ [x];
gcd(ret) >< (int a, int b) | b==0 => ret ~ a | _ => gcd(ret) ~ (b, a%b);
r << Add(1,2);          -- abbreviation for Add(r,1) ~ 2
```

### 3.1 In scope

* Agents with one principal port and a fixed number of secondary ports,
  declared once and referenced by name.
* Active-pair rules: `LHS >< RHS => result`, matched on agent kinds only
  (ports carry no data).
* Rule result expressed purely as a set of wires (`~`) between:
  - ports of the two active-pair agents,
  - ports of newly created agents.
* One rule per (unordered) agent pair; interaction is local.

### 3.2 Out of scope (explicitly)

* Guards and multi-body rules (`| cond => ...`) — **[DECIDED]** not implemented.
* Built-in integer values `(int i)` and arithmetic/comparison in rules —
  follows from "no guards"; data-plane values stay outside rule structure.
* List/tuple syntactic sugar (`[]`, `x:xs`, `[a,b]`, `(a,b)`) — encodings
  belong to the compiler front-end (`lamagraph-compiler`), not the net EDSL.
* `<<` abbreviation — surface sugar, unnecessary for combinators.
* Reuse annotations, multi-threading concerns, I/O agents.

---

## 4. Design decisions

D1. **Type-level as much as possible** **[DECIDED]**.
    Arity of every agent, number of edges/nodes a rule creates, and the
    allocation count must be computed by GHC from the rule definition, not
    written by hand. Term-level values appear only inside `Node` payloads
    (addresses, labels) where the hardware interface demands them.

D2. **Compilation target fixed** **[DECIDED]**: the EDSL compiles to instances
    of the existing class `INet.Net.INet`
    (`getAddressesAllocationCount` + `makeReduction`). The class is not
    redesigned in this effort. Consequences:
    * all rules of one agent set share one instance head
      `INet agentType nodesNumber edgesNumber portsNumber`, therefore the
      result vectors are sized by the *maximum* over all rule pairs; smaller
      rules are padded (existing `def`-padding convention);
    * the EDSL may still choose richer internal types per rule and erase to
      the fixed-size vectors at the instance boundary.

D3. **No guards** **[DECIDED]** — see §3.2.

D4. **Surface syntax: maximum plain Haskell + TH** **[DECIDED]**.
    Combinator EDSL (no QuasiQuoter). TemplateHaskell is used for mechanical,
    derivable artifacts: the flat label enum stored in `Node`, the
    label↔arity mapping, conversion functions, dispatch tables, and the
    exhaustiveness check. Anything expressible with ordinary
    types/type-families/GADTs should not go through TH.

D5. **Symmetry** **[DECIDED]**: rules are conceptually symmetric (an active
    pair is unordered), and support for automatic mirrored dispatch must
    exist; writing both directions manually is acceptable for the first
    iteration, provided the EDSL gives one obvious place per direction and
    does not require duplicating the rule *body*, only the dispatch entry.

D6. **Duplication is an ordinary agent** **[DECIDED]**: `Duplicate` is not a
    built-in; users declare it with arity 2 (or more) and write its rules like
    any other agent. No special ESDL machinery.

---

## 5. Functional requirements

R1. **Single agent declaration.**
    An agent kind is declared exactly once and yields, from one definition:
    the flat runtime label (for `_nodeType`), and the type-level secondary
    arity. Eliminates P1: no parallel enum/GADT/existential trio maintained
    by hand. TH may generate the flat label type from a Haskell list of
    declarations (replacing/extending `Core.Concrete.TypesTH.makeAgentsType`).

R2. **Rule definition form.**
    A rule is written roughly as: pattern pair + list of wires +
    list of fresh agents, e.g. conceptually

    ```haskell
    apply >< abs => [ p1 ~= q1, p2 ~= q2 ]                    -- β-rule
    erase >< x   => fresh EraseBy (sec 0 x), fresh EraseBy (sec 1 x)
    ```

    Exact combinator names/shapes are a design freedom; required properties:
    * LHS port references are statically scoped variables bound by the
      patterns (replacing raw `Main`/`Pointer` addresses of the old
      `PortPointer`);
    * wiring any combination of LHS ports and fresh-agent ports is
      expressible — including secondary↔secondary across the two LHS agents
      (fixes P3);
    * leaving a port unwired must be an explicit, visible choice
      (`NotConnected` in output), never an accident.

R3. **Derived static counts.**
    From the rule definition alone, GHC derives: number of created nodes,
    number of created edges, hence the allocation count. The value passed to
    `getAddressesAllocationCount` and the number of nodes the rule body
    constructs must have one source of truth (fixes P5's desync half). The
    assignment of allocated addresses to created nodes is part of the
    generated `makeReduction` glue, total and order-defined (fixes the
    "stupid TODO").

R4. **Total dispatch.**
    For the declared agent set, pair coverage is checked at compile time:
    every unordered pair either has a rule or is explicitly declared to have
    none (`errorX "no rule"` at runtime is then a deliberate, documented
    choice, not an omission) — addresses P4. TH is the expected tool for the
    closed-world check.

R5. **Symmetry support.**
    Declaring a rule yields dispatch entries for both orientations of the
    active pair. Manual mode (write orientation swap yourself) permitted
    initially (D5), but the rule body is written once.

R6. **Instance generation boundary.**
    The final artifact per agent set is an `INet` instance (or a value bundle
    from which the user writes the trivial instance): `makeReduction`
    dispatches on the two labels, extracts secondary ports, runs the rule's
    derived code, pads to the instance's `nodesNumber`/`edgesNumber`, and
    distributes free addresses (D2). Whether TH emits the whole instance or
    helper functions is open (§8).

R7. **Clash-clean output.**
    Everything the EDSL produces must live under `Clash.Prelude`
    conventions: `Vec`, `Index`, `Unsigned`, `NFDataX`/`ShowX` derivability,
    no bare `Prelude`, no added strictness (see AGENTS.md §7.2). The EDSL's
    *definition-time* layer (combinators, type families) may use whatever
    Haskell it needs, as long as it does not leak into synthesized terms.

---

## 6. Constraints (non-functional)

C1. Target package: `lamagraph-core`; new modules under `INet/`
    (suggested: `INet.Rule.*`); no changes to `.cabal` flags, GHC options, or
    the fragile Clash plugin flags (AGENTS.md §7.2).

C2. Formatting/lint gates apply: Fourmolu 0.18 (column 120, unicode never),
    HLint clean.

C3. Existing hand-written rules (`Core.Concrete.ReduceRulesLambda`) serve as
    oracle: EDSL-generated reductions must agree with them for the
    λ-calculus agent set (`Apply`, `Abstract`, `Erase`).

C4. Tests: property tests comparing generated rules against the hand-written
    ones; golden-style snapshot of generated TH output is acceptable.

---

## 7. Success criteria

S1. λ-calculus net set (`Apply`/`Abstract`/`Erase`) fully expressible in the
    EDSL; hand-written rules deleted or reduced to thin wrappers.
S2. Adding a new agent + rule requires exactly: one declaration, one rule
    body, nothing else (no conversions, no counts, no dispatch edits).
S3. Deliberately omitting a pair rule produces a compile-time diagnostic, not
    a runtime `undefined`.
S4. Simulation of a small program through `Core.Reducer` behaves identically
    before/after migration.

---

## 8. Open questions

Q1. Does TH emit the complete `INet` instance (R6) or only helpers, leaving a
    short hand-written instance? Leaning: full instance, one splice per agent
    set. *(Still open.)*

Q2. Internal rule representation: deeply-embedded AST refined by type
    families into counts (safer, enables R4 checks), versus direct
    monadic/consumer construction with phantom-typed counts accumulated in
    the type (closer to the old `ClashContext`, less re-checkable).
    **[RESOLVED]** — direction D: shallow `State` monad over fixed-size
    `Vec`s, counts bound at the instance ceiling (see `decisions.md`
    D-Builder/D-Vec and `edsl-requirements-3.md` §8).

Q3. Address distribution policy inside generated `makeReduction`: order of
    created nodes ↔ order of free-address vector — currently implicit in the
    old code; must be pinned down and documented (affects memory-manager
    tests). **[RESOLVED]** — creation order (SF16 in
    `edsl-requirements-3.md`).

Q4. Should the EDSL also cover initial-net construction (the job of
    `Core.Concrete.Initial` / loader), or strictly rules? Current scope:
    rules only. *(Still open.)*

Q5. Naming: `(|><|)` kept for familiarity with the previous attempt, or
    plainer `rule` combinator? **[RESOLVED]** — `|><|` kept; remaining
    primitive naming fixed at skeleton review (`decisions.md` D-Fresh/D-Wire).
