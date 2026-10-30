# EDSL history: attempts, requirements, superseded decisions

Archive of how the interaction-net rule EDSL was designed. It is **not** a
reference: the current guide is [edsl.md](edsl.md), and the API lives in the
Haddock of `INet.Edsl`, `INet.Edsl.Agents`, and `INet.Define`. This page keeps
the chronology and the ideas that were dropped so they are not rediscovered by
accident.

## Attempt 1 — the prototype in `INet/Define.hs`

The first version already had the vocabulary that survived: a rule operator
`|><|`, a GADT carrying type-level arity, `PortPointer` (`Free` / `Main` /
`Pointer`), a `State` builder accumulating edges and nodes into `Vec`s, a
wiring primitive, a node-construction primitive, dispatch on the pair of agent
constructors, a static allocation count, and the `INet` class as the target.

Its problems motivated a redesign:

| # | Problem |
|---|---|
| P1 | **Triple representation of agents** — the same agent existed as a flat label enum (stored in `Node`), an arity-carrying GADT, and an existential wrapper; three hand-written conversions had to stay in sync. |
| P2 | **Manual type-level bookkeeping** — `portsNumber`, `edgesNumber`, `nodesNumber` were free `KnownNat` parameters threaded through the builder; nothing forced them to match what a body built. |
| P3 | **Incomplete wiring** — only `Main/Main` and `Free/Main` were covered; the most common β-rule case, secondary-to-secondary across the pair, fell into `undefined`. |
| P4 | **Dispatch totality** — unknown constructor pairs hit runtime `undefined`; coverage of the declared set was unchecked. |
| P5 | **Allocation-count desync** — the count was written by hand next to rules that allocate; the mapping from fresh nodes to allocated addresses was left as a TODO. |
| P6 | **Symmetry by copy-paste** — mirrored pair orientations were enumerated manually per rule. |

## Attempt 2 — requirements

Goal: write rules close to Inpla notation and let the compiler derive the
mechanical parts — the reduction result, the static allocation count, and the
plumbing between agent names and the flat labels stored in `Node`.

Fixed upfront: compile to the existing `INet.Net.INet` class (no redesign);
no guards or multi-body rules; plain Haskell plus Template Haskell, no
quasiquoter; rule bodies written once with mirrored dispatch; `Duplicate` is an
ordinary declared agent; synthesized output stays Clash-clean (`Vec`, `Index`,
no lists, no added strictness).

Functional requirements and their intended answers:

| # | Requirement | Intended answer |
|---|---|---|
| R1 | Single agent declaration | One declaration yields the runtime label and the type-level arity; no parallel enum/GADT/existential trio. |
| R2 | Rule definition form | Pattern pair plus wires plus fresh agents; any port combination wireable; leaving a port free must be explicit. |
| R3 | Derived static counts | GHC derives created nodes/edges/allocation count from the body, so count and body cannot diverge. |
| R4 | Total dispatch | Every unordered pair has a rule or an explicit "no rule"; omission is a compile-time diagnostic. |
| R5 | Symmetry | Declaring a rule yields both pair orientations; the body is written once. |
| R6 | Instance generation boundary | Either a generated `INet` instance or helpers plus a short hand-written instance. |
| R7 | Clash-clean output | Only Clash conventions in synthesized terms. |

Success criteria were: the λ-calculus set expressible; adding an agent takes
one declaration plus one body; omitted rules fail at compile time; simulation
behaviour unchanged after migration.

Inpla scope was fixed here; the current statement of what is in and out of
scope is [edsl.md](edsl.md) §2.

## Attempt 3 — surface requirements

A follow-up discussion fixed the rule-definition surface:

- **Head (SF1–SF4):** name the two agent kinds, bind only their secondary
  ports, statically scoped binders, zero-arity agents bind nothing.
- **Body (SF5–SF7):** a sequential `do` block; one `wire` primitive covering
  every endpoint combination; creation and principal-port connection are two
  statements (`fresh` then `wire`).
- **Fresh agents (SF8–SF11):** creation yields the new node and its secondary
  ports; a fresh node can be bound like a head agent; forward references are
  impossible by construction.
- **Traversal (SF12–SF13):** rules that fan out over all secondary ports are
  one traversal over the port vector, with length taken from the declared
  arity.
- **Counts (SF14–SF16):** fixed-size vectors sized once at the instance
  ceiling, per-body padding, overflow is an error; allocated addresses follow
  creation order.
- **Layering (SF17–SF18):** a core combinator layer, plus an optional purely
  cosmetic sugar layer for Inpla-like patterns.

Resolved here: the builder is a `State` monad over fixed-size `Vec`s (not an
indexed monad); address assignment follows creation order; Template Haskell
emits helper declarations only, not whole instances; the active-pair operator
keeps the `|><|` spelling.

## What the implementation actually did

| Requirement | Outcome |
|---|---|
| R1 single declaration | Met (`INet.Edsl.Agents`). |
| R2 rule form | Partial: functions over `Vec PortPointer`; no sugar, no `|><|`. |
| R3 derived counts | **Not implemented:** counts are hand-written; overflow is runtime `errorX`. |
| R4 total dispatch | **Not implemented:** missing pairs are explicit `errorX` arms. |
| R5 symmetry | Met (same body, swapped arguments). |
| R6 generated instance | Met: generated helpers plus a hand-written instance. |
| R7 Clash-clean | Partially verified: bodies avoid lists/folds; not synthesized. |
| Sugar layer (SF17–SF18) | Not implemented. |
| Initial nets (attempt 2, Q4) | Still out of scope; fixtures remain hand-written. |

The `|><|` spelling survived the requirements but not the implementation: a
rule is a plain function, and the pair orientation is the dispatch arm's
choice.

## Rejected or deferred alternatives

Recorded so they are not re-proposed without context:

- an `AgentSet` record carrying four type-name strings and a four-field
  `AgentDecl` — redundant, because tag, singleton, and label names follow from
  the constructor name;
- deriving every type name from a set prefix (`LambdaKind`, `LambdaArity`, …)
  — rejected as churn in every documented signature;
- Template Haskell emitting whole `INet` instances rather than helper
  declarations — bodies and dispatch carry semantics and stay hand-written;
- an indexed monad with per-body type-level counts — rejected in favour of one
  ceiling per agent set;
- a quasiquoter for Inpla-like syntax — plain Haskell plus TH was chosen;
- merging the runtime label enum into `AgentKind` — left open: it would change
  `Node._nodeType` and the test normalizer.

## Superseded and relocated decisions

The design-decision records (D-*) were removed as separate documents; the
surviving rationale now sits next to the code it explains:

| Decision | Where it lives now |
|---|---|
| Agent as a singleton type tag, not a node constructor | Haddock of `INet.Edsl.AgentLabel` |
| `fresh` returns address plus secondary pointers | Haddock of `INet.Edsl.fresh` |
| Creation and wiring are separate statements | Haddock of `INet.Edsl.wire` / `fresh` |
| Builder is `State` over fixed-size vectors | Haddock of `INet.Edsl.RuleState` / `RuleBuilder` |
| Vector sizes fixed at the instance ceiling | Haddock of `INet.Edsl.RuleState`; the old D-Vec claim that overflow is a compile-time error was wrong — the implementation aborts with `errorX` |
| Arity is type-level; runtime nodes use the maximum arity | Haddock of `INet.Edsl.AgentLabel`, `Core.Node` |
| `Duplicate` is an ordinary agent | `INet.Define` module header |
| One body, two dispatch entries | `INet.Define` module header |
| Only secondary ports are bound | Haddock of `INet.Edsl.nodeSecondaries` |
| Generic machinery vs concrete usage split | Module headers of `INet.Edsl` and `INet.Define` |
| TH generates the declaration skeleton only | Module header of `INet.Edsl.Agents` |
