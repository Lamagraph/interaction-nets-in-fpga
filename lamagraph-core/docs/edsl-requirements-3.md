# EDSL for Interaction Net Rules — Surface Requirements (attempt 3)

This document records the feature set agreed during the design discussion that
followed `edsl-requirements-2.md`. It intentionally avoids any concrete
technical realization (no module layout, no type signatures, no library
choices); it states only *what* the EDSL must let a developer express and
*which properties* must hold. Where a target notation helps understanding,
it is shown as an informal sketch of desired surface shape only.

Relationship to attempt 2: all decisions marked **[DECIDED]** there remain in
force (type-level counts, compilation target `INet.Net`, no guards, plain
Haskell + TH, symmetry support, Duplicate as ordinary agent). This document
refines the *rule-definition surface* (R2) and answers part of the open
questions Q2/Q3/Q5 from there.

---

## 1. Rule head: pattern binding of ports

SF1. A rule is introduced by naming the two interacting agent kinds and,
    for each side, binding its **secondary ports** to local names:

    ```
    Dup (p1, p2) |><| SomeAgent sp
    ```

    This mirrors Inpla's `Dup(p1, p2) >< SomeAgent(sp)`.

SF2. Principal ports are **never bound** in the head: they are consumed by
    the active pair itself. Only secondary ports get names.

SF3. Port binders are statically scoped: every name introduced in the head is
    usable in exactly that rule's body and nowhere else (replaces the raw
    `Main`/`Pointer` addressing of the first prototype, P3/F3 of attempt 2).

SF4. An agent side with zero secondary ports binds nothing (e.g. `Erase`),
    and the head form must accommodate mixed arities naturally.

## 2. Rule body: imperative wiring block

SF5. The body is a sequential block of statements, read top-to-bottom:

    ```
    Dup (p1, p2) |><| SomeAgent sp = do
      ...
    ```

SF6. The primitive statement **wire** connects two port ends. Any combination
    must be expressible — this is the fix for P3 of attempt 2:

    - LHS secondary ↔ RHS secondary (argument-to-body wire of the β-rule),
    - LHS/RHS port ↔ port of a freshly created agent,
    - port of a fresh agent ↔ port of another fresh agent,
    - any port left unconnected must be an explicit, visible choice
      (`NotConnected` in the output), never silent.

SF7. Wiring a principal port of a fresh agent to some end is expressed as a pair
     of statements — create (`fresh`) then connect (`wire`) — so the most common
     shape — "make a copy whose input is this wire" — stays two short lines.
     *(Replaces the earlier one-statement `setPrim (freshAgent X) p` idea; see
     decisions.md D-Wire.)*

## 3. Fresh agent creation

SF8. The body can create new agents of any declared kind. Creation yields a
    value representing that concrete new agent, from which its secondary
    ports are accessible for further wiring.

SF9. Creation and principal-port connection are separate statements: a freshly
     created agent is bound with `fresh` (yielding its address and secondary
     ports), and its principal port is attached to an already-named end via
     `wire`. *(Replaces the earlier `setPrim (freshAgent X) p`; decisions.md
     D-Fresh/D-Wire.)*

     ```
     addr <- fresh SomeAgent
     wire (Pointer addr Primary) p1
     ```

SF10. A fresh agent can itself be **pattern-bound**, exactly like a head
     agent, so its secondary ports receive names directly:

     ```
     (_, dp1 :> dp2 :> Nil) <- fresh Dup
     ```

     *(Illustrative; exact binder shape depends on the chosen surface.)*

SF11. Forward references are impossible by construction: a port end may only
      be wired after the entity owning it has been bound (head binders or a
      completed creation statement).

## 4. Bulk traversal over secondary ports

SF12. Rules that fan out over all secondary ports of an agent (the general
      duplicator-vs-any-agent shape) must be writable as one traversal over
      the port list rather than enumerated per arity:

      ```
      mapM (\(p, s1, s2) -> do
              Dup (dp1, dp2) <- setPrim (freshAgent Dup) p
              wire s1 dp2
              wire s2 dp1)
           (zip3 sp (secPorts a1) (secPorts a2))
      ```

SF13. The traversal length is determined by the declared arity of the
      traversed agent, not written by hand; everything the sub-block creates
      or wires per element is accounted for automatically (see §5).

## 5. Derived static quantities (reaffirms R3)

SF14. The builder accumulates into fixed-size `Vec`s whose sizes are the
     instance-level **maximum** over all rules; individual rule bodies pad with
     `def`. *(Refines R3: counts are fixed once at the `INet` instance boundary,
     not re-derived from each body — decisions.md D-Vec.)*

SF15. Each rule body fills up to the fixed vectors and pads the remainder with
     `def`. A body that overflows its vector is a compile-time error, never
     runtime truncation. Traversals (SF12) contribute their full length against
     the same ceilings (kills P2/P5 of attempt 2).

SF16. Assignment of allocated addresses to created nodes follows the order
      of creation (`fresh`) statements: first created node receives the first
      free address (answers Q3; must be documented and tested).

## 6. Unchanged from attempt 2

The following carry over without modification and are listed for completeness
only: single agent declaration yielding label + arity (R1); one rule per
unordered pair with compile-time pair coverage (R4); mirrored dispatch from a
single body (R5); compilation to the existing `INet.Net` interface with
max-size padding (R6/D2); Clash-clean synthesized output (R7); no guards, no
built-in data values (§3.2 there); Duplicate is a plain declared agent, not
built-in machinery (D6).

## 7. Layering

SF17. The feature set above splits into two layers:

     - **Core layer**: the full functionality, expressed with ordinary typed
       combinators; no metaprogramming required to use it.
     - **Sugar layer** (optional, may come later): mechanical per-agent
       conveniences generated from agent declarations — constructor-shaped
       patterns for heads and fresh bindings (SF1, SF10 verbatim), so user
       code approaches Inpla notation exactly.

SF18. The sugar layer must be purely cosmetic: anything writable with it is
      writable in the core layer, and adding it later must not change rule
      semantics or derived counts.

## 8. Resolved and remaining open questions

Resolved by this discussion:

- Q2 (embedding style): **resolved** — direction D: builder is a `State` monad
  over fixed-size `Vec`s (decisions.md D-Builder/D-Vec), counts bound at the
  `INet` instance ceiling. Not an indexed monad, no per-body type-level counts.
- Q3 (address order): creation order, see SF16.
- Q5 resolved: the active-pair operator keeps the `|><|` spelling for
  familiarity (F1 of attempt 2); final combinator naming deferred to review
  of the skeleton.

Still open:

- Q1 (whether TH emits whole `INet` instances or helper functions);
- whether the sugar layer (SF17/SF18) is wanted for the first iteration;
- concrete naming of body primitives (creation `fresh`, wiring `wire`,
  traversal) — partially fixed (see decisions.md D-Fresh/D-Wire), remaining
  naming to be locked when the skeleton is reviewed.
