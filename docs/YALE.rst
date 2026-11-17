.. _yale:

======================================
YALE interaction nets (reference)
======================================

A reference extraction of the **YALE** interaction-net system — the agent
alphabet and the complete set of reduction rules — from Ian Mackie, *YALE: Yet
Another Lambda Evaluator Based on Interaction Nets* (1998). The focus is the
core weak-reduction system (paper pp. 122–124) plus the strong-reduction
``φ`` rules (p. 125).

The goal is an **implementation-precise** specification: every rule states the
exact primary/secondary port identification and every connection, so a rule can
be transcribed directly into a graph-rewriting implementation without
ambiguity.

Source
======

- Paper: Ian Mackie, "YALE: Yet Another Lambda Evaluator Based on Interaction
  Nets", 1998. Local copy: ``~/Downloads/291251.289434.pdf``.
- Core rules: paper pp. 122–124 (PDF pp. 6–8).
- Strong-reduction ``φ`` rules: paper p. 125 (PDF p. 9).

Terminology
===========

This document uses the terminology of :doc:`../README <../README>` (the
project interaction-net formalism). The mapping to the paper's wording:

+----------------------------------+----------------------------------+
| This doc / ``README``             | Paper (Mackie 1998)              |
+==================================+==================================+
| primary port                     | principal port                   |
+----------------------------------+----------------------------------+
| secondary port                   | auxiliary port                   |
+----------------------------------+----------------------------------+
| active pair ``a ⋈ b``            | active pair                      |
+----------------------------------+----------------------------------+
| reduction rule ``(l₁, l₂, n)``   | interaction rule                 |
+----------------------------------+----------------------------------+
| ``Ar(l)`` (secondary arity)      | arity                            |
+----------------------------------+----------------------------------+

Recall the ``README`` axioms for a rule ``(l₁, l₂, n)``: symmetry (``(l₂, l₁,
n)`` also holds), at most one rule per agent pair, ``n`` is in normal form, and
the **arity axiom** ``Ar(l₁) + Ar(l₂) = |I(n)|`` where ``I(n)`` is the
interface (set of free ports) of ``n``. Every rule below satisfies the arity
axiom; the free-port count ``|I(n)|`` is given for each.

Graphical notation
==================

How to read the paper's figures (and the Mermaid renderings below):

- **Agent** — a circle labelled with its symbol (``λ``, ``b``, ``δ``, …).
- **Primary port** — marked by an **arrowhead** on its wire. An agent has
  exactly one. *A primary port may be free (unconnected); it is still drawn and
  arrowed.*
- **Secondary port** — a plain stub (no arrowhead).
- **Free port** — a port with no connection: a dangling stub, or an end of a
  bare edge. The free ports of the RHS are the interface ``I(n)``.
- **Active pair** — the two primary ports joined by an edge (drawn with
  arrowheads). Written ``a ⋈ b``.
- **Bare edge** — an edge with no agent at either end; each end is a free port.
  A bare edge *splices* the two surrounding subnets together.
- **Rule** — ``LHS ⇒ RHS``. The LHS shows the two agents, their active pair,
  and their free (secondary) ports. The RHS shows the result net ``n``: fresh
  agents, internal edges, bare edges, and free ports.
- **Port indexing** — clockwise from the primary port: ``(P, A1, A2, …)``. The
  indexing is rotation-invariant (only the cyclic order matters).

Mermaid idiom
=============

The figures below use Mermaid ``flowchart``. Legend:

- One ``flowchart LR`` per side (LHS and RHS are separate diagrams).
- **Agent** = a ``subgraph`` titled with the agent symbol.
- **Primary port** = a *stadium* node ``xP(["P"])``. **Always drawn, even when
  free** — a free primary port is a stadium node with no edges.
- **Secondary port** = a *rectangle* node ``xA1["A1"]``.
- **Net edge** (a connection between two ports) = ``---``.
- **Active pair** = a *thick* edge ``===`` between the two primary-port nodes.
- **Free port** = a node whose only edge is its interface dot.
- **Interface port** = a *circle* node ``ik(("I_k"))`` attached to a free
  port — an LHS secondary stub or an RHS free port. The same ``I_k`` appears
  on the LHS port and on the RHS free port it becomes.
- **Bare edge** = ``---`` between two free-port nodes, each labelled by the LHS
  port it stands for.

For each rule the **textual spec** (active pair, fresh agents, internal edges,
interface) is the authoritative statement; the diagrams are a visual aid. The
**interface** numbers the LHS secondary ports ``I₁..I_N`` (``N = Ar(l₁) +
Ar(l₂)``; ``l₁``'s ports first, then ``l₂``'s) and states, for each, what it
becomes on the RHS: ``I_k = a.p`` means interface port ``I_k`` becomes the
fresh agent port ``a.p``; ``I_k = bare edge (with I_j)`` means interface ports
``I_k`` and ``I_j`` are the two ends of a bare edge (spliced together). The
**internal edges** list only connections between fresh-agent ports and the
**interface** lists only free ports, so the two blocks are disjoint.

Agents
======

+--------+------+------------------------------------------------+
| Agent  | Ar   | Role                                           |
+========+======+================================================+
| ``k``  | 0    | discard / constant                             |
+--------+------+------------------------------------------------+
| ``ν``  | 0    | void / empty list                              |
+--------+------+------------------------------------------------+
| ``ε``  | 0    | eraser                                         |
+--------+------+------------------------------------------------+
| ``d``  | 0    | box-opener / delay                             |
+--------+------+------------------------------------------------+
| ``@``  | 2    | application                                    |
+--------+------+------------------------------------------------+
| ``c``  | 2    | copy                                           |
+--------+------+------------------------------------------------+
| ``δ``  | 2    | delay / let / duplicator                       |
+--------+------+------------------------------------------------+
| ``φ``  | 1    | strong-reduction pass-through (p. 125 only)    |
+--------+------+------------------------------------------------+
| ``λ``  | 3    | abstraction                                    |
+--------+------+------------------------------------------------+
| ``b``  | 3    | substitution                                   |
+--------+------+------------------------------------------------+
| ``b′`` | 3    | lifted substitution / shift                    |
+--------+------+------------------------------------------------+

Port roles
----------

The paper does not name the secondary-port roles. The roles below are
**derived from the rules** as a mnemonic for what each port typically carries;
they are not a paper statement, and they are not a preservation law — a rule's
interface connects each LHS port to an RHS port (see each rule's **Interface**
block), and in some rules a port's role changes (R7 and R11 in particular).
Ports are indexed ``(P, A1, A2, …)`` clockwise from the primary port (see
*Graphical notation*); the parenthetical cites the rule that most clearly
establishes the role.

- ``k`` (Ar 0) — ``P``: held term (R3).
- ``ν`` (Ar 0) — ``P``: empty-list marker (R2).
- ``ε`` (Ar 0) — ``P``: erasure (R16).
- ``d`` (Ar 0) — ``P``: box-opening / list continuation (R1, R2).
- ``@`` (Ar 2) — ``P``: root; ``A1``: result; ``A2``: argument (R1).
- ``c`` (Ar 2) — ``P``: copy root; ``A1``, ``A2``: symmetric copy outputs
  (R6).
- ``δ`` (Ar 2) — ``P``: outer / box-opening; ``A1``, ``A2``: symmetric inner
  (R9, R10).
- ``φ`` (Ar 1) — ``P``: front; ``A1``: back. Strong reduction only (p. 125);
  no core rule uses it.
- ``λ`` (Ar 3) — ``P``: root; ``A1``: bound variable; ``A2``: list head;
  ``A3``: body root (R1).
- ``b`` (Ar 3) — ``P``: box / target; ``A1``: list link; ``A2``: term /
  occurrence; ``A3``: list link (R3).
- ``b′`` (Ar 3) — ``P``: list link; ``A1``: term; ``A2``: list link;
  ``A3``: box port (R10). A one-step cyclic shift of ``b``'s roles — the
  reason it is called the *lifted* (shifted) substitution.

.. note::

   The rule-derived ``λ`` roles disagree with the paper's translation figure
   in places; the roles above are the ones the 16 rules actually use.

Rules
=====

Rule 1: λ ⋈ @ (β-reduction)
---------------------------

``(λ, @, n)`` — β-reduction: the body is spliced into the result position and
the argument into the bound-variable edge; the list head is handed to a fresh
``d``.

**LHS**

.. code-block:: mermaid

  flowchart LR
    subgraph lam["λ"]
      lamP(["λ.P"])
      lamA1["λ.A1"]
      lamA2["λ.A2"]
      lamA3["λ.A3"]
    end
    subgraph at["@"]
      atP(["@.P"])
      atA1["@.A1"]
      atA2["@.A2"]
    end
    lamP === atP
    lamA1 --- i1(("I₁"))
    lamA2 --- i2(("I₂"))
    lamA3 --- i3(("I₃"))
    atA1 --- i4(("I₄"))
    atA2 --- i5(("I₅"))

**RHS**

.. code-block:: mermaid

  flowchart LR
    subgraph d["d"]
      dP(["d.P"])
    end
    e1["λ.A3"] --- e2["@.A1"]
    e3["λ.A1"] --- e4["@.A2"]
    dP --- i2(("I₂"))
    e1 --- i3(("I₃"))
    e2 --- i4(("I₄"))
    e3 --- i1(("I₁"))
    e4 --- i5(("I₅"))

**Spec**

- **Active pair:** ``λ.P ⋈ @.P``
- **Fresh agents:** ``d``
- **Internal edges:** —
- **Interface (|I| = 5):**
  - I₁ = ``λ.A1`` → bare edge (with I₅)
  - I₂ = ``λ.A2`` → ``d.P``
  - I₃ = ``λ.A3`` → bare edge (with I₄)
  - I₄ = ``@.A1`` → bare edge (with I₃)
  - I₅ = ``@.A2`` → bare edge (with I₁)

Rule 2: d ⋈ ν (erase empty list)
--------------------------------

``(d, ν, n)`` — if the reduced term was a closed ``λ``, the ``d`` introduced by
rule 1 simply erases the ``ν`` that marks the empty list.

**LHS**

.. code-block:: mermaid

  flowchart LR
    subgraph d["d"]
      dP(["d.P"])
    end
    subgraph nu["ν"]
      nuP(["ν.P"])
    end
    dP === nuP

**RHS** — the empty net (no agents, no edges).

**Spec**

- **Active pair:** ``d.P ⋈ ν.P``
- **Fresh agents:** —
- **Interface (|I| = 0):** — (no interface ports)

Rule 3: b ⋈ k (substitute a constant)
-------------------------------------

``(b, k, n)`` — a substitution whose target is the discard agent ``k``: the
occurrence edge is handed to a fresh ``k`` and the two list links are spliced.

**LHS**

.. code-block:: mermaid

  flowchart LR
    subgraph b["b"]
      bP(["b.P"])
      bA1["b.A1"]
      bA2["b.A2"]
      bA3["b.A3"]
    end
    subgraph k["k"]
      kP(["k.P"])
    end
    bP === kP
    bA1 --- i1(("I₁"))
    bA2 --- i2(("I₂"))
    bA3 --- i3(("I₃"))

**RHS**

.. code-block:: mermaid

  flowchart LR
    subgraph k["k"]
      kP(["k.P"])
    end
    e1["b.A1"] --- e2["b.A3"]
    kP --- i2(("I₂"))
    e1 --- i1(("I₁"))
    e2 --- i3(("I₃"))

**Spec**

- **Active pair:** ``b.P ⋈ k.P``
- **Fresh agents:** ``k``
- **Internal edges:** —
- **Interface (|I| = 3):**
  - I₁ = ``b.A1`` → bare edge (with I₃)
  - I₂ = ``b.A2`` → ``k.P``
  - I₃ = ``b.A3`` → bare edge (with I₁)

Rule 4: b ⋈ λ (push substitution through abstraction)
-----------------------------------------------------

``(b, λ, n)`` — a substitution is pushed through an abstraction: the
abstraction is copied (fresh ``λ′``) and the substitution is re-targeted at the
copy's box (fresh ``b``), with the body re-wired.

**LHS**

.. code-block:: mermaid

  flowchart LR
    subgraph b["b"]
      bP(["b.P"])
      bA1["b.A1"]
      bA2["b.A2"]
      bA3["b.A3"]
    end
    subgraph lam["λ"]
      lamP(["λ.P"])
      lamA1["λ.A1"]
      lamA2["λ.A2"]
      lamA3["λ.A3"]
    end
    bP === lamP
    bA1 --- i1(("I₁"))
    bA2 --- i2(("I₂"))
    bA3 --- i3(("I₃"))
    lamA1 --- i4(("I₄"))
    lamA2 --- i5(("I₅"))
    lamA3 --- i6(("I₆"))

**RHS**

.. code-block:: mermaid

  flowchart LR
    subgraph lp["λ′"]
      lpP(["λ′.P"])
      lpA1["λ′.A1"]
      lpA2["λ′.A2"]
      lpA3["λ′.A3"]
    end
    subgraph b2["b"]
      b2P(["b.P"])
      b2A1["b.A1"]
      b2A2["b.A2"]
      b2A3["b.A3"]
    end
    lpA3 --- b2A2
    lpP --- i2(("I₂"))
    lpA1 --- i4(("I₄"))
    lpA2 --- i5(("I₅"))
    b2P --- i6(("I₆"))
    b2A1 --- i1(("I₁"))
    b2A3 --- i3(("I₃"))

**Spec**

- **Active pair:** ``b.P ⋈ λ.P``
- **Fresh agents:** ``λ′``, ``b``
- **Internal edges:** ``(λ′.A3, b.A2)``
- **Interface (|I| = 6):**
  - I₁ = ``b.A1`` → ``b.A1``
  - I₂ = ``b.A2`` → ``λ′.P``
  - I₃ = ``b.A3`` → ``b.A3``
  - I₄ = ``λ.A1`` → ``λ′.A1``
  - I₅ = ``λ.A2`` → ``λ′.A2``
  - I₆ = ``λ.A3`` → ``b.P``

Rule 5: b ⋈ ν (substitute into empty list)
------------------------------------------

``(b, ν, n)`` — a substitution whose list is empty (``ν``): the occurrence edge
is handed to a fresh ``ν`` and the two list links are spliced.

**LHS**

.. code-block:: mermaid

  flowchart LR
    subgraph b["b"]
      bP(["b.P"])
      bA1["b.A1"]
      bA2["b.A2"]
      bA3["b.A3"]
    end
    subgraph nu["ν"]
      nuP(["ν.P"])
    end
    bP === nuP
    bA1 --- i1(("I₁"))
    bA2 --- i2(("I₂"))
    bA3 --- i3(("I₃"))

**RHS**

.. code-block:: mermaid

  flowchart LR
    subgraph nu["ν"]
      nuP(["ν.P"])
    end
    e1["b.A1"] --- e2["b.A3"]
    nuP --- i2(("I₂"))
    e1 --- i1(("I₁"))
    e2 --- i3(("I₃"))

**Spec**

- **Active pair:** ``b.P ⋈ ν.P``
- **Fresh agents:** ``ν``
- **Internal edges:** —
- **Interface (|I| = 3):**
  - I₁ = ``b.A1`` → bare edge (with I₃)
  - I₂ = ``b.A2`` → ``ν.P``
  - I₃ = ``b.A3`` → bare edge (with I₁)

Rule 6: c ⋈ k (copy a constant)
-------------------------------

``(c, k, n)`` — copying against the discard agent yields two independent
``k`` agents, one per copy input.

**LHS**

.. code-block:: mermaid

  flowchart LR
    subgraph c["c"]
      cP(["c.P"])
      cA1["c.A1"]
      cA2["c.A2"]
    end
    subgraph k["k"]
      kP(["k.P"])
    end
    cP === kP
    cA1 --- i1(("I₁"))
    cA2 --- i2(("I₂"))

**RHS**

.. code-block:: mermaid

  flowchart LR
    subgraph k1["k₁"]
      k1P(["k₁.P"])
    end
    subgraph k2["k₂"]
      k2P(["k₂.P"])
    end
    k1P --- i1(("I₁"))
    k2P --- i2(("I₂"))

**Spec**

- **Active pair:** ``c.P ⋈ k.P``
- **Fresh agents:** ``k₁``, ``k₂``
- **Internal edges:** —
- **Interface (|I| = 2):**
  - I₁ = ``c.A1`` → ``k₁.P``
  - I₂ = ``c.A2`` → ``k₂.P``

Rule 7: c ⋈ λ (copy an abstraction)
-----------------------------------

``(c, λ, n)`` — an abstraction is duplicated: two fresh copies ``λ₁``, ``λ₂``
share a fresh ``c′`` for the body, and two fresh ``δ`` agents delay the two
bound-variable edges.

**LHS**

.. code-block:: mermaid

  flowchart LR
    subgraph c["c"]
      cP(["c.P"])
      cA1["c.A1"]
      cA2["c.A2"]
    end
    subgraph lam["λ"]
      lamP(["λ.P"])
      lamA1["λ.A1"]
      lamA2["λ.A2"]
      lamA3["λ.A3"]
    end
    cP === lamP
    cA1 --- i1(("I₁"))
    cA2 --- i2(("I₂"))
    lamA1 --- i3(("I₃"))
    lamA2 --- i4(("I₄"))
    lamA3 --- i5(("I₅"))

**RHS**

.. code-block:: mermaid

  flowchart LR
    subgraph cp["c′"]
      cpP(["c′.P"])
      cpA1["c′.A1"]
      cpA2["c′.A2"]
    end
    subgraph l1["λ₁"]
      l1P(["λ₁.P"])
      l1A1["λ₁.A1"]
      l1A2["λ₁.A2"]
      l1A3["λ₁.A3"]
    end
    subgraph l2["λ₂"]
      l2P(["λ₂.P"])
      l2A1["λ₂.A1"]
      l2A2["λ₂.A2"]
      l2A3["λ₂.A3"]
    end
    subgraph d1["δ₁"]
      d1P(["δ₁.P"])
      d1A1["δ₁.A1"]
      d1A2["δ₁.A2"]
    end
    subgraph d2["δ₂"]
      d2P(["δ₂.P"])
      d2A1["δ₂.A1"]
      d2A2["δ₂.A2"]
    end
    cpA1 --- l1A3
    cpA2 --- l2A3
    l1A2 --- d1A1
    l2A2 --- d1A2
    l1A1 --- d2A1
    l2A1 --- d2A2
    cpP --- i5(("I₅"))
    l1P --- i1(("I₁"))
    l2P --- i2(("I₂"))
    d1P --- i4(("I₄"))
    d2P --- i3(("I₃"))

**Spec**

- **Active pair:** ``c.P ⋈ λ.P``
- **Fresh agents:** ``c′``, ``λ₁``, ``λ₂``, ``δ₁``, ``δ₂``
- **Internal edges:** ``(c′.A1, λ₁.A3)``, ``(c′.A2, λ₂.A3)``, ``(λ₁.A2, δ₁.A1)``,
  ``(λ₂.A2, δ₁.A2)``, ``(λ₁.A1, δ₂.A1)``, ``(λ₂.A1, δ₂.A2)``
- **Interface (|I| = 5):**
  - I₁ = ``c.A1`` → ``λ₁.P``
  - I₂ = ``c.A2`` → ``λ₂.P``
  - I₃ = ``λ.A1`` → ``δ₂.P``
  - I₄ = ``λ.A2`` → ``δ₁.P``
  - I₅ = ``λ.A3`` → ``c′.P``

Rule 8: c ⋈ ν (copy the empty list)
-----------------------------------

``(c, ν, n)`` — copying the empty-list marker yields two independent ``ν``
agents.

**LHS**

.. code-block:: mermaid

  flowchart LR
    subgraph c["c"]
      cP(["c.P"])
      cA1["c.A1"]
      cA2["c.A2"]
    end
    subgraph nu["ν"]
      nuP(["ν.P"])
    end
    cP === nuP
    cA1 --- i1(("I₁"))
    cA2 --- i2(("I₂"))

**RHS**

.. code-block:: mermaid

  flowchart LR
    subgraph nu1["ν₁"]
      nu1P(["ν₁.P"])
    end
    subgraph nu2["ν₂"]
      nu2P(["ν₂.P"])
    end
    nu1P --- i1(("I₁"))
    nu2P --- i2(("I₂"))

**Spec**

- **Active pair:** ``c.P ⋈ ν.P``
- **Fresh agents:** ``ν₁``, ``ν₂``
- **Internal edges:** —
- **Interface (|I| = 2):**
  - I₁ = ``c.A1`` → ``ν₁.P``
  - I₂ = ``c.A2`` → ``ν₂.P``

Rule 9: δ ⋈ δ (cross the delays)
--------------------------------

``(δ, δ, n)`` — two delay agents meet: their auxiliary ports are crossed and
spliced, no fresh agents.

**LHS**

.. code-block:: mermaid

  flowchart LR
    subgraph d1["δ₁"]
      d1P(["δ₁.P"])
      d1A1["δ₁.A1"]
      d1A2["δ₁.A2"]
    end
    subgraph d2["δ₂"]
      d2P(["δ₂.P"])
      d2A1["δ₂.A1"]
      d2A2["δ₂.A2"]
    end
    d1P === d2P
    d1A1 --- i1(("I₁"))
    d1A2 --- i2(("I₂"))
    d2A1 --- i3(("I₃"))
    d2A2 --- i4(("I₄"))

**RHS**

.. code-block:: mermaid

  flowchart LR
    e1["δ₁.A1"] --- e2["δ₂.A2"]
    e3["δ₁.A2"] --- e4["δ₂.A1"]
    e1 --- i1(("I₁"))
    e2 --- i4(("I₄"))
    e3 --- i2(("I₂"))
    e4 --- i3(("I₃"))

**Spec**

- **Active pair:** ``δ₁.P ⋈ δ₂.P``
- **Fresh agents:** —
- **Internal edges:** —
- **Interface (|I| = 4):**
  - I₁ = ``δ₁.A1`` → bare edge (with I₄)
  - I₂ = ``δ₁.A2`` → bare edge (with I₃)
  - I₃ = ``δ₂.A1`` → bare edge (with I₂)
  - I₄ = ``δ₂.A2`` → bare edge (with I₁)

Rule 10: b ⋈ δ (substitution blocks a delay)
--------------------------------------------

``(b, δ, n)`` — a substitution meets a delay: the substitution becomes a
lifted substitution ``b′`` and its box port is wired to the delay, blocking
further reduction until the box is opened.

**LHS**

.. code-block:: mermaid

  flowchart LR
    subgraph b["b"]
      bP(["b.P"])
      bA1["b.A1"]
      bA2["b.A2"]
      bA3["b.A3"]
    end
    subgraph del["δ"]
      delP(["δ.P"])
      delA1["δ.A1"]
      delA2["δ.A2"]
    end
    bP === delP
    bA1 --- i1(("I₁"))
    bA2 --- i2(("I₂"))
    bA3 --- i3(("I₃"))
    delA1 --- i4(("I₄"))
    delA2 --- i5(("I₅"))

**RHS**

.. code-block:: mermaid

  flowchart LR
    subgraph bp["b′"]
      bpP(["b′.P"])
      bpA1["b′.A1"]
      bpA2["b′.A2"]
      bpA3["b′.A3"]
    end
    subgraph del["δ"]
      delP(["δ.P"])
      delA1["δ.A1"]
      delA2["δ.A2"]
    end
    bpA3 --- delP
    bpP --- i1(("I₁"))
    bpA1 --- i2(("I₂"))
    bpA2 --- i3(("I₃"))
    delA1 --- i4(("I₄"))
    delA2 --- i5(("I₅"))

**Spec**

- **Active pair:** ``b.P ⋈ δ.P``
- **Fresh agents:** ``b′``, ``δ``
- **Internal edges:** ``(b′.A3, δ.P)``
- **Interface (|I| = 5):**
  - I₁ = ``b.A1`` → ``b′.P``
  - I₂ = ``b.A2`` → ``b′.A1``
  - I₃ = ``b.A3`` → ``b′.A2``
  - I₄ = ``δ.A1`` → ``δ.A1``
  - I₅ = ``δ.A2`` → ``δ.A2``

Rule 11: δ ⋈ b′ (open a lifted substitution)
--------------------------------------------

``(δ, b′, n)`` — a delay opens a lifted substitution: the ``b′`` is split into
two ``b`` agents interleaved with three fresh ``δ`` agents.

**LHS**

.. code-block:: mermaid

  flowchart LR
    subgraph del["δ"]
      delP(["δ.P"])
      delA1["δ.A1"]
      delA2["δ.A2"]
    end
    subgraph bp["b′"]
      bpP(["b′.P"])
      bpA1["b′.A1"]
      bpA2["b′.A2"]
      bpA3["b′.A3"]
    end
    delP === bpP
    delA1 --- i1(("I₁"))
    delA2 --- i2(("I₂"))
    bpA1 --- i3(("I₃"))
    bpA2 --- i4(("I₄"))
    bpA3 --- i5(("I₅"))

**RHS**

.. code-block:: mermaid

  flowchart LR
    subgraph b1["b₁"]
      b1P(["b₁.P"])
      b1A1["b₁.A1"]
      b1A2["b₁.A2"]
      b1A3["b₁.A3"]
    end
    subgraph b2["b₂"]
      b2P(["b₂.P"])
      b2A1["b₂.A1"]
      b2A2["b₂.A2"]
      b2A3["b₂.A3"]
    end
    subgraph d1["δ₁"]
      d1P(["δ₁.P"])
      d1A1["δ₁.A1"]
      d1A2["δ₁.A2"]
    end
    subgraph d2["δ₂"]
      d2P(["δ₂.P"])
      d2A1["δ₂.A1"]
      d2A2["δ₂.A2"]
    end
    subgraph d3["δ₃"]
      d3P(["δ₃.P"])
      d3A1["δ₃.A1"]
      d3A2["δ₃.A2"]
    end
    b1A3 --- d1A2
    b2A2 --- d1A1
    b1P --- d2A2
    b2A3 --- d2A1
    b1A1 --- d3A2
    b2P --- d3A1
    d1P --- i3(("I₃"))
    d2P --- i4(("I₄"))
    d3P --- i5(("I₅"))
    b1A2 --- i2(("I₂"))
    b2A1 --- i1(("I₁"))

**Spec**

- **Active pair:** ``δ.P ⋈ b′.P``
- **Fresh agents:** ``b₁``, ``b₂``, ``δ₁``, ``δ₂``, ``δ₃``
- **Internal edges:** ``(b₁.A3, δ₁.A2)``, ``(b₂.A2, δ₁.A1)``, ``(b₁.P, δ₂.A2)``,
  ``(b₂.A3, δ₂.A1)``, ``(b₁.A1, δ₃.A2)``, ``(b₂.P, δ₃.A1)``
- **Interface (|I| = 5):**
  - I₁ = ``δ.A1`` → ``b₂.A1``
  - I₂ = ``δ.A2`` → ``b₁.A2``
  - I₃ = ``b′.A1`` → ``δ₁.P``
  - I₄ = ``b′.A2`` → ``δ₂.P``
  - I₅ = ``b′.A3`` → ``δ₃.P``

.. note::

   All five interface ports are determined by the figure. The three ``b′``
   ports map to the three fresh ``δ`` primary ports (``I₃ → δ₁.P``,
   ``I₄ → δ₂.P``, ``I₅ → δ₃.P``) and the two ``δ`` inner ports map to the two
   free ``b`` ports (``I₁ → b₂.A1``, ``I₂ → b₁.A2``). The internal wiring is a
   *shifted* pairing: each fresh ``δ`` receives one port from each ``b``
   (``δ₁``: ``b₁.A3``, ``b₂.A2``; ``δ₂``: ``b₁.P``, ``b₂.A3``; ``δ₃``:
   ``b₁.A1``, ``b₂.P``).

Rule 12: δ ⋈ α (duplicate an agent, schema)
-------------------------------------------

``(δ, α, n)`` — rule *scheme*: a delay duplicates an agent ``α`` of arity
``n``. The agent is copied (``α₁``, ``α₂``) and each of its ``n`` auxiliary
ports is fanned out through a fresh ``δⱼ``.

**LHS**

.. code-block:: mermaid

  flowchart LR
    subgraph del["δ"]
      delP(["δ.P"])
      delA1["δ.A1"]
      delA2["δ.A2"]
    end
    subgraph al["α"]
      alP(["α.P"])
      alA1["α.A1"]
      alAdots["⋯"]
      alAn["α.An"]
    end
    delP === alP
    delA1 --- i1(("I₁"))
    delA2 --- i2(("I₂"))
    alA1 --- i3(("I₃"))
    alAn --- in2(("Iₙ₊₂"))

**RHS** (shown for ``j = 1`` and ``j = n``; the pattern repeats per ``j``)

.. code-block:: mermaid

  flowchart LR
    subgraph a1["α₁"]
      a1P(["α₁.P"])
      a1A1["α₁.A1"]
      a1An["α₁.An"]
    end
    subgraph a2["α₂"]
      a2P(["α₂.P"])
      a2A1["α₂.A1"]
      a2An["α₂.An"]
    end
    subgraph d1["δ₁"]
      d1P(["δ₁.P"])
      d1A1["δ₁.A1"]
      d1A2["δ₁.A2"]
    end
    subgraph dn["δₙ"]
      dnP(["δₙ.P"])
      dnA1["δₙ.A1"]
      dnA2["δₙ.A2"]
    end
    a1A1 --- d1A1
    a2A1 --- d1A2
    a1An --- dnA1
    a2An --- dnA2
    a1P --- i1(("I₁"))
    a2P --- i2(("I₂"))
    d1P --- i3(("I₃"))
    dnP --- in2(("Iₙ₊₂"))

**Spec**

- **Active pair:** ``δ.P ⋈ α.P``
- **Fresh agents:** ``α₁``, ``α₂``, ``δ₁ … δₙ``
- **Internal edges:** for each ``j = 1..n``: ``(α₁.Aj, δⱼ.A1)`` and
  ``(α₂.Aj, δⱼ.A2)``
- **Interface (|I| = n + 2):**
  - I₁ = ``δ.A1`` → ``α₁.P``
  - I₂ = ``δ.A2`` → ``α₂.P``
  - Iⱼ₊₂ = ``α.Aj`` → ``δⱼ.P`` (for ``j = 1..n``)

Rule 13: b ⋈ b′ (push a substitution through a lift)
----------------------------------------------------

``(b, b′, n)`` — a substitution is pushed through a lifted substitution: the
result is two ``b′`` agents and one ``b``.

**LHS**

.. code-block:: mermaid

  flowchart LR
    subgraph b["b"]
      bP(["b.P"])
      bA1["b.A1"]
      bA2["b.A2"]
      bA3["b.A3"]
    end
    subgraph bp["b′"]
      bpP(["b′.P"])
      bpA1["b′.A1"]
      bpA2["b′.A2"]
      bpA3["b′.A3"]
    end
    bP === bpP
    bA1 --- i1(("I₁"))
    bA2 --- i2(("I₂"))
    bA3 --- i3(("I₃"))
    bpA1 --- i4(("I₄"))
    bpA2 --- i5(("I₅"))
    bpA3 --- i6(("I₆"))

**RHS**

.. code-block:: mermaid

  flowchart LR
    subgraph bp1["b′₁"]
      bp1P(["b′₁.P"])
      bp1A1["b′₁.A1"]
      bp1A2["b′₁.A2"]
      bp1A3["b′₁.A3"]
    end
    subgraph bp2["b′₂"]
      bp2P(["b′₂.P"])
      bp2A1["b′₂.A1"]
      bp2A2["b′₂.A2"]
      bp2A3["b′₂.A3"]
    end
    subgraph b["b"]
      bP(["b.P"])
      bA1["b.A1"]
      bA2["b.A2"]
      bA3["b.A3"]
    end
    bp1A3 --- bp2A1
    bp1A2 --- bA2
    bp2A2 --- bA1
    bp1P --- i1(("I₁"))
    bp1A1 --- i2(("I₂"))
    bA3 --- i3(("I₃"))
    bp2P --- i5(("I₅"))
    bp2A3 --- i4(("I₄/I₆"))
    bP --- i6(("I₄/I₆"))

**Spec**

- **Active pair:** ``b.P ⋈ b′.P``
- **Fresh agents:** ``b′₁``, ``b′₂``, ``b``
- **Internal edges:** ``(b′₁.A3, b′₂.A1)``, ``(b′₁.A2, b.A2)``,
  ``(b′₂.A2, b.A1)``
- **Interface (|I| = 6):**
  - I₁ = ``b.A1`` → ``b′₁.P``
  - I₂ = ``b.A2`` → ``b′₁.A1``
  - I₃ = ``b.A3`` → ``b.A3``
  - I₄ = ``b′.A1`` → ``b′₂.A3`` or ``b.P``
  - I₅ = ``b′.A2`` → ``b′₂.P``
  - I₆ = ``b′.A3`` → ``b.P`` or ``b′₂.A3``

.. note::

   The three ``b``-side ports (I₁, I₂, I₃) and I₅ are mapped with confidence:
   I₅ = ``b′.A2 → b′₂.P`` is forced by the list-link role. The remaining two
   ``b′`` ports (I₄ = ``b′.A1``, I₆ = ``b′.A3``) form a 2-element ambiguity
   onto ``{b′₂.A3, b.P}``; the figure does not disambiguate which goes where,
   so both pairings are listed as alternatives.

Rule 14: c ⋈ b′ (copy a lifted substitution)
--------------------------------------------

``(c, b′, n)`` — a lifted substitution is copied: two ``b′`` agents, two ``c``
agents, and one ``δ``.

**LHS**

.. code-block:: mermaid

  flowchart LR
    subgraph c["c"]
      cP(["c.P"])
      cA1["c.A1"]
      cA2["c.A2"]
    end
    subgraph bp["b′"]
      bpP(["b′.P"])
      bpA1["b′.A1"]
      bpA2["b′.A2"]
      bpA3["b′.A3"]
    end
    cP === bpP
    cA1 --- i1(("I₁"))
    cA2 --- i2(("I₂"))
    bpA1 --- i3(("I₃"))
    bpA2 --- i4(("I₄"))
    bpA3 --- i5(("I₅"))

**RHS**

.. code-block:: mermaid

  flowchart LR
    subgraph bp1["b′₁"]
      bp1P(["b′₁.P"])
      bp1A1["b′₁.A1"]
      bp1A2["b′₁.A2"]
      bp1A3["b′₁.A3"]
    end
    subgraph bp2["b′₂"]
      bp2P(["b′₂.P"])
      bp2A1["b′₂.A1"]
      bp2A2["b′₂.A2"]
      bp2A3["b′₂.A3"]
    end
    subgraph c1["c₁"]
      c1P(["c₁.P"])
      c1A1["c₁.A1"]
      c1A2["c₁.A2"]
    end
    subgraph c2["c₂"]
      c2P(["c₂.P"])
      c2A1["c₂.A1"]
      c2A2["c₂.A2"]
    end
    subgraph del["δ"]
      delP(["δ.P"])
      delA1["δ.A1"]
      delA2["δ.A2"]
    end
    delA2 --- bp1A1
    delA1 --- bp2A1
    bp1A2 --- c1A2
    bp1A3 --- c2A1
    bp2A2 --- c1A1
    bp2A3 --- c2A2
    bp1P --- i1(("I₁"))
    bp2P --- i2(("I₂"))
    delP --- i3(("I₃"))
    c1P --- i4(("I₄"))
    c2P --- i5(("I₅"))

**Spec**

- **Active pair:** ``c.P ⋈ b′.P``
- **Fresh agents:** ``b′₁``, ``b′₂``, ``c₁``, ``c₂``, ``δ``
- **Internal edges:** ``(δ.A2, b′₁.A1)``, ``(δ.A1, b′₂.A1)``, ``(b′₁.A2, c₁.A2)``,
  ``(b′₁.A3, c₂.A1)``, ``(b′₂.A2, c₁.A1)``, ``(b′₂.A3, c₂.A2)``
- **Interface (|I| = 5):**
  - I₁ = ``c.A1`` → ``b′₁.P``
  - I₂ = ``c.A2`` → ``b′₂.P``
  - I₃ = ``b′.A1`` → ``δ.P``
  - I₄ = ``b′.A2`` → ``c₁.P``
  - I₅ = ``b′.A3`` → ``c₂.P``

Rule 15: d ⋈ b′ (erase a lifted substitution)
---------------------------------------------

``(d, b′, n)`` — the final ``b′`` rule: the ``d`` agent that opens the box of
the abstraction erases the ``b′``.

**LHS**

.. code-block:: mermaid

  flowchart LR
    subgraph d["d"]
      dP(["d.P"])
    end
    subgraph bp["b′"]
      bpP(["b′.P"])
      bpA1["b′.A1"]
      bpA2["b′.A2"]
      bpA3["b′.A3"]
    end
    dP === bpP
    bpA1 --- i1(("I₁"))
    bpA2 --- i2(("I₂"))
    bpA3 --- i3(("I₃"))

**RHS** (the paper draws the preserved ``b′`` ports as a single vertical bar)

.. code-block:: mermaid

  flowchart LR
    subgraph d["d"]
      dP(["d.P"])
    end
    f1["b′.A1"]
    f2["b′.A2"]
    f3["b′.A3"]
    f1 --- i1(("I₁"))
    f2 --- i2(("I₂"))
    f3 --- i3(("I₃"))

**Spec**

- **Active pair:** ``d.P ⋈ b′.P``
- **Fresh agents:** ``d``
- **Internal edges:** —
- **Interface (|I| = 3):**
  - I₁ = ``b′.A1`` → ``b′.A1`` (preserved)
  - I₂ = ``b′.A2`` → ``b′.A2`` (preserved)
  - I₃ = ``b′.A3`` → ``b′.A3`` (preserved)

.. note::

   The paper renders the preserved ``b′`` ports as a single vertical bar
   (shorthand for "these ports pass through") alongside the ``d`` agent. The
   arity axiom fixes ``|I(n)| = 3``; whether ``d.P`` is a separate fourth free
   port or is identified with the box-opening connection (i.e. one of the three
   interface ports) is not unambiguous in the figure. Confirm against the box
   structure in the translation section before implementing.

Rule 16: ε ⋈ α (erasure, schema)
--------------------------------

``(ε, α, n)`` — rule *scheme*: the eraser ``ε`` interacting with any agent
``α`` of arity ``n`` erases ``α`` and spawns ``n`` fresh ``ε`` agents, one per
auxiliary port, which erase the net connected to those ports.

**LHS**

.. code-block:: mermaid

  flowchart LR
    subgraph eps["ε"]
      epsP(["ε.P"])
    end
    subgraph al["α"]
      alP(["α.P"])
      alA1["α.A1"]
      alAdots["⋯"]
      alAn["α.An"]
    end
    epsP === alP
    alA1 --- i1(("I₁"))
    alAn --- in(("Iₙ"))

**RHS**

.. code-block:: mermaid

  flowchart LR
    subgraph e1["ε₁"]
      e1P(["ε₁.P"])
    end
    edots["⋯"]
    subgraph en["εₙ"]
      enP(["εₙ.P"])
    end
    e1P --- i1(("I₁"))
    enP --- in(("Iₙ"))

**Spec**

- **Active pair:** ``ε.P ⋈ α.P``
- **Fresh agents:** ``ε₁ … εₙ``
- **Internal edges:** —
- **Interface (|I| = n):**
  - Iⱼ = ``α.Aj`` → ``εⱼ.P`` (for ``j = 1..n``)

.. note::

   When ``Ar(α) = 0`` the RHS is the **empty net**. In particular ``ε ⋈ ε →
   ∅`` marks the end of the erasing process (garbage collection).

Implicit term rules
===================

The following term-level operations are realised by the system but have **no
dedicated interaction rule** — they fall out of the net structure and the rules
above:

- **Variable:** ``x[v/x] = v`` and ``y[v/x] = y`` (``y ≠ x``).
- **Push through application:** ``(t₁ t₂)[v/x] = (t₁[v/x]) (t₂[v/x])`` (the
  left and right projections).
- **Push through substitution:** composing a substitution with an existing one
  re-targets the inner substitution rather than expanding it.

These are the standard explicit-substitution push rules; the interaction rules
are their graph-level realisation.

Strong reduction (φ)
====================

Weak reduction stops short of normal form when free variables remain (the
duplication process cannot complete until all terms are closed). To force
complete reduction, YALE adds a ``φ`` agent (``Ar(φ) = 1``) with the following
rules (paper p. 125):

+-----------------+------------------------------------------------------+
| Rule            | Effect                                               |
+=================+======================================================+
| ``φ α ⇒ α φ``   | pass-through: ``φ`` is pushed past an agent ``α``   |
+-----------------+------------------------------------------------------+
| ``φ φ ⇒ ∅``     | two ``φ`` agents annihilate                          |
+-----------------+------------------------------------------------------+
| ``φ b ⇒ b′ φ``  | ``φ`` converts a substitution into a lifted one      |
+-----------------+------------------------------------------------------+
| ``φ b′ ⇒ b φ``  | ``φ`` converts a lifted substitution back            |
+-----------------+------------------------------------------------------+

These are schemas/shorthands for the strong-reduction extension; the
pass-through rule is parametric in ``α``.

Implementation notes & caveats
==============================

- **Port roles are derived.** The port roles in the *Port roles* section are
  derived from the rules, not stated in the paper; the ``λ`` roles in
  particular disagree with the translation figure in places.
- **Rule 13 (``b ⋈ b′``).** The interface is narrowed to a 2-element
  ambiguity (I₄/I₆ onto ``{b′₂.A3, b.P}``); see the rule's note.
- **Rule 15 (``d ⋈ b′``).** The paper's vertical-bar shorthand leaves the
  exact interface (3 ports per the arity axiom) ambiguous with respect to
  ``d.P``; see the rule's note.
- **System properties.** The system is deadlock-free; there is at most one rule
  per agent pair; every RHS is in normal form; and the arity axiom
  ``Ar(l₁) + Ar(l₂) = |I(n)|`` holds for every rule. Each rule's **interface**
  block is a bijection between the LHS secondary ports (labelled
  ``I₁..I_|I(n)|``, ``l₁``'s first then ``l₂``'s) and the RHS free ports — the
  interface-preservation property — and it holds for every rule.
- **Schemas.** Rules 12 and 16 are schemes parametric in the arity ``n`` of the
  non-``δ``/non-``ε`` agent; instantiate them per agent arity.
