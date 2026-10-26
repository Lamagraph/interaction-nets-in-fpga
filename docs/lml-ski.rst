.. _lml-ski:

==========================
LML to SKI (interaction nets)
==========================

Design of the ``lamagraph-ski`` package: a translator from the LML core language
to the SKI combinator calculus (L1) and a pure interaction-net runner, used to
differentially test the LML interpreter against the interaction-nets-based (SKI)
interpreter. Part of hub #85.

Pipeline
========

A closed LML core program (top-level bindings + a result expression) is
evaluated two ways and the results compared:

1. **LML side** — ``Lamagraph.Compiler.Eval.evalCoreBindsDefEnv`` interprets the
   core program; the resulting runtime value is canonicalized to ``Value'``.
2. **SKI side** — the program is translated to a SKI term, reduced to normal
   form by the pure runner, and decoded back to a ``Value'``.

``Lamagraph.Ski.translateAndRun`` runs both and raises ``Mismatch`` (carrying
both values) if they disagree.

Translation
===========

``Lamagraph.Ski.Translate.translateCore`` expands the core term to a named-``λ``
term — inlining non-recursive ``let`` bindings and swapping ``+ - *`` for their
Church combinators — then converts it to SKI via the standard lambda→SKI
abstraction rule (``Lamagraph.Ski.Lambda.toSki``). Every binder is given a fresh
name so composing terms never introduces a name clash. Constructs outside the
minimal subset (integer literals, ``+ - *``, ``let``, lambda, application) are
reported as ``UnsupportedConstruct``.

Top-level bindings are inlined by substitution in **reverse** source order: a
later binding's right-hand side may reference an earlier binding, so the earlier
binding must still be a live variable when the later one is inlined (otherwise
the reference would be left free and the result wrong).

Encodings
=========

Integers are encoded as **Church naturals** (``Lamagraph.Ski.Church``): ``+`` is
Church addition, ``*`` is Church multiplication, and ``-`` is saturating Church
subtraction (``max 0 (a - b)``). Booleans use the standard Church encoding. The
wider value set (tuples, ADTs via Scott encoding, char/string) is the scope of
the follow-up task.

Extraction (decoding)
=====================

SKI interaction-net normal forms are **not canonical** for combinator
equivalence: equivalent terms reduce to *different* normal forms (e.g. ``add 1
1`` versus the direct Church numeral ``2``). A result therefore cannot be read
off by comparing normal forms. Instead the decoder forces the result to a
canonical unary shape: a Church numeral ``n`` applied to ``(λy. tick y)`` and a
base reduces to a chain of exactly ``n`` ``tick`` nodes, which are counted.
Because the net shares ``tick`` through a ``D`` (duplicator) node, the count
expands ``D`` nodes and counts *logical* occurrences
(``Lamagraph.Ski.Compare.countLogical``), not physical nodes.

Integer (natural) limitation
============================

The minimal subset encodes integers as Church **naturals**, so subtraction is
saturating (``max 0 (a - b)``) and negative results are out of scope. Full
signed-integer support is deferred.

Runner
======

``Lamagraph.Ski.Run.reduceToNormalForm`` reduces a SKI net to normal form (or
returns ``StepOverflow`` when the ``MaxSteps`` budget is exhausted). It is pure
(no IO, no per-step printing), so it is suitable for property tests.

The default driver is a **worklist** reducer
(``Lamagraph.Ski.Worklist.reduceStepsWL``). Rather than rescanning the whole
graph for every rule on every step, it tracks the set of redex pairs and, after
each contraction, only re-examines the local neighbourhood of the contracted
pair, so each step costs O(local degree) instead of O(graph size). A redex pair
is admitted only if it actually matches a rule (a shared principal port with no
matching rule is a *dead* pair and is rejected — admitting it would make the
driver loop forever). As a safety net, whenever the worklist drains a full scan
is performed and normal form is declared only when that scan finds no redex, so
the driver is correct even if the incremental neighbourhood update ever misses a
pair.

The fixed-order reference driver (``reduceStepsFixedOrder``) is kept for
debugging and for the worklist-vs-fixed-order equivalence tests; it tries each
rule in a fixed order against the whole graph and is O(graph size) per step.

.. note::

   The worklist reducer also changes the contraction order, so the step count
   differs from the fixed-order driver (e.g. ``1 + 1`` is 1432 steps versus
   3766). The decoded result is unchanged (the SKI system is confluent), which
   the equivalence tests guard.

Seam for the FPGA core
======================

The comparison target is the SKI *software* interpreter from ``graph-rewriting``
(``INet.Rules`` plus the pure reduction driver in this package). The FPGA core
(``lamagraph-core``) has no SKI rules yet, so it is not the comparison target.
A seam is left for it: the pure ``reduceToNormalForm`` interface can later be
backed by the FPGA core's SKI reduction once those rules exist.
