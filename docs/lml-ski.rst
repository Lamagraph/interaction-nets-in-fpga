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

Differential testing
====================

The test suite (``lamagraph-ski/test/Spec.hs``) exercises the pipeline with
three complementary layers, all built on ``translateAndRun``:

- **Harness** — ``assertEqualBoth`` runs a core program through both evaluators
  and asserts the agreed value equals an expected one. A ``StepOverflowReached``
  or any ``SkiError`` is a loud failure (the reduction is bounded by
  ``defaultMaxSteps``, so a test never hangs).
- **Property** — a hedgehog property instantiates a fixed set of minimal-subset
  LML programs (addition, multiplication, ``let``, lambda, higher-order
  application; no recursion) with random bounded inputs (``0..8``) and asserts
  LML == SKI for every (program, input).
- **Goldens** — a small set of (program, input) triples captures the expected
  canonical ``Value'`` in golden files (``test/golden/``); a change in either
  interpreter's result flips the golden.

Step-bound policy: every reduction is bounded by ``defaultMaxSteps``; an
overflow is a test failure, not a hang. The input range is kept small so each
reduction stays tractable on the worklist reducer.

Translation
===========

``Lamagraph.Ski.Translate.translateCore`` expands the core term to a named-``λ``
term — inlining non-recursive ``let`` bindings and swapping ``+ - *``, the
boolean constructors (``true``/``false``), and the comparison operators
(``< > <= >=``) for their Church combinators — then converts it to SKI via the
standard lambda→SKI abstraction rule (``Lamagraph.Ski.Lambda.toSki``). Every
binder is given a fresh name so composing terms never introduces a name clash.
An if-then-else (a ``match`` on a boolean) is translated to a direct Church-bool
selection. Constructs outside the supported subset (integer literals, ``+ - *``,
booleans, comparisons, ``let``, lambda, application) are reported as
``UnsupportedConstruct``.

The abstraction rule carries one essential efficiency shortcut: ``λv. t`` is
translated to ``K t`` when ``v`` is not free in ``t``. Without it the ``S`` rule
re-abstracts every subterm and the term blows up exponentially in the nesting
depth — a comparison combinator grew to ~290k SKI nodes and its reduction to
~580k steps; with the shortcut the same combinator is ~230 nodes and reduces in
~1k steps. The shortcut keeps the translation linear in the term size.

Top-level bindings are inlined by substitution in **reverse** source order: a
later binding's right-hand side may reference an earlier binding, so the earlier
binding must still be a live variable when the later one is inlined (otherwise
the reference would be left free and the result wrong).

Encodings
=========

Integers are encoded as **Church naturals** (``Lamagraph.Ski.Church``): ``+`` is
Church addition, ``*`` is Church multiplication, and ``-`` is saturating Church
subtraction (``max 0 (a - b)``). Booleans use the standard Church encoding
(``true = λt f. t``, ``false = λt f. f``), with ``not``/``and``/``or`` as Church
bool combinators. The comparisons ``< > <= >=`` are Church combinators over
Church numerals that return a Church bool, built from saturating subtraction,
``isZero``, and ``not`` (e.g. ``a <= b`` is ``isZero (sub a b)`` and ``a < b``
is ``not (isZero (sub b a))``). Tuples use the Church pair
(``pair x y = λf. f x y``, with ``fst p = p (λx y. x)`` and
``snd p = p (λx y. y)``); an n-tuple is the right-associated nesting
``pair e1 (pair e2 (... (pair e(n-1) en)))``. The wider value set (ADTs via
Scott encoding, char/string) is the scope of the follow-up subtasks.

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

A Church bool is decoded by applying it to two distinct markers (the ``true``
and ``false`` branch markers); whichever marker survives in the normal form
gives the boolean value. A tuple is decoded by walking the nested-pair
structure: at each level the pair is projected with the ``fst``/``snd``
selectors and each element is decoded recursively. The decoder is
**shape-directed**: it is told the expected ``Value'`` shape (taken from the
LML interpreter's result), which supplies the type information the SKI normal
form lacks, and dispatches to the integer, boolean, or tuple decoder
accordingly.

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
