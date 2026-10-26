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

``Lamagraph.Ski.Run.reduceToNormalForm`` applies the SKI rule set in a fixed
order, one rule per step, until no rule fires (normal form) or the ``MaxSteps``
budget is exhausted (``StepOverflow``). It is pure (no IO, no per-step
printing), so it is suitable for property tests.

.. note::

   The current reducer is unoptimized: composed terms (e.g. ``2 + 3``) take many
   minutes to reduce, and lambda-with-arithmetic or ``sub`` terms take longer
   still. See the performance finding on task #86 before scaling up the
   differential tests.

Seam for the FPGA core
======================

The comparison target is the SKI *software* interpreter from ``graph-rewriting``
(``INet.Rules`` plus the pure reduction driver in this package). The FPGA core
(``lamagraph-core``) has no SKI rules yet, so it is not the comparison target.
A seam is left for it: the pure ``reduceToNormalForm`` interface can later be
backed by the FPGA core's SKI reduction once those rules exist.
