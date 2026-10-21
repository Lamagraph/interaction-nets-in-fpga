.. _architecture:

============
Architecture
============

This page is the **project structure map**: where each kind of thing lives in
the repository. It is the single source of truth for the project structure,
read by the ``project-navigation`` and ``reusing`` skills before exploring the
code. Do not scan the whole tree to learn the layout — start here.

Top level
=========

.. list-table::
   :header-rows: 1

   * - Path
     - What it is
   * - ``lamagraph-core/``
     - Clash library implementing the interaction-net processor (hardware
       description).
   * - ``lamagraph-compiler/``
     - Haskell compiler for the LML language (parser, typechecker, encoders,
       evaluator).
   * - ``docs/``
     - Project documentation (this tree). ``index.rst`` is the entrypoint.
   * - ``tasks/``
     - Planning artifacts: ``global_plan.md`` (per batch), ``detailed_plan.md``
       (per task).
   * - ``.opencode/skills/``
     - Agent skills — thin pointers to the facts in ``docs/`` and the skills
       that own them.
   * - ``.github/workflows/``
     - CI. ``ci.yaml`` orchestrates ``pre-commit.yaml``, ``fourmolu.yaml``,
       ``hlint.yaml``, ``build-and-test.yaml``, ``coverage.yaml``.
   * - ``stack.yaml``
     - Stack project file (resolver + the two packages).

lamagraph-core
==============

Clash-based hardware description of the interaction-net processor.

.. list-table::
   :header-rows: 1

   * - Path
     - Contents
   * - ``src/Core/``
     - Processor core. ``Core.hs`` (top-level), ``CPU.hs``, ``Node.hs``,
       ``Reducer.hs``, ``Loader.hs``, ``Map.hs``.
   * - ``src/Core/Concrete/``
     - Concrete reduction-rule and type definitions. ``Initial.hs``,
       ``ReduceRulesLambda.hs``, ``TypesTH.hs`` (Template Haskell).
   * - ``src/Core/MemoryManager/``
     - Memory manager. ``MemoryManager.hs``, ``ChangesAccumulator.hs``,
       ``NodeChanges.hs``.
   * - ``src/INet/``
     - Interaction-net data structures. ``Net.hs``.
   * - ``tests/``
     - Test suites: ``unittests.hs`` (test-library, HUnit/Hedgehog),
       ``doctests.hs`` (doctest-parallel), ``NodeGenerate.hs`` (helper).
   * - ``bin/``
     - Executables: ``Clash.hs`` (``clash``), ``Clashi.hs`` (``clashi``) —
       Clash simulation/interactive shells.
   * - ``docs/*.svg``
     - Figures referenced from the core (rule diagrams).
   * - ``hie.yaml``
     - HLS/IDE cradle mapping source paths to cabal components.
   * - ``lamagraph-core.cabal``
     - Package descriptor (exposed modules, ``clash``/``clashi`` exes,
       ``doctests`` + ``test-library`` suites).
   * - ``cabal.project``
     - Cabal project file (enables tests for HLS).

lamagraph-compiler
==================

Haskell compiler for the LML language. Source lives under
``src/Lamagraph/Compiler/``.

.. list-table::
   :header-rows: 1

   * - Path
     - Contents
   * - ``src/Lamagraph/Compiler/Syntax/``
     - Concrete syntax (AST) for LML: declarations, expressions, patterns,
       types, literals.
   * - ``src/Lamagraph/Compiler/Parser/``
     - Lexer + parser. ``Parser.hs`` / ``Parser.info`` / ``Parser/Lexer.hs``
       are **generated** by alex/happy and are gitignored (build artifacts).
       Hand-written: ``LexerTypes.hs``, ``LexerUtils.hs``, ``SrcLoc.hs``.
   * - ``src/Lamagraph/Compiler/ModuleResolver/``
     - Name resolution / module resolution.
   * - ``src/Lamagraph/Compiler/Typechecker/``
     - Type inference, unification, typechecker state.
   * - ``src/Lamagraph/Compiler/Core/``
     - Core language and LML-to-Core translation (``LmlToCore.hs``,
       ``MonadDesugar.hs``, ``Pretty.hs``).
   * - ``src/Lamagraph/Compiler/Nets/``
     - Net encodings and processing: ``Encodings/`` (lambda-calculus core,
       token-passing CBV), ``Reduction.hs``, ``Processing.hs``, ``Types.hs``,
       ``Examples.hs``, ``Utils.hs``.
   * - ``src/Lamagraph/Compiler/Eval.hs``
     - Evaluator.
   * - ``src/Lamagraph/Compiler/Driver.hs``
     - Compilation driver (wires the pipeline together).
   * - ``src/Lamagraph/Compiler/PrettyAst.hs``, ``PrettyLml.hs``
     - Pretty-printers.
   * - ``app/Main.hs``
     - Executable entrypoint (``lamagraph-compiler-exe``).
   * - ``test/``
     - Golden-test suite. ``Spec.hs`` is the tasty entrypoint; golden fixtures
       live under ``test/golden/`` (``core``, ``eval``, ``nets``, ``parser``,
       ``typechecker``) and ``test/golden-module/``.
   * - ``lamagraph-compiler.cabal``
     - Package descriptor, generated from ``package.yaml`` by hpack.
   * - ``CHANGELOG.md``
     - User-facing release notes (Keep a Changelog + PVP).

Documentation
=============

.. list-table::
   :header-rows: 1

   * - Artifact
     - Where it lives / how it is checked
   * - Project docs (RST)
     - ``docs/``; ``index.rst`` is the toctree entrypoint.
   * - Haddock (public API)
     - Built by ``stack haddock lamagraph-core lamagraph-compiler`` (docs-build
       CI job).
   * - Per-package READMEs
     - ``lamagraph-core/README.md``, ``lamagraph-compiler/README.md``.
   * - Release notes
     - ``lamagraph-compiler/CHANGELOG.md``.

CI
==

``.github/workflows/ci.yaml`` is the entrypoint; it calls, in order:

.. list-table::
   :header-rows: 1

   * - Workflow
     - Check
   * - ``pre-commit.yaml``
     - ``pre-commit run --all-files`` (tidy-up hooks).
   * - ``fourmolu.yaml``
     - Fourmolu format check (version pinned in the workflow).
   * - ``hlint.yaml``
     - HLint, fail on warning.
   * - ``build-and-test.yaml``
     - ``stack build --pedantic`` + ``stack test`` (ubuntu + macOS).
   * - ``coverage.yaml``
     - HPC coverage, uploaded to Codecov.
