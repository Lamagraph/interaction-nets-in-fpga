.. Interaction Nets in FPGA — documentation index

================================================
Interaction nets based processor in Clash
================================================

This is the documentation entrypoint. The repository is described in
:doc:`../README <../README>` for humans; this tree holds the navigation map
and the design/structure documentation.

Contents
========

.. toctree::
   :maxdepth: 2
   :caption: Documentation

   architecture
   contributing

Navigation
==========

- :doc:`architecture` — the project structure map (where each kind of thing
  lives). Read this before exploring the code.
- :doc:`contributing` — the contribution guidelines (integration branch, commit
  format, merge strategy).
- ``README.md`` — project landing page (interaction-net formalism, required
  tools, editor setup).
- Per-package READMEs — one-line summary and build/test instructions for
  ``lamagraph-core`` and ``lamagraph-compiler``.
- Haddock — public API of each package (built by the docs-build CI job).

The agent skills under ``.opencode/skills/`` resolve project-specific facts via
the docs (and the skills that own them); they are thin pointers and never
re-describe this documentation.
