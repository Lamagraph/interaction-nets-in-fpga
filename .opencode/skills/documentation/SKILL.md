---
name: documentation
description: Use when determining which docs to update for a code change. Defines the principles of documentation completeness; the doc systems and where each lives live in the project structure map.
---

# Documentation

The project's documentation systems and where each kind of documentation lives
are in the project structure map in `docs/architecture.rst` (Documentation
section); the completeness rules below define the required doc actions. Read
the map before updating any docs.

## Principles

- Every code change requires a documentation action; a change that needs no
  doc update is the exception, not the rule.
- Document where the system of record for that kind of content lives (API
  reference, project docs) — never in a second copy somewhere else.
- The entry-point document holds navigation and one-line summaries; detailed
  behavior lives in the dedicated docs.

## Completeness verification

A documentation update is complete when:

- [ ] At least one doc file was created or updated for the change, per the
      completeness rules above and the doc systems in `docs/architecture.rst`.
- [ ] New or changed public APIs are documented in the system that covers them.
- [ ] Detailed behavior is not re-added to the entry-point document.
- [ ] Doc comments and docstrings follow the conventions of their
      documentation system.

For navigation conventions and how to improve them, see the
`project-navigation` skill. To build and check the docs, see the
`quality-gates` skill — the docs build is part of the gate.
