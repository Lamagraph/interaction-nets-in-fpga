---
name: quality-gates
description: Use before integrating a task. Defines the hard gate that must pass and how to interpret its result; the gate contents (tests, style, docs build) are the checks in the CI workflows.
---

# Quality Gates

The hard gate a task must pass before integration. It has exactly two terminal
states: **PASS** or **BLOCKED**. There is no "pass with exceptions". In stacked
mode the same gate is also run over the whole `main...integration` diff
immediately before the final pull request to `main`.

## What the gate is

The gate is **every** check in `.github/workflows/` — including compilation and
test execution, not only style and docs. The CI workflows are the source of
truth for the exact commands they run (the structure map in
`docs/architecture.rst` lists them); this skill only defines the gate
semantics.

## Procedure

1. Run every check from `.github/workflows/` that applies to the areas the
   change touches. Build: exit 0. Tests: 0 failures, 0 skipped. Style: no
   errors. Docs build: exit 0.
2. Interpret the result:
   - All clean → **PASS**. Proceed to merge (see `git-workflow`).
   - Any failure → **BLOCKED**.

## On BLOCKED

- STOP. Do not merge. Do not mark the task done.
- Do **not** assess whether a failure is pre-existing or unrelated to your
  changes — fix it regardless.
- Do **not** weaken, skip, or comment out failing tests to make the suite green
  (see the Blocked Work Protocol in the `subtask-loop` skill).
- Fix every failure and re-run until **PASS**.
