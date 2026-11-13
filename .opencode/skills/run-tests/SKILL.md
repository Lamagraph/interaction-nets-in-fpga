---
name: run-tests
description: Use when running the test suite. Thin pointer to the CI workflows and the quality-gates skill, which hold the exact commands, plus process rules for running and interpreting results.
---

# Run tests

What the test pipeline is and where the exact commands live are in the CI
workflows (`.github/workflows/`; the structure map in `docs/architecture.rst`
lists them) and the `quality-gates` skill. Read them before running anything.

## Running a subset of tests

The suite is [tasty](https://hackage.haskell.org/package/tasty). `stack test`
runs everything; to run a subset, forward a `-p` pattern to the test
executable with `--ta` (test arguments), targeting the specific suite:

```
stack test lamagraph-ski:lamagraph-ski-test --ta '-p "<pattern>"'
```

- `<pattern>` is matched against the full dot-separated test path
  `<pkg>.<group>.<test>`, e.g. `lamagraph-ski.yale-reduce.2 + 3`.
- **It is a substring match, not a regex.** A pattern made only of
  `[A-Za-z0-9._- ]` is a literal substring; `.` is a literal dot, so
  `can.nicalize` matches nothing.
- Any other character (`+ = < > : ( ) | " $ /` …) switches the pattern to an
  **awk expression** over the path fields: `$0` = whole name, `$1` = outermost
  group, …, `$NF` = leaf; `NF` = field count. `~ /pat/` is also substring.
  Quote the pattern in double quotes inside `--ta` when it has spaces.
  Examples:
  - `-p 'non-recursive complex'` — the whole complex group.
  - `-p '$3 ~ /max 3 5/'` — one test by its leaf name.
  - `-p '$2 == "booleans and comparisons"'` — a group whose name has spaces.
- Add `-l` (inside `--ta`) to **list** matches without running them:
  `--ta '-p "<pattern>" -l'`. Check a pattern this way first, so you do not
  accidentally launch a known-slow or hanging test.

Fastest iteration (no stack overhead): run the built binary directly.

```
.stack-work/dist/*/ghc-*/build/lamagraph-ski-test/lamagraph-ski-test -p '<pattern>'
```

## Rules

- Run exactly what the `quality-gates` skill prescribes for the areas you
  changed; when in doubt, run the full suite.
- Success is 0 failures and 0 skipped. A suite that "passes" with skips or
  weakened assertions does not count.
- Never weaken, skip, or comment out failing tests to make the suite green
  (see the Blocked Work Protocol in the `subtask-loop` skill).
- If a required tool is missing on this machine, install it at the version the
  CI workflows prescribe — do not substitute a different runner.
