# AGENTS.md

This file is a short TOC of the agent workflow. The project itself is
described in `README.md`, which is landing for humans and the entrypoint for navigating the
repository for all.

## Start here

1. Load the `workflow-management` skill
   (`.opencode/skills/workflow-management/SKILL.md`) first, before doing
   anything else.
2. Read `README.md` for project navigation.
3. Before exploring code, load the `project-navigation` skill
   (`.opencode/skills/project-navigation/SKILL.md`): navigate via
   `docs/index.rst` and `docs/architecture.rst`, not by scanning the whole
   repository.

## Main Principles

* This file is a short entry point for fast cold errors-free start.
* Only one source of truth. No duplicates. Each thing (in both code and documentation) described exactly once. Use generalization (especially for code), cross-references, links, other similar techniques to avoid duplicates and reuse staff.
* Source-of-truth hierarchy: code, scripts, CI configs > docs. If a fact can
  be extracted from code or scripts (e.g., CI configs), it is not duplicated
  in docs. Docs hold only what cannot be unambiguously reconstructed from
  code: design decisions, non-trivial constraints. Skills stay thin pointers
  to docs, code, or CI — they never re-describe them.
* Tools, not instructions. If you can do something with existing tool --- do it. No thinking, no long instructions, no manual analysis. You want to analyze code coverage? Just run coverage tool and analyze report. No workaround for regular tasks. If there is a tool for regular task it must be installed and configured appropriately.
* Never stay silent about problems. Any problem discovered or detected must be addressed. If it is in current scope or the fix is easy (e.g. a misprint), fix it — even if pre-existing. If it is significantly out of scope, report it to the user and propose creating an issue.
* Always learn, never forget — encode patterns before session ends

## Skills

### Domain

| Skill | When to use |
|---|---|
| `.opencode/skills/project-navigation` | Starting navigation before exploring code |
| `.opencode/skills/run-tests` | Running the test suite |
| `.opencode/skills/code-style` | Formatting / linting before commit |

### Workflow / process

| Skill | When to use |
|---|---|
| `.opencode/skills/workflow-management` | Driving the overall task loop |
| `.opencode/skills/planning` | Global plans, atomic subtasks, task authoring |
| `.opencode/skills/subtask-loop` | Executing one atomic subtask |
| `.opencode/skills/git-workflow` | Branching, commits, merging |
| `.opencode/skills/reusing` | Finding existing code/docs to reuse, not duplicate |
| `.opencode/skills/user-guidance-transfer` | Recording user guidance verbatim |
| `.opencode/skills/documentation` | Mapping code changes to doc updates |
| `.opencode/skills/quality-gates` | The pre-merge gate that must pass |
| `.opencode/skills/code-review` | Whole-repo review before merge |

## Project Facts Contract

The workflow and domain skills above are generic; they resolve project-specific
facts through the hooks named in this section. Each hook below is the single
source of truth for the fact it names. Skills never re-state these facts —
they read them here (and in the docs/CI it points to).

### Project structure hook

The directory map of where things live. See `docs/architecture.rst` for the
authoritative structure map (it is the *project structure* map the
`project-navigation` and `reusing` skills read). Summary:

- `lamagraph-core/` — Clash library implementing the interaction-net processor
  (hardware description). Source: `src/Core/`, `src/INet/`. Tests: `tests/`.
  Binaries: `bin/` (`clash`, `clashi`). Docs figures: `docs/*.svg`.
- `lamagraph-compiler/` — Haskell compiler for the LML language
  (parser, typechecker, encoders, evaluator). Source: `src/Lamagraph/Compiler/`.
  Executable: `app/`. Tests: `test/` (golden tests under `test/golden/`).
  Generated parser sources (`src/Lamagraph/Compiler/Parser.hs`,
  `Parser.info`, `Parser/Lexer.hs`) are produced by alex/happy and are
  gitignored — they are build artifacts, not first-party source.
- `docs/` — project documentation (RST). `docs/index.rst` is the navigation
  entrypoint; `docs/architecture.rst` is the structure map.
- `tasks/` — planning artifacts: `global_plan.md` (per batch) and
  `detailed_plan.md` (per task). See `tasks/README.md`.
- `.opencode/skills/` — the agent skills (thin pointers to the hooks above).
- `.github/workflows/` — CI. The `ci.yaml` workflow orchestrates the reusable
  workflows: `pre-commit.yaml`, `fourmolu.yaml`, `hlint.yaml`,
  `build-and-test.yaml`, `coverage.yaml`.

### Contribution guidelines hook

- **Integration branch**: resolved once per session from the git config key
  `lamagraph.integration-branch` (see the `git-workflow` skill for the exact
  command). Default is `main` (direct mode). A developer may set it to a
  personal long-lived branch for stacked mode:
  `git config lamagraph.integration-branch <branch>`.
- **Commit format**: `(<issue>-S<n>): <summary>` — one subtask identifier per
  commit, e.g. `(73-S1): add project facts contract`. Ranges, lists, and
  commas are forbidden.
- **Issue closing**: the last subtask's commit carries `Closes #<issue>` (and
  `Closes #<hub>` for the last task of a batch) as standalone lines.
- **Merge strategy**: pull requests into `main` are merged with **rebase and
  merge** (fast-forward) so `main` stays linear and per-subtask commits are
  preserved. In stacked mode, feature branches are rebased and fast-forward
  merged into the integration branch; no PR is opened until the user asks.

### Quality checks hook

The gate a task must pass before integration. It is every check in
`.github/workflows/` (see the `quality-gates` skill for the gate semantics and
the `run-tests`/`code-style` skills for process). Local equivalents:

- **pre-commit**: `pre-commit run --all-files` (tidy-up hooks; see
  `.pre-commit-config.yaml`).
- **fourmolu**: `fourmolu` over all `*.hs` files (version pinned in
  `.github/workflows/fourmolu.yaml`; run without `-i` to check, with `-i` to
  rewrite). The config is `fourmolu.yaml`.
- **hlint**: `hlint` over the repo, fail on warning (as in
  `.github/workflows/hlint.yaml`).
- **build**: `stack build --pedantic` (as in `build-and-test.yaml`).
- **test**: `stack test` (0 failures, 0 skipped).
- **docs build**: haddock for both packages, `stack haddock lamagraph-core
  lamagraph-compiler` (see the docs-build workflow in
  `.github/workflows/`). Exit 0.

### Documentation conventions hook

- `README.md` — the navigation entrypoint. Holds one-line summaries and links;
  detailed behavior lives in dedicated docs, never re-added here.
- `docs/` (RST) — project documentation. `docs/index.rst` is the toctree
  entrypoint; `docs/architecture.rst` is the structure map. Design decisions
  and non-trivial constraints live here.
- `lamagraph-core/README.md`, `lamagraph-compiler/README.md` — per-package
  summaries (what the package is, how to build/test it).
- Haddock — public API of each package (the `docs build` gate covers it).
- `lamagraph-compiler/CHANGELOG.md` — user-facing release notes for the
  compiler package (Keep a Changelog + PVP format).
