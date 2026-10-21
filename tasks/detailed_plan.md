# Detailed plan — #73 Adopt agent skills

Task: #73 (label `task`). Integration branch: `main` (direct mode).
Feature branch: `gsv/agentify-skill-adoption`.

This task adopts the opencode agent skills (copied from another project) by
authoring the missing *project facts contract* in `AGENTS.md` and aligning the
surrounding infrastructure to what the skills expect. The skills are generic
and resolve all project-specific facts through five hooks named in AGENTS.md;
those hooks did not exist, so every skill was a dangling reference. This task
closes that gap and creates the docs/tasks/CI infrastructure the hooks point
to.

The work is a single, coherent infrastructure unit (it cannot be compiled or
tested in increments — there is no source code), so it is one atomic subtask.

## Subtasks

### S1: Project facts contract + docs + tasks + CI + infra fixes

**Code:** None (documentation/infra only — no `.hs` files). [done]

### S2: Make the local `stack build` work (disable Casa)

**Code:** None in the repo (the config change is local, per the user's choice).
The repo-facing change is a documentation note.

The local `stack build --pedantic` (apt stack 3.7.1) hung deterministically
after "Using package location completions from a lock file", in
`futex_do_wait`, with no child process. Diagnosis: the package-location
completion step routes through the **Casa** package server (`casa: enable:
true` is the default), and that call hangs on this machine — Casa/Hackage are
reachable via curl, so it is stack's own HTTP client, not raw connectivity.

- **Fix (local):** `~/.stack/config.yaml` gains `casa: enable: false`. (The
  `package-index` key was not usable in string form in stack 3.7.1 — it expects
  an object — so the supported `casa.enable` toggle alone suffices.)
- **Docs:** `README.md` "Required tools" gains a **Stack** subsection noting
  the Casa hang and the `casa: enable: false` remedy, so other devs hitting it
  can find it.
- **Verification:** with the fix, `stack build --pedantic` downloads GHC 9.8.4
  and completes (144 actions); `stack test` → lamagraph-core (doctests +
  21/21) and lamagraph-compiler (115/115) all pass.

**Spec:**
- The config change is deliberately local (user config), not in `stack.yaml`,
  so the repo is untouched and the project's stack.yaml stays minimal.
- CI is unaffected (haskell-actions/setup does not use the local config).

### S4: Work around the broken `macos-26-arm64` runner SDK (CI)

**Code:** `.github/workflows/build-and-test.yaml` (one added step, macOS leg
only).

The CI `build-and-test` macOS job fails at the very first link (the `Setup.hs`
build inside `stack build --only-dependencies`) with
`ld: multiple errors: tapi error: malformed file` /
`error: unknown architecture arm64e.x1-macos`, referencing
`MacOSX27.0.sdk`. Root cause: the `macos-26-arm64` runner image
`20261013.0424.1` ships a broken Xcode/CLT 27 SDK whose `.tbd` stubs the
default linker cannot parse. This is a known, tracked upstream regression —
[actions/runner-images#14940](https://github.com/actions/runner-images/issues/14940)
("macos-26-arm64 20261013.0424.1 broke GHC (linking phase)") — and is **not**
caused by this task: it breaks `main` too, on any Haskell project. The Ubuntu
leg is unaffected (`ubuntu-latest` still resolves to `ubuntu-24.04`, which
builds fine).

- **Fix:** on the macOS leg of `build-and-test.yaml`, add a step (gated on
  `startsWith(matrix.os, 'macos')`) that runs
  `sudo xcode-select -s /Library/Developer/CommandLineTools` after checkout and
  before the Haskell setup. Selecting the Command Line Tools toolchain uses a
  working linker/SDK and unblocks GHC linking. This is the same workaround
  clash-compiler (the same Clash toolchain this repo uses) adopted for the
  identical break.
- **Verification:** YAML parses; `pre-commit run --files .github/workflows/build-and-test.yaml`
  passes. The real proof is the CI `build-and-test` macOS job going green (CI
  is the canonical gate — see the build/test note).
- **Revert when:** drop the step once [actions/runner-images#14940]
  is fixed and `macos-latest` resolves to a working image.

### S3: Revert S1's golden-fixture trailing-newline changes

**Code:** `lamagraph-compiler/test/golden*/` — restore the 93 golden fixtures
to their `main` content.

S1's `pre-commit` `end-of-file-fixer` added a trailing newline to 93 golden
fixtures (they were committed without one). But the golden-test generators
(`renderPretty`/`renderUnbounded` in `GoldenCommon.hs`) emit strings with **no
trailing newline**, so `goldenVsString` then failed on all 93 — the bodies are
identical, only the trailing `\n` differs. S1's "passing" pre-commit gate
masked this, because pre-commit was run before the golden tests.

- **Fix:** `git checkout main -- <93 files>` restores the no-trailing-newline
  content. The fixtures must match the generator output byte-for-byte, so the
  newline is removed.
- **Verification:** `stack test` → 0 failures (lamagraph-compiler 115/115,
  lamagraph-core 21/21 + doctests).

**Note for the future:** the `end-of-file-fixer` hook should keep excluding
golden-test fixture files, since their trailing bytes are part of the test
contract. See the follow-up in the Design Notes.

## Design Notes (discovered during implementation)

### end-of-file-fixer vs golden fixtures

The pre-commit `end-of-file-fixer` hook (and the VS Code
`files.insertFinalNewline` setting) must not apply to the compiler's golden-test
fixtures under `lamagraph-compiler/test/golden*/`. Those files are compared
byte-for-byte against generator output (`renderPretty`/`renderUnbounded`) that
ends without a trailing newline. Adding a newline flips every affected test to
FAIL even though the visible content is unchanged. Remaining work: extend the
`end-of-file-fixer` `exclude` in `.pre-commit-config.yaml` to cover
`lamagraph-compiler/test/golden/.*` (and the `.vscode` newline settings if
they are to be enforced on fixtures) so a future `pre-commit run --all-files`
cannot re-break them.

**Files:**
- `AGENTS.md` — add the *Project Facts Contract* section: project-structure,
  contribution-guidelines (integration-branch git config key
  `lamagraph.integration-branch`, commit format `(<issue>-S<n>): summary`,
  rebase-and-merge), quality-checks (the five CI workflows mapped to local
  commands), and documentation-conventions hooks.
- `docs/index.rst` — new documentation entrypoint (toctree).
- `docs/architecture.rst` — new project structure map (the authoritative
  *project structure* hook target).
- `tasks/README.md` — new; explains `global_plan.md` / `detailed_plan.md`.
- `.pre-commit-config.yaml` — fix stale `end-of-file-fixer` excludes
  (`ast|ppr|core|output|nets` paths not present here) to `.*\.svg$`.
- `lamagraph-compiler/README.md` — fill (was empty) with a package summary.
- `.github/workflows/docs-build.yaml` — new haddock docs-build workflow.
- `.github/workflows/ci.yaml` — wire `docs-build` into the CI chain.
- `.opencode/skills/*` — track the 12 skill files (the adopted skills).
- Golden fixtures under `lamagraph-compiler/test/golden*/` — 93 files gain a
  trailing newline (the pre-commit `end-of-file-fixer` hook, now un-excluded
  from these paths, applied it). Pure EOF-newline fixes; no content change.

**Tests:** N/A (no source code). Verified:
- `pre-commit run --all-files` → all hooks pass (exit 0).
- `fourmolu` over all 90 `.hs` files → no diffs (clean).
- `hlint .` → no hints (exit 0).
- `stack build --pedantic` / `stack test` → delegated to CI (see note).

**Docs:** This subtask *is* the documentation. `docs/architecture.rst` is the
structure map; `AGENTS.md` contract is the hook source; per-package README and
CI workflow are aligned to it.

**Spec:**
- The four hooks in `AGENTS.md` are the single source of truth; skills stay
  thin pointers and do not restate them.
- `docs/index.rst` is the toctree entrypoint; `docs/architecture.rst` is the
  structure map read by `project-navigation`/`reusing`.
- The docs-build CI job runs `stack haddock lamagraph-core lamagraph-compiler`
  so the "docs build" quality gate is concrete.

## Build/test note (environment)

Local `stack build` hangs on this machine while contacting the Casa package
server (`casa.stackage.org`) before producing any output — a pre-existing
environment/network issue, not caused by this change (which touches no `.hs`
files). The canonical build/test gate is the CI `build-and-test.yaml` job,
which uses `haskell-actions/setup@v2.12.0` with GHC 9.8.4 and resolves the
lts-23.18 snapshot correctly. The fast gates (pre-commit, fourmolu, hlint)
pass locally.

## Status

- [x] S1: Project facts contract + docs + tasks + CI + infra fixes
- [ ] S4: Work around the broken `macos-26-arm64` runner SDK (CI)
