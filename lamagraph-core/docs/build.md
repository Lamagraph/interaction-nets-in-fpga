# lamagraph-core Build, Test and Conventions

Build layout, test tooling, and cross-cutting code conventions for the package. Architecture context lives in [architecture.md](architecture.md); per-module reference lives in Haddock (`cabal haddock`).

---

## 1. Build, Test, Output

| Piece | Detail |
|---|---|
| Library | `lamagraph-core.cabal`, `hs-source-dirs: src`. |
| Executables | `clash` (compile `topEntity` to HDL), `clashi` (interactive). |
| HDL output | `hdl/systemverilog/Core.Core.topEntity/` — `topEntity.sv`, `topEntity_types.sv`, `topEntity_shim.cpp`, `topEntity.sdc`, `clash-manifest.json`. |
| Board projects | `hdl/make-project-primer25k.tcl`, `hdl/make-project-mega138k.tcl`. |
| Test suite | `test-library` (`tests/unittests.hs`): hedgehog/tasty — EDSL-vs-hand rule equivalence, CPU/Loader/Reducer/MemoryManager unit tests. |
| Doctests | `doctests` (`tests/doctests.hs`) for `Core.Map`, `Core.MemoryManager` helpers. |

Notable GHC/Clash options (from `common-options`): `NoImplicitPrelude` (uses `Clash.Prelude`), `-fexpose-all-unfoldings` (Clash needs source), `-fno-worker-wrapper` and `-fno-unbox-*` (avoid unstable names / unsupported optimizations), type-lits solver plugins (`ghc-typelits-natnormalise`, `ghc-typelits-extra`, `ghc-typelits-knownnat`).

---

## 2. Cross-Cutting Conventions

- **Clash idioms.** `Vec` everywhere (fixed sizes bound at the type level, never lists), `errorX` for undefined hardware behavior, `NFDataX`/`ShowX`/`Default` instances, `SNat`/type-level `Nat` arithmetic, `makeLenses` (TH) for record access.
- **Static sizing.** Every vector length in the pipeline (`ReduceRuleResult`, `Interface`, `Changes` map, FSM counters) is derived from the four `INet` parameters, so cycle counts per phase are known at compile time (derived in [architecture.md](architecture.md) §4.1).
- **Address space.** 4-bit addresses, 16 cells — small enough that linear scans (`Core.Map`, bitmap free-search) are acceptable.
- **Deprecated surface.** `Core.Loader.loadInterface`, `loadActivePair`, `writeNewNodes` are superseded by inline logic in `Core.CPU` and kept only for compatibility.
- **Haddock + diagrams.** Rule haddocks embed the SVGs from `docs/`; design rationale for the EDSL layer is in `docs/decisions.md` and `docs/edsl.md`.
- **Documentation tooling.** The import graph in `architecture.md` §5 can be regenerated with `agent-scripts/gen-module-graph.sh` → `docs/module-graph.generated.md`.
