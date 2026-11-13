# Agent scripts

Executable helpers written by the agent during task execution (see `.opencode/core-protocol.md`,
section *Agent Scripts*). These are committed — reusable tooling, not scratch artifacts.

| Script | What it does | When useful |
|---|---|---|
| `check-docs.sh` | Consistency check of the `lamagraph-core` docs set: freshness of `docs/module-graph.generated.md`, no references to the removed `modules.md`, exposed-module count agreement (`lamagraph-core.cabal` library stanza ↔ `docs/architecture.md` §5.1 module map), and every relative Markdown link resolves. Exit code 0 = all checks pass. | After editing anything under `lamagraph-core/docs/` or the cabal module list. |
| `gen-module-graph.sh` | Generates the Mermaid module-dependency graph from `import` lines in `src/` and prints it to stdout. | Regenerating `lamagraph-core/docs/module-graph.generated.md` when project-internal imports change. |
