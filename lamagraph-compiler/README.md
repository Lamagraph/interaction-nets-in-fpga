# lamagraph-compiler

Haskell compiler for the **LML** language. It parses LML source, resolves
names, infers types, desugars to a core language, and lowers the core into
interaction-net encodings (lambda-calculus core and token-passing CBV) that
the `lamagraph-core` processor reduces. It also includes a small evaluator.

## Layout

- `src/Lamagraph/Compiler/` — compiler library (see `docs/architecture.rst`
  for the full module map).
- `app/Main.hs` — the `lamagraph-compiler-exe` executable.
- `test/` — golden-test suite (tasty); fixtures under `test/golden/`.
- `lamagraph-compiler.cabal` — generated from `package.yaml` by hpack.
- `CHANGELOG.md` — release notes (Keep a Changelog + PVP).

Note: the parser sources (`src/Lamagraph/Compiler/Parser.hs`, `Parser.info`,
`Parser/Lexer.hs`) are produced by alex/happy at build time and are gitignored.

## Build & test

Built as part of the Stack project at the repository root:

```shell
stack build lamagraph-compiler   # library + exe
stack test lamagraph-compiler    # golden tests
```

The full gate (format, lint, pedantic build, tests) is the `quality-gates`
skill, run by `.github/workflows/ci.yaml`.
