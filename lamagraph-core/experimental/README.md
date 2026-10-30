# experimental/

Code that is deliberately kept out of the build. Nothing in this directory is part
of the `lamagraph-core` cabal package: it is neither a separate component nor listed
in any `hs-source-dirs`. The compiler never sees it; it is retained only as a
reference for future work.

## `Core/Concrete/TypesTH.hs`

Moved here from `src/Core/Concrete/TypesTH.hs` (git history preserves the original).

What it is: a single Template Haskell function

```haskell
makeAgentsType :: String -> [String] -> Maybe Int -> Q [Dec]
```

generating an agent-label enumeration type (optionally with `UnsignedInt1..n`
payload constructors) as a `DataD` declaration.

Why it was moved out:

- **Unused.** No module in `src/` or `tests/` imports it; nothing depended on it.
- **Generated type is not Clash-compatible.** The `DataD` is built with an empty
  deriving clause list, so the generated type has no `NFDataX` / `ShowX` / `Default`
  (nor `Generic`) instances. Clash requires these for anything that ends up in
  hardware state or ports, so a type produced by `makeAgentsType` could not be used
  in a synthesizable design as-is.

To return it to `src/`:

1. `git mv experimental/Core/Concrete/TypesTH.hs src/Core/Concrete/TypesTH.hs`.
2. Add `Core.Concrete.TypesTH` back to `exposed-modules` in `lamagraph-core.cabal`.
3. Emit deriving clauses in the generated `DataD` (`NFDataX`, `ShowX`, `Generic`,
   and `Default` if the type is used in `topEntity` state), or document the
   post-processing expected of callers.
4. Use it from a real module (or test) and make `cabal build all`, `cabal test`
   and HDL generation pass.

Note: the EDSL agent-set generator
(`INet.Edsl.Agents.declareAgents`, see [`../docs/edsl.md`](../docs/edsl.md))
replaced the idea of extending `makeAgentsType` for label declarations: it
generates the label / kind / arity / singleton skeleton from one declaration
list. It deliberately has no payload-constructor support, so the
`UnsignedInt1..n` payloads of `makeAgentsType` have no counterpart there.
