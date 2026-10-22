# Changelog for `lamagraph-compiler`

All notable changes to this project will be documented in this file.

The format is based on [Keep a Changelog](https://keepachangelog.com/en/1.0.0/),
and this project adheres to the
[Haskell Package Versioning Policy](https://pvp.haskell.org/).

## Unreleased

### Added

- ADT (algebraic data type) support on the core representation level:
  constructor patterns and or-patterns are now desugared to core `Match`
  expressions.
- **Complex or-patterns** are now desugared: an or-pattern such as
  `Op ((Add | Sub), x)` is expanded into separate match cases, one per
  or-pattern alternative.
- **Complex (nested) constructor patterns** are now desugared: patterns such as
  `Node (Leaf a, Leaf b)` are desugared to nested `Match` expressions that bind
  the nested constructors' fields.

### Notes

- The typechecker requires both sides of an or-pattern to bind the same
  variables (existing behavior, unchanged by this work). An or-pattern that
  frees different variables on each side is rejected with
  `VarMustOccurOnBothSidesOfOrPattern`.

## 0.1.0.0 - YYYY-MM-DD
