#!/usr/bin/env bash
# Purpose: Documentation consistency checks for lamagraph-core/docs.
# Usage:   agent-scripts/check-docs.sh   (from anywhere; exit code 0 = all checks pass)
#
# Checks:
#   1. docs/module-graph.generated.md is in sync with the import lines in src/
#      (regenerated via agent-scripts/gen-module-graph.sh and diffed).
#   2. docs/*.md contain no references to files removed from the docs set
#      (currently: modules.md — the per-module reference lives in Haddock).
#   3. Exposed-module count agreement: lamagraph-core.cabal library stanza vs
#      the module map in docs/architecture.md section 5.1, and no stale
#      "N exposed modules" claims anywhere in docs/.
#   4. Every relative Markdown link in docs/*.md resolves to an existing file.

set -u

repo_root=$(cd "$(dirname "$0")/.." && pwd)
cd "$repo_root/lamagraph-core"

fail=0
ok()  { printf 'ok:   %s\n' "$*"; }
bad() { printf 'FAIL: %s\n' "$*"; fail=1; }

# --- 1. generated import graph is fresh -------------------------------------
tmp=$(mktemp)
trap 'rm -f "$tmp"' EXIT
if "$repo_root/agent-scripts/gen-module-graph.sh" src > "$tmp" 2>/dev/null; then
  if diff -u docs/module-graph.generated.md "$tmp" > /dev/null; then
    ok "module-graph.generated.md matches imports in src/"
  else
    bad "docs/module-graph.generated.md is stale — regenerate with: agent-scripts/gen-module-graph.sh src > lamagraph-core/docs/module-graph.generated.md"
  fi
else
  bad "agent-scripts/gen-module-graph.sh failed"
fi

# --- 2. no references to removed docs files ---------------------------------
refs=$(grep -n 'modules\.md' docs/*.md 2>/dev/null || true)
if [ -n "$refs" ]; then
  bad "references to removed docs/modules.md:"
  printf '%s\n' "$refs"
else
  ok "no references to removed modules.md"
fi

# --- 3. exposed-module counts agree -----------------------------------------
cabal_count=$(awk '/^library$/,/^  default-language/' lamagraph-core.cabal | grep -c '^    [A-Z]' || true)
map_count=$(awk '/^### 5\.1 Module map/{f=1;next} f&&/^#/{f=0} f' docs/architecture.md | grep -c '^| `' || true)
if [ "$cabal_count" -eq "$map_count" ]; then
  ok "exposed-module count agrees ($cabal_count: cabal = architecture.md module map)"
else
  bad "module count mismatch: cabal exposed-modules=$cabal_count, architecture.md module map=$map_count"
fi
stale=$(grep -rn 'exposed modules' docs/*.md | grep -v "$cabal_count exposed modules" || true)
if [ -n "$stale" ]; then
  bad "stale exposed-module count claim:"
  printf '%s\n' "$stale"
else
  ok "no stale exposed-module counts in docs/"
fi

# --- 4. relative markdown links resolve -------------------------------------
for md in docs/*.md; do
  dir=$(dirname "$md")
  while IFS= read -r target; do
    case "$target" in
      http://*|https://*|\#*) continue ;;
    esac
    path=${target%%#*}
    [ -z "$path" ] && continue
    if [ ! -e "$dir/$path" ]; then
      bad "broken relative link in $md: $target"
    fi
  done < <(grep -oE '\]\([^)]+\)' "$md" | sed -e 's/^](//' -e 's/)$//')
done
ok "relative link scan finished"

# ----------------------------------------------------------------------------
if [ "$fail" -eq 0 ]; then
  printf 'check-docs: all checks passed\n'
else
  printf 'check-docs: FAILURES FOUND\n'
fi
exit "$fail"
