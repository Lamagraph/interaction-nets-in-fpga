#!/usr/bin/env bash
set -u
# Pre-commit hook: verify each .hs file is fourmolu-formatted.
# Fourmolu 0.18 has no check mode (its -c flag is check-idempotence, which
# does not detect an unformatted file), so the check is: run fourmolu on the
# file and compare its output to the file contents. Uses `stack exec` when
# available so the pinned version (stack.yaml extra-deps) is the one that
# runs; falls back to fourmolu on PATH otherwise.
if [ "$#" -eq 0 ]; then
  exit 0
fi
if command -v stack >/dev/null 2>&1; then
  run_fourmolu() { stack exec -- fourmolu "$1" 2>/dev/null; }
else
  run_fourmolu() { fourmolu "$1" 2>/dev/null; }
fi
status=0
for f in "$@"; do
  out=$(run_fourmolu "$f")
  if [ "$?" -ne 0 ]; then
    echo "fourmolu: failed to parse $f" >&2
    status=1
    continue
  fi
  if [ "$out" != "$(cat "$f")" ]; then
    echo "fourmolu: $f is not formatted (run fourmolu -i $f)" >&2
    status=1
  fi
done
exit $status
