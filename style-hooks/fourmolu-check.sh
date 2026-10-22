#!/usr/bin/env bash
set -u
# Pre-commit hook: verify each .hs file is fourmolu-formatted.
# Fourmolu 0.18 has no check mode (its -c flag is check-idempotence, which
# does not detect an unformatted file), so the check is: run fourmolu on the
# file and compare its output to the file contents. Resolves fourmolu from
# PATH at the version pinned in stack.yaml extra-deps (installed via ghcup
# locally and in the CI pre-commit job).
if [ "$#" -eq 0 ]; then
  exit 0
fi
if ! command -v fourmolu >/dev/null 2>&1; then
  echo "fourmolu: not found on PATH (expected in .local/bin via ghcup or stack bin dir)" >&2
  exit 1
fi
run_fourmolu() { fourmolu "$1" 2>/dev/null; }
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
