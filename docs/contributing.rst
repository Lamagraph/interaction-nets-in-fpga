.. _contributing:

===============
Contributing
===============

The contribution model: how branches, commits, and merges work. This page is
the single source of truth for the contribution guidelines that the
``git-workflow`` and ``planning`` skills read before doing git operations.

Integration branch
==================

All work targets an **integration branch**, resolved once per session from the
git config key ``lamagraph.integration-branch``:

.. code-block:: bash

   git config --get lamagraph.integration-branch

It is ``main`` in **direct mode**. A developer may set it to a personal,
long-lived branch for **stacked mode**:

.. code-block:: bash

   git config lamagraph.integration-branch <branch>

Never commit directly to the integration branch.

Commit format
=============

Commit subjects follow ``(<issue>-S<n>): <summary>`` — one subtask identifier
per commit, e.g. ``(73-S1): add project facts contract``. Ranges, lists, and
commas are forbidden.

Local style checks
==================

The style gate (fourmolu formatting, hlint) runs locally at commit time via
the pre-commit hooks in ``.pre-commit-config.yaml`` and is pinned to the exact
versions the CI workflows use, so local results match CI bit-for-bit.

Commands (run from the repo root):

.. code-block:: bash

   stack exec -- fourmolu -i <file>       # fix formatting in place
   stack exec -- fourmolu <file> | diff <file> -                        # verify (empty diff = formatted)
   stack exec -- hlint .                  # lint
   pre-commit run --all-files             # every CI style hook in one run

Version pinning: the style tools are pinned in ``stack.yaml``
``extra-deps`` (fourmolu, hlint) and consumed by the hooks through
``stack exec``. Never substitute a differently-versioned local binary — a
different fourmolu version may format differently and produce spurious
diffs. The CI workflows (``.github/workflows/``) remain the single source of
truth for the exact commands and versions; bump the ``extra-deps`` pins
together with the workflow versions when they change.

The fourmolu pre-commit hook is a wrapper (``style-hooks/fourmolu-check.sh``):
fourmolu 0.18 has no check mode (its ``-c`` flag is check-idempotence, which
does not detect an unformatted file), so the wrapper runs fourmolu on each
file and compares its output to the file contents.

Issue closing
=============

The last subtask's commit carries ``Closes #<issue>`` (and ``Closes #<hub>``
for the last task of a batch) as standalone lines.

Merge strategy
==============

Pull requests into ``main`` are merged with **rebase and merge**
(fast-forward) so ``main`` stays linear and per-subtask commits are preserved.
In stacked mode, feature branches are rebased and fast-forward merged into the
integration branch; no pull request is opened until the user asks.
