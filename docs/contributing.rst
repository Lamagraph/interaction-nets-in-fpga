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
