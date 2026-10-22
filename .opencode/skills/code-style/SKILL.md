---
name: code-style
description: Use before committing to format and lint. Thin pointer to the CI workflows, which hold the exact commands and tool versions.
---

# Code style

What the style checks enforce and the exact commands (including tool versions)
are in the CI workflows (`.github/workflows/`); the `quality-gates` skill names
the gate. Read the CI workflows before running anything.

Do not commit until the check passes clean. If a required tool is missing on
this machine, install it at the version the doc prescribes — a different
version may format differently and produce spurious diffs.
