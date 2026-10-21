# tasks/

Planning artifacts for the agent workflow. This directory holds the two plan
files the workflow skills (`planning`, `subtask-loop`, `workflow-management`)
read and update. Each file is the single source of truth for its scope; the
corresponding GitHub comment is a pure mirror.

| File | Scope | Lifecycle |
|---|---|---|
| `global_plan.md` | One **batch** of related tasks (tracked by a `hub` issue). | Created when the batch is set up; updated as each task integrates. Marked tasks get a `[done #N]` suffix. |
| `detailed_plan.md` | One **task** (a `task` issue) on the active feature branch. | Created when the task's feature branch is cut; the plan is decomposed into atomic subtasks `S1`, `S2`, …, each committed individually. |

## Rules

- A task is done only when its subtask commits are on the integration branch
  and the last subtask's commit carries `Closes #<N>` (the task's own issue).
- Subtask identifiers are unique within a task and appear in commit subjects
  as `(<issue>-S<n>): <summary>` (see the contribution guidelines hook in
  `AGENTS.md`).
- The `global_plan.md` is mirrored to the hub issue as a comment whose first
  line is `<!-- global-plan -->`; `detailed_plan.md` is mirrored to the task
  issue as a comment whose first line is `<!-- detailed-plan -->`. The local
  file is the source of truth; the comment is a pure mirror.
- A blocked or deferred subtask stops the task — see the Blocked Work Protocol
  in the `subtask-loop` skill.
