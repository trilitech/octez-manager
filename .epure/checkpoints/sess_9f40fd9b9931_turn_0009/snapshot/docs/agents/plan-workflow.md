# Plan-First Workflow

Reference guide for planning non-trivial changes. See also the root [AGENTS.md](../../AGENTS.md).

## Plan-First Workflow

For non-trivial changes, write a plan before writing code. Plans serve as the primary review unit — it's cheaper to catch a wrong approach in a plan than in a 500-line diff.

### When a Plan Is Required

- New abstractions or module boundaries
- Changes touching 3+ files (non-local refactoring)
- New TUI pages or major page rewrites
- Changes to scheduler architecture or data flow
- Anything that affects the public CLI interface

### Plan Format

Plans live in `plans/<short-name>.md` and must include:

```markdown
# Plan: <short description>

## Goal
What user-visible problem does this solve?

## Affected Modules
Which files/modules will be created, modified, or deleted?

## Approach
How will you implement this? Include key design decisions.

## Risks
What could go wrong? What assumptions are you making?

## Verification
How will you verify the change works? (tests, manual steps, CI checks)
```

### Workflow

1. **Write the plan** and get approval before writing code
2. **Implement** following the approved plan
3. **Validate** through CI pipeline
4. **Review** the diff against the plan

Small changes (bug fixes, single-file edits, documentation) don't need plans — use good judgment.
