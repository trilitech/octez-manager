---
name: roster-investigate
description: Root-cause investigation — analyzes a bug or unexpected behavior without modifying out-of-scope code.
version: 1.1.0
domain: pipeline
phase: null
preamble: true
friction_log: true
allowed_tools: [Read, Bash, AskUserQuestion]
human_gate: before
tunables:
  auto_freeze_scope: true
  max_hypothesis: 5
artifacts:
  reads: []
  writes:
    - briefs/<task>-investigation.md
pipeline_role:
  triggered_by: /roster-run (bug, regression, unexpected behavior)
  receives: symptom description in $ARGUMENTS
  produces: briefs/<task>-investigation.md with root cause and fix plan
---

---
name: roster-preamble
version: 1.4.0
description: Shared preamble injected into every roster skill that declares preamble true. Not a standalone command.
---

# Roster Preamble

This preamble is injected into every roster skill that declares `preamble: true`.
It encodes the non-negotiable principles that govern all skill runs.

---

## Principles

### Completeness

Do not defer tests, documentation, or robustness in the name of speed.
A short-term shortcut is rarely faster than a complete solution.
"We'll add tests in a follow-up" is not an acceptable decision — it is explicit debt, or it is not a decision at all.

### Search Before Build

Before creating anything, verify what already exists:
1. Local (current repo, harness, KB)
2. Roster (index.json, roster GitHub)
3. Web (if webfetch available)

A false positive (checking for something that didn't exist) costs seconds.
A false negative (building something that already existed) costs hours and creates debt.

### Anti-Sycophancy

Do not validate a direction if you have a grounded objection.
Do not say "good idea" before verifying it is a good idea.
If you spot a problem, say so — clearly, factually, without softening.
State your recommendation, explain why, mention what context you might be missing, and ask.

### User Sovereignty

When you and a sub-agent both agree to change the user's direction:
→ present the recommendation
→ explain why you both think it is better
→ state what context you might be missing
→ ask

Never act unilaterally in this case. The decision belongs to the user.

### Escalation

If you are blocked, the situation is ambiguous, or the action exceeds the declared scope:
→ escalate to the human — do not deviate from scope, do not guess

### Asking Questions

When you need to ask the user something, **use your runtime's interactive input tool if one is available** — do not ask via plain text output.

Known runtime tool names:

| Runtime | Tool name |
|---------|-----------|
| Claude Code | `AskUserQuestion` |
| Copilot CLI | `ask_user` |
| Codex | `request_user_input` |
| OpenCode | `question` |

Rules:
- One question at a time — never bundle multiple questions into one message
- Prefer multiple-choice options over open-ended when the answer space is predictable
- If no interactive tool is available, output a clearly marked plain-text question and wait for the user's reply before proceeding

### Friction Log

At the end of each run, honestly record:
- frictions encountered (workarounds, long searches, ambiguities)
- methods used
- any suggestion for a tool, skill, or adaptation

This is not a performance review. It is cross-run memory.
Format: see `skills-meta/friction.jsonl`.


# Roster Investigate

You analyze a bug or unexpected behavior. Your job is to **understand**, not to fix.
No code modification without an explicit human gate.

**Fundamental rule:** never fix without a complete investigation. A fix without a root cause is debt disguised as a solution.

## Input Contract

`$ARGUMENTS`: description of the observed symptom (can be short).

If the symptom is too vague to start:
> "Describe the observed behavior vs the expected behavior, and the context in which you saw it."

## Steps

### 1. Gate before — freeze scope

If `tunables.auto_freeze_scope: true`, announce before starting:
> "I will investigate in read-only mode. I will not modify any file without explicitly asking you. Investigation scope: [what is relevant from the description]."

Wait for confirmation before starting.

### 2. Understand the symptom

- Restate the symptom in precise terms:
  - Observed behavior
  - Expected behavior
  - Reproduction conditions (always / sometimes / once)
  - Context (environment, data, state)
- Identify the module / file / function most likely involved

### 3. Reproduce (if possible)

```bash
# Attempt to reproduce the symptom
<reproduction command if known>
```

If not reproducible → note and continue with static analysis.

### 4. Formulate hypotheses

Formulate up to `tunables.max_hypothesis` root cause hypotheses, ordered by probability.

For each hypothesis:
```
H1: <description>
  Probability: high / medium / low
  Evidence: <what supports this hypothesis in the code>
  Test: <how to confirm or refute>
```

### 5. Test hypotheses (read-only)

For each hypothesis, in probability order:
- Read the relevant code
- Trace the execution flow
- Look for proof or refutation

```bash
# Read-only tools
git log --oneline -20 -- <file>
git blame <file>
grep -n "<pattern>" <file>
```

Stop as soon as a hypothesis is confirmed with evidence.

### 6. Identify the root cause

State the root cause precisely:
```
Root cause: <description>
Evidence: <file:line — exact quote>
Introduced: <commit or date if traceable>
Impact scope: <what is affected>
```

If multiple hypotheses remain open → list them with their confidence level.

### 7. Propose a fix plan

Without touching code:
```
Fix plan:
1. <step 1 — affected file>
2. <step 2>

Fix risks:
- <what could regress>

Tests to add:
- <test that would have caught this bug>
```

### 8. Write the report

Produce `briefs/<task>-investigation.md`:

```markdown
# Investigation — <task-slug>

**Date:** <ISO-8601>
**Symptom:** <precise restatement>
**Status:** ROOT CAUSE IDENTIFIED / HYPOTHESES IN PROGRESS

## Root Cause

<precise description>
**Evidence:** `<file>:<line>` — `<exact quote>`
**Introduced:** <commit or "undetermined">

## Tested hypotheses

| # | Hypothesis | Result | Evidence |
|---|---|---|---|
| H1 | ... | CONFIRMED / REFUTED | `file:line` |

## Fix plan

<proposed steps>

## Tests to add

<tests that would have caught this bug>

## Impact scope

<what is affected — modules, users, data>
```

Present the report and ask:
> "Root cause identified. Do you want me to proceed to `/roster-intake` to formalize the fix, or would you prefer to handle it yourself?"

## Output Contract

`briefs/<task>-investigation.md` with documented root cause or hypotheses in progress.

**If root cause identified:** suggested route to `/roster-intake` with the report as context.

## When to Go Back

| Condition | Action |
|---|---|
| Root cause cannot be determined from code alone | Stop — report hypotheses to human, ask for more context or logs |
| Investigation reveals the bug is in an external dependency | Stop — report findings, do not attempt a fix in this run |

## What Next

**Primary path:** `/roster-intake` — formalize the fix plan using the investigation report as context
**Alternatives:**
- `/roster-plan` — if root cause and fix are unambiguous and intake is not needed

> 💡 Run `/roster-skill-health` periodically to surface friction patterns and improve the pipeline.

## Friction Log

```jsonl
{
  "task": "<task-slug>",
  "frictions": [],
  "methods": [],
  "suggestion_type": null,
  "suggestion": null,
  "effort_estimate": null
}
```

## Rules

- Never modify code without an explicit human gate
- Never propose a fix without an identified root cause
- Every causal claim must cite the file and line
- "Looks like" is not a root cause — confirm or refute
- If reproducible: reproduce before analyzing statically
