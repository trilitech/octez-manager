---
name: qa-state-schema
type: schema-doc
---

# Schema — `briefs/<task>-qa-state.json`

Canonical documentation of the QA round-state artifact written by
`skills/pipeline/roster-qa.md` and read by `scripts/check-qa-convergence.js`
(spec: `specs/qa-loop-bounding.md`, FR-260..286). Analogous in spirit to
`schema/review-json-schema.md`, but a distinct, single-writer artifact — the
QA phase owns it exclusively; roster-review and `scripts/check-review-convergence.js`
MUST NOT read or write it, and roster-qa MUST NOT add fields to
`briefs/<task>-review.json` (FR-263).

The markdown report `briefs/<task>-qa.md` keeps its exact status-line contract
(`**Status:** GO ✅` / `**Status:** NO-GO ❌`, ship-gate hook grep, FR-260) —
this JSON file is a sibling, not a replacement, and survives the report's
per-run overwrite.

## Top-level shape

```json
{
  "task": "string — task slug",
  "date": "string — ISO-8601",
  "status": "GO | NO-GO",
  "round": 1,
  "cycle": 1,
  "qa_no_go_round": 0,
  "rounds_audit": ["<see below>"],
  "cross_runtime": {},
  "max_qa_rounds_at_emission": 5,
  "escalation": "null | \"qa-not-converging\""
}
```

### Field reference

| Field | Type | Meaning |
|---|---|---|
| `task` | string | task slug |
| `date` | string | ISO-8601 timestamp of this verdict emission |
| `status` | `"GO" \| "NO-GO"` | this verdict |
| `round` | int | physical counter, +1 on every QA verdict emission (two-event lifecycle, FR-262) |
| `cycle` | int | incremented at each fresh-cycle initialization (persisted GO, or absent prior); retained across NO-GO rounds within the cycle |
| `qa_no_go_round` | int | qualifying-only counter — reset to 0 on GO, +1 only on a qualifying NO-GO (FR-267); compared against `max_qa_rounds`. **Deliberately NOT in `required`** (see Required fields, below) |
| `rounds_audit` | array | append-only within a cycle, one entry per verdict emission (FR-265); see shape below |
| `cross_runtime` | object | inert carry-forward from `scripts/lib/review/review-lifecycle.js`'s `deriveRoundState` (C-2) — `{}` on a fresh cycle, otherwise carried verbatim. QA never reads or interprets this field; it exists only because the lifecycle helper's return shape includes it |
| `max_qa_rounds_at_emission` | int | optional — the tunable's value at this verdict's emission, for audit trail readability |
| `escalation` | `null \| "qa-not-converging"` | set when the cap was hit at this verdict (FR-273) |

### `rounds_audit[]` entry shape

```json
{
  "round": 2,
  "date": "2026-07-14T00:00:00Z",
  "verdict": "GO | NO-GO",
  "causes": ["gate-failure"],
  "qualifying": true
}
```

`causes` is drawn from: `gate-failure` (step 2 quality-gate product failure),
`environment-failure` (step 2 could not exercise the product because a prerequisite or setup
failed),
`spec-check-failure` (step 3 spec runnable-check FAIL), `code-intel-violation`
(step 3.5 exit 1), `code-intel-malformed` (step 3.5 exit 2), `tui-failure`
(step 4), `cross-runtime-discrepancy` (step 4.5). A round with mixed causes
records all of them; `qualifying` is true iff at least one recorded cause is
in the qualifying set (EC-4) — see `scripts/lib/qa/qa-convergence-rules.js`'s
`QUALIFYING_CAUSES`.

## Required fields (JSON Schema, `schema/qa-state.schema.json`)

`required` = `[status, round, cycle, rounds_audit]` **only**. `qa_no_go_round`
is deliberately NOT required: a state file that predates this mechanism (or a
hand-repaired draft) lacking it MUST be treated procedurally as `0` with a
warning by the gate (FR-280/EC-3), mirroring `check-review-convergence.js`'s
handling of a legacy `no_go_round`-less `review.json`
(`scripts/check-review-convergence.js:183-191`). Requiring the field in the
schema would make that legacy case fail closed at exit 2 instead of the
intended warn-and-default-0 — the schema's `required` list and the gate's
procedural default must not collide (V2-2).

## Single-writer invariant (FR-263)

- QA phase (`skills/pipeline/roster-qa.md`) is the only writer of
  `briefs/<task>-qa-state.json`.
- `scripts/check-review-convergence.js` and `scripts/lib/review/*` MUST NOT
  read or write this file.
- roster-review MUST NOT add QA-round fields to `briefs/<task>-review.json`.

## Reuse note (C-2)

`round`/`cycle`/fresh-cycle detection is derived by shelling out to
`node scripts/lib/review/review-lifecycle.js --prior briefs/<task>-qa-state.json`
— the lifecycle helper's `deriveRoundState` keys only on `status`/`round`/
`cycle`/`rounds_audit`/`cross_runtime`, all of which this shape provides. The
`cross_runtime` field the helper returns is carried forward verbatim into
this state file and is otherwise unused by QA.
