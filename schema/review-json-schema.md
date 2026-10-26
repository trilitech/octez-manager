---
name: review-json-schema
type: schema-doc
---

# Schema — `briefs/<task>-review.json`

Canonical, machine-checkable-adjacent documentation of the full review-verdict shape written by
`skills/pipeline/roster-review.md` and read by `scripts/check-review-convergence.js`. This file
moved the full shape out of the skill (FR-111, `specs/review-skill-slimming.md`) — the skill now
only carries a minimal summary + a pointer here.

The finding object itself (used inside `findings[]` and `cross_runtime_findings[]`) is defined
once, machine-readably, in `schema/review-finding.schema.json` — this document does not repeat
that shape; it documents the round-tracking fields a finding gains **after** roster-review
ingests it, and the envelope around the findings arrays.

## Top-level shape

```json
{
  "task": "string — task slug",
  "date": "string — ISO-8601",
  "status": "GO | NO-GO",
  "auto_fixes_applied": [
    { "path": "string", "line": "int", "category": "string", "description": "string" }
  ],
  "findings": ["<finding + round-tracking fields, see below>"],
  "cross_runtime_findings": ["<finding, augment-only, never rewritten>"],
  "summary": { "critical": 0, "high": 0, "medium": 0, "low": 0, "auto_fixed": 0 },
  "no_go_reason": { "type": null, "cause": null, "failed_acs": [] },
  "no_go_round": 0,
  "round": 1,
  "cycle": 1,
  "rounds_audit": ["<see below>"],
  "cross_runtime": { "<runtime>": { "status": "healthy|degraded|skipped-human", "reason": null, "config_digest": "string", "round": 1 } },
  "streak_override": null,
  "mode": "express-lite|express|fast|full",
  "assurance": "local|independent|null — required as local for Express Lite",
  "escalation_needed": false,
  "escalation_reason": null,
  "normalized_by": "string — normalizer_version, when scripts/review-normalize.js ran (FR-108)"
}
```

### `no_go_reason.type`

`null | "spec-ac-failure" | "code-plan-failure" | "cross-runtime-finding" | "out-of-scope-change" | "design-not-converging" | "review-integrity-failure"`

`"review-integrity-failure"` (added by `specs/r5-trace-enforcement.md` FR-175, C-1): the
convergence gate's top-level `cause` is `"unattested-invocation"` — a claimed tool invocation
(specialist, scope-gate, normalizer, or cross-runtime pass) has no corresponding trace/journal
line. Routes to re-running the round's claimed tooling for real, **never** to `/roster-spec`
design-not-converging routing, and **never** eligible for the streak override (INV-8).

### `no_go_reason.cause`

Set when `type == "design-not-converging"`: `null | "round-cap" | "unencodable-finding" |
"novel-finding-streak"` — persisted `cause` is never `"process-incomplete"` (that gate-internal
cause is repaired pre-persist, §5.5 B-5).

Set when `type == "review-integrity-failure"` (FR-174/FR-175, `specs/r5-trace-enforcement.md`):
`"unattested-invocation"` — the ONLY cause this `type` carries. This is the one exception to the
otherwise-binding "`cause` only set when `type == design-not-converging`" rule stated above: the
two `type`/`cause` pairings are mutually exclusive and each is exhaustive for its own `type`.

### `escalation_reason`

`null | "new-public-api" | "implicit-design-decision" | "spec-update-needed" | "behaviour-change"`

### Legacy / omission rules

- `cross_runtime_findings`: omit the key entirely if no second runtime ran this cycle.
- `round` / `cycle` / `rounds_audit` / `cross_runtime`: absent entirely on a legacy task (predates
  `review-fanout-convergence`) — never write them mid-cycle if the prior `review.json` lacked
  `round`.
- `streak_override`: `null` unless the human exercised the B-6 streak override this round, then
  `{round, by: "human"}`.

### `cycle` (INV-3/E-5)

Int, incremented at each fresh-cycle initialization (a persisted GO, or an absent prior file) and
retained unchanged across every NO-GO round within that cycle. Distinct from `round`, which resets
to 1 at each fresh cycle — `cycle` is what tells two same-numbered rounds in different cycles apart.
`scripts/lib/review/review-lifecycle.js` is the executable rule, both in-process (`deriveRoundState`,
required directly by `scripts/review-normalize.js` to cross-check a caller-supplied `--round`) and
via its own CLI (`node scripts/lib/review/review-lifecycle.js --prior <path>` → `{round, cycle,
fresh_cycle}`), which `skills/pipeline/roster-review.md` §5.5 shells out to at draft composition.

### Human-skip cross_runtime entry shape (E-10/INV-7)

`{ "status": "skipped-human", "reason": "string", "config_digest": "string", "round": "int", "ts": "ISO-8601", "actor": "human" }`
— the explicit human-skip decision, schema-valid and distinguishable from `degraded`/`healthy`.
`shouldRefuseDegraded` matches only `status: "degraded"` — a skip entry can never arm the breaker.

## Finding round-tracking fields (added on top of `schema/review-finding.schema.json`)

Every entry in `findings[]` — old and new — carries these fields in addition to the base finding
shape:

| Field | Type | Meaning |
|---|---|---|
| `fid` | string | E-3: `fingerprint + "#" + sha8(summary)` — addressable identity for reobservation matching, probable-duplicate records, and gate `checks[]` keying |
| `first_seen_round` | int | the round this finding first appeared |
| `resolved_round` | int \| null | the round it was marked RESOLVED, if any |
| `reopened_from_round` | int \| null | E-4: the `resolved_round` it was reopened FROM, when a re-report regressed a RESOLVED entry |
| `reopened_at_round` | int \| null | E-4: the round the reopen was detected — also the reopened-strike input (§5.5) |
| `check` | string \| null | path to the linked ratchet check (invoked as `node <path>`) |
| `check_encodable` | bool | default `true`; `false` = implementer proposes no deterministic check is possible |
| `red_verified` | bool \| null | set by the gate's full-mode red/green run |
| `pre_fix_sha` | string \| null | HEAD at the round this finding was first recorded as a NO-GO driver |
| `check_blob` | string \| null | `git hash-object` of the check file at last verification |

`cross_runtime_findings[]` entries carry the base finding shape only (no round-tracking fields) —
they are mirrored into `findings` (gaining these fields) only when they drive a NO-GO (FR-015).
They ARE canonicalized (fingerprint + `fid`) and deduped within their own array at normalize time
(INV-5/E-7) — "never rewritten" applies to what happens after that point, not before it.

## `briefs/<task>-gate-report.json` (E-2)

The convergence gate's full stdout JSON, persisted verbatim by roster-review after every gate
invocation (overwritten each round). Consumed read-only by the normalizer's next invocation
(`--gate-report`) to resolve a carried RESOLVED finding's disposition via its `checks[]` array —
`{check, fid, fingerprint, red_verified, check_blob, ...}` per entry, keyed by `(check, fid)` with
the `fingerprint` fallback for legacy entries.

## `rounds_audit[]` entry shape

```json
{
  "round": 2,
  "reviewed_sha": "<HEAD at the PREVIOUS round's verdict emission, or null>",
  "fix_sha": "<HEAD at THIS round's draft composition, or null if the tree is dirty>",
  "fix_sha_reason": "dirty-tree | null",
  "specialists_run": [
    { "name": "reviewer", "selection_reason": "owner reviewer always runs (FR-072)" }
  ],
  "strike": false,
  "trace_schema_version": "1.0"
}
```

`strike` is populated **after** the convergence gate reports it — never computed by
roster-review itself.

`trace_schema_version` (optional; `specs/r5-trace-enforcement.md` FR-167): stamped by
roster-review on every **new** round (required prose-side, absent-safe for legacy entries) —
echoes the `schema_version` written in that round's `briefs/<task>-review-trace.jsonl` lines. Its
presence is one of the two OR'd conditions the gate uses to derive whether the round is
trace-obligated (the other being the trace file's own existence, FR-169) — omitting it cannot
un-obligate a round whose trace file already exists.

## `cross_runtime` entry shape (keyed by runtime name)

```json
{ "status": "healthy|degraded", "reason": "string|null", "config_digest": "string", "round": 1 }
```

`config_digest` hashes the runtime name, `--version` output (10s timeout), and sandbox-mode
flags — never the review timeout or prompt/diff content (FR-093/FR-067). See the human-skip
variant above (E-10/INV-7) for the `skipped-human` status shape.
