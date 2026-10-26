// scripts/lib/qa/qa-convergence-rules.js — CommonJS
// Pure, dependency-free rule functions for the QA-loop-bounding mechanics
// (spec: specs/qa-loop-bounding.md, FR-260..286). Mirrors the shape of
// scripts/lib/review/review-convergence-rules.js but is a fresh, minimal
// module — no reuse of review's strike/streak machinery, which QA has no
// analogue for (C-9: QA only needs a round-cap + a process-incomplete
// bookkeeping check).
//
// Module boundary: this file owns the qualifying-cause classification, the
// round-cap violation, and the missing-audit-entry violation. CLI parsing,
// input validation, and orchestration live in scripts/check-qa-convergence.js.
"use strict";

// [NEEDS-HUMAN] C-4 (spec Open Point 1): code-intel-violation and
// tui-failure qualifying is an INTERPRETATION of OQ3 — the binding decision
// text names only "quality-gate/test failures" (step 2/3) and does not
// explicitly name step 3.5 exit 1 or step 4 TUI failures. This spec proceeds
// with them as qualifying (fail-safe direction: more causes qualify -> the
// pipeline escalates to a human sooner, never later). If the human overrules
// this interpretation, narrowing is a one-line flip: delete the two entries
// below and flip the fixture that currently asserts they qualify
// (scripts/check-qa-convergence.test.js).
const QUALIFYING_CAUSES = new Set([
  "gate-failure",
  "spec-check-failure",
  "code-intel-violation",
  "tui-failure",
]);

// FR-268/EC-4: a rounds_audit entry qualifies iff at least one of its
// recorded causes is in the qualifying set. cross-runtime-discrepancy,
// code-intel-malformed, and environment-failure are deliberately excluded:
// an attempt that could not exercise the product is inconclusive, not a
// product-quality strike. A mixed entry still qualifies through its product
// failure cause.
function isQualifying(entry) {
  if (!entry || !Array.isArray(entry.causes)) return false;
  return entry.causes.some((cause) => QUALIFYING_CAUSES.has(cause));
}

// FR-271/EC-5: inclusive comparison — qa_no_go_round == max_qa_rounds
// already escalates (a cap of 5 means 5 qualifying NO-GOs escalate, not 6).
function computeCapViolation(qaNoGoRound, maxRounds) {
  if (qaNoGoRound >= maxRounds) {
    return {
      type: "round-cap",
      cause: "qa-round-cap",
      detail: `qa_no_go_round (${qaNoGoRound}) >= max-rounds (${maxRounds})`,
    };
  }
  return null;
}

// FR-278 exit-3 case: the current round's rounds_audit entry must be
// present. This is a FRESH minimal check for QA — deliberately NOT reusing
// review's computeMissingAuditViolation, which requires review-shaped fields
// (reviewed_sha, fix_sha, specialists_run) that a QA state file never has.
function computeMissingAuditViolation(state, round) {
  const roundsAudit = Array.isArray(state.rounds_audit) ? state.rounds_audit : [];
  const entry = roundsAudit.find((e) => e && e.round === round);
  if (!entry) {
    return {
      type: "missing-round-audit",
      cause: "process-incomplete",
      detail: `no rounds_audit entry for round ${round}`,
    };
  }
  return null;
}

// FR-278: the only design-level cause QA has is qa-round-cap.
// process-incomplete is NEVER the top-level cause (mirrors review's
// contract: it is a gate-internal bookkeeping classification, repaired
// pre-persist or surfaced directly — never treated as a design violation).
function selectCause(violations) {
  const causes = new Set(violations.map((v) => v.cause));
  if (causes.has("qa-round-cap")) return "qa-round-cap";
  return null;
}

module.exports = {
  QUALIFYING_CAUSES,
  isQualifying,
  computeCapViolation,
  computeMissingAuditViolation,
  selectCause,
};
