#!/usr/bin/env node
// check-qa-convergence.js — CommonJS
// Read-only mechanical gate deciding whether a QA NO-GO route-back to
// /roster-implement is permitted (spec: specs/qa-loop-bounding.md,
// FR-260..286). Mirrors scripts/check-review-convergence.js's exit contract
// and orchestration shape, but is a fresh, minimal script: QA has no
// findings ratchet, no strikes, no red/green verification — only a physical
// round counter, a qualifying-round cap, and a bookkeeping completeness
// check (C-9).
//
// Usage: node scripts/check-qa-convergence.js <qa-state.json path> [--max-rounds N]
//
// Exit contract:
//   0 = pass (no violations)
//   1 = violation — top-level `cause` is "qa-round-cap"
//   2 = degraded input (absent/malformed/schema-invalid qa-state.json, an
//       unknown flag, or a bad --max-rounds value) — fail-closed
//   3 = process-incomplete-only (missing rounds_audit entry for the current
//       round) — repair the draft and re-gate, do NOT route; top-level
//       `cause` is never "process-incomplete"
//
// Read-only guarantee (FR-279): this script MUST NOT modify any repo file
// or `.git` state — it only reads the state path and stdlib schema.
//
// Legacy handling (FR-280/EC-3, V2-2): a qa-state.json lacking
// `qa_no_go_round` is treated as round 0 with a warning — schema/qa-
// state.schema.json deliberately does NOT require the field, so this never
// collides with schema validation's own exit-2 path.
//
// Config-echo contract: the JSON report ALWAYS includes `config:
// {max_rounds}` on every exit code reached past input validation.
"use strict";

const fs = require("fs");
const path = require("path");
const { compile } = require("./lib/review/finding-schema");
const { computeCapViolation, computeMissingAuditViolation, selectCause } = require("./lib/qa/qa-convergence-rules");

const KNOWN_FLAGS = new Set(["--max-rounds"]);
const DEFAULT_MAX_ROUNDS = 5;

let schemaValidator = null;
function getSchemaValidator() {
  if (!schemaValidator) {
    schemaValidator = compile(require("../schema/qa-state.schema.json"));
  }
  return schemaValidator;
}

function parseArgs(argv) {
  const out = { statePath: null, maxRounds: DEFAULT_MAX_ROUNDS, unknownFlag: null };
  for (let i = 0; i < argv.length; i++) {
    const a = argv[i];
    if (a === "--max-rounds") {
      out.maxRounds = parseInt(argv[++i], 10);
    } else if (a.startsWith("--") && !KNOWN_FLAGS.has(a)) {
      out.unknownFlag = a;
    } else if (!out.statePath) {
      out.statePath = a;
    } else {
      out.unknownFlag = a;
    }
  }
  return out;
}

function fail(code, message) {
  if (message) process.stderr.write(`check-qa-convergence: ${message}\n`);
  process.exit(code);
}

// Reads and parses the qa-state.json file. Exits (via fail) on any absent/
// unreadable/malformed/non-object input — never returns in that case.
function readState(statePath, displayPath) {
  if (!fs.existsSync(statePath)) fail(2, `qa-state.json not found: ${displayPath}`);

  let raw;
  try {
    raw = fs.readFileSync(statePath, "utf8");
  } catch (e) {
    fail(2, `cannot read qa-state.json: ${e.message}`);
    return;
  }

  let state;
  try {
    state = JSON.parse(raw);
  } catch (e) {
    fail(2, `qa-state.json is not valid JSON: ${e.message}`);
    return;
  }
  if (!state || typeof state !== "object" || Array.isArray(state)) {
    fail(2, "qa-state.json must be a JSON object");
    return;
  }
  return state;
}

// FR-264/V2-2: structural validation against schema/qa-state.schema.json.
// `qa_no_go_round` is not in `required` — its absence is a procedural
// default (deriveQaNoGoRound), never a schema failure.
function validateSchema(state) {
  const { valid, errors } = getSchemaValidator().validate(state);
  if (!valid) fail(2, `qa-state.json failed schema validation: ${errors.join("; ")}`);
}

// FR-280/EC-3: a legacy state lacking qa_no_go_round is treated as 0 with a
// warning — never exit 2. A present-but-invalid value still fails closed.
function deriveQaNoGoRound(state, warnings) {
  if (!Object.prototype.hasOwnProperty.call(state, "qa_no_go_round")) {
    warnings.push("legacy qa-state.json: qa_no_go_round key absent — treating as round 0");
    return 0;
  }
  const value = state.qa_no_go_round;
  if (typeof value !== "number" || !Number.isFinite(value) || value < 0) {
    fail(2, "qa-state.json field qa_no_go_round must be a non-negative number");
    return;
  }
  return value;
}

// Parses argv, reads qa-state.json, validates its shape. Exits (via fail)
// on any usage or degraded-input error — never returns in that case.
function validateArgsAndState(argv) {
  const args = parseArgs(argv);
  if (args.unknownFlag) fail(2, `unknown flag or extra argument: ${args.unknownFlag}`);
  if (!args.statePath) fail(2, "usage: <qa-state.json path> [--max-rounds N]");
  if (Number.isNaN(args.maxRounds) || args.maxRounds < 1) fail(2, "--max-rounds must be a positive integer");

  const state = readState(path.resolve(process.cwd(), args.statePath), args.statePath);
  validateSchema(state);

  const warnings = [];
  const qaNoGoRound = deriveQaNoGoRound(state, warnings);

  return { args, state, warnings, qaNoGoRound };
}

// Assembles the JSON report. `cause` reflects only design-level violations
// (qa-round-cap) — process-incomplete is never the top-level cause (FR-278).
function buildReport({ args, state, qaNoGoRound, warnings, violations }) {
  return {
    task: typeof state.task === "string" ? state.task : null,
    status: state.status,
    round: state.round,
    cycle: state.cycle,
    qa_no_go_round: qaNoGoRound,
    config: { max_rounds: args.maxRounds },
    cause: selectCause(violations),
    warnings,
    violations,
  };
}

// FR-278 exit precedence past input validation: 1 (qa-round-cap) > 3
// (process-incomplete-only) > 0 (pass). Exit 2 (degraded input) is handled
// earlier via fail() and never reaches this function.
function decideExit(report) {
  const hasCapViolation = report.violations.some((v) => v.cause === "qa-round-cap");
  if (hasCapViolation) return 1;
  const hasProcessIncomplete = report.violations.some((v) => v.cause === "process-incomplete");
  if (hasProcessIncomplete) return 3;
  return 0;
}

function main() {
  const { args, state, warnings, qaNoGoRound } = validateArgsAndState(process.argv.slice(2));

  const violations = [];
  const capViolation = computeCapViolation(qaNoGoRound, args.maxRounds);
  if (capViolation) violations.push(capViolation);
  const auditViolation = computeMissingAuditViolation(state, state.round);
  if (auditViolation) violations.push(auditViolation);

  const report = buildReport({ args, state, qaNoGoRound, warnings, violations });

  process.stdout.write(JSON.stringify(report, null, 2) + "\n");
  process.exit(decideExit(report));
}

if (require.main === module) {
  main();
}

module.exports = { parseArgs, decideExit, buildReport };
