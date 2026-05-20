# Rewards config: remove dead and cosmetic fields

**Date:** 2026-04-28
**Owner:** valentin

## Problem

The rewards Config tab exposes 12 fields that are stored, serialized, and rendered to the user, but the calculator and executor never consume them. The UI is actively misrepresenting product behavior — toggling "Baker Pays Tx Fee" or switching `payout_mode` from Actual to Ideal does nothing. This is worse than missing functionality: it's a UX integrity bug.

Audit performed by grepping every field of `Payout_config.t` against the consumers in `src/rewards/`, `src/cli/cmd_rewards.ml`, `src/ui/pages/rewards/`, and `src/ui/rewards_scheduler.ml`.

## Decisions per field

| Field | Decision | Tracking issue |
|-------|----------|----------------|
| `baker_pays_alloc_fee` | remove | (no issue — policy: baker absorbs alloc fees) |
| `baker_pays_tx_fee` | remove | #924 |
| `gas_buffer` | remove | (no issue) |
| `kt_gas_buffer` | remove | (no issue) |
| `deser_gas_buffer` | remove | (no issue) |
| `fee_buffer` | remove | (no issue) |
| `kt_fee_buffer` | remove | (no issue) |
| `payout_mode` | remove | #925 |
| `rpc_fallback_pool` | remove | (no issue — limited value in our deployment model) |
| `min_delay_blocks` | remove | (no issue — semantics never defined) |
| `max_delay_blocks` | remove | (no issue — semantics never defined) |
| `explorer_url` | remove | #926 |

## Companion changes (per-delegator overrides)

`Rewards.delegator_override` exposes per-delegator `baker_pays_tx_fee` and `baker_pays_alloc_fee` overrides (`payout_config.ml:275-276`). These mirror the baker-wide flags and must be removed in lockstep — see open question Q1.

## Touched files (expected)

Source:
- `src/rewards/payout_config.ml` + `.mli` — drop fields from `t`, `default`, `to_json`, `of_json`, `validate`
- `src/rewards/rewards.ml` + `.mli` — drop fields from `delegator_override` if Q1 = remove
- `src/rewards/config_import.ml` — drop import-side parsing of removed fields (hjson + JSON paths)
- `src/rewards/hjson_parser.ml` — drop `baker_pays_allocation_fee` literal token if present
- `src/rewards/reward_calculator.ml` — drop the unused `estimated_tx_fee = 400L` constant if `baker_pays_tx_fee` is removed and the report no longer surfaces estimated tx fees, OR keep if the estimated-fees report column stays
- `src/cli/cmd_rewards.ml` — drop the summary lines that print removed fields (`continual_*` stays)
- `src/ui/pages/rewards/rewards_config_tab.ml` + `.mli` — drop variants from the field enum, label/value/edit branches

Tests:
- `test/test_payout_config.ml` — drop assertions on removed fields
- `test/test_reward_calculator.ml` — drop overrides referencing removed fields
- `test/test_hjson_parser.ml` — drop `baker_pays_allocation_fee` from fixtures

Other:
- `CHANGELOG.md` — `Removed` section entry referencing all three feature-tracking issues
- Shell completions: re-run `make completions` after CLI changes
- Format: `dune fmt`
- Copyright: `./scripts/check-copyright.sh`

## Open questions

### Q1: Per-delegator overrides — RESOLVED

`Rewards.delegator_override.baker_pays_tx_fee` and `baker_pays_alloc_fee` are removed completely from the codebase in lockstep with the baker-wide flags. The feature is tracked in #924; stubs add noise without value. When #924 is implemented, both the baker-wide flag and the per-delegator override will be reintroduced together.

### Q2: On-disk config backwards-compat — RESOLVED

No compatibility shim. The rewards feature is experimental. `Yojson.Safe.Util.member` already ignores unknown fields, so old `config.json` files load without error and the removed fields are silently dropped on next save. No `version` bump, no migration code.

## Out of scope

- Implementing any of the three tracked features (#924, #925, #926). Those are separate work.
- Touching `continual_*` fields — they are wired up.
- Rewriting the rewards Config tab layout — only removing rows for deleted fields.
- The `version` field itself stays (used for forward-compat in `validate`).

## Acceptance criteria

- `dune build && dune runtest && dune fmt --auto-promote && ./scripts/check-copyright.sh` all clean.
- `make completions` is up to date.
- An existing on-disk `config.json` file with the old fields loads without error after the change.
- Rewards Config tab no longer shows the removed fields.
- CHANGELOG entry under `[Unreleased] / Removed` references #924, #925, #926.
- Three feature issues remain open and unblocked.

## Atomic commit plan

1. **chore(rewards): remove dead/cosmetic config fields** — single commit covering the type, JSON, validate, config tab, hjson parser, tests, completions, changelog. Single concept: "remove fields that don't drive behavior." Not split further because the type change and every consumer must compile together (`payout_config.ml` is the dependency root).

If the diff is larger than ~400 lines, split into:
1a. **chore(rewards): remove `baker_pays_*_fee` fields and per-delegator overrides** (#924)
1b. **chore(rewards): remove `payout_mode`** (#925)
1c. **chore(rewards): remove `explorer_url`** (#926)
1d. **chore(rewards): remove unused gas/fee buffers, delay-block window, rpc fallback pool**

Each sub-commit must compile independently — the type change comes first, all consumers updated atomically with it.
