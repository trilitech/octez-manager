# Plan — Per-bucket rewards attribution (fix over-distribution)

Date: 2026-04-28
Author: Claude (techlead orchestration), reviewer: Valentin
Revision: v3 (final)

## Problem

`octez-manager`'s rewards module **over-distributes payouts to delegators**.

Under Tezos Adaptive Issuance + Staking, TzKT's `/v1/rewards/split/{baker}/{cycle}`
endpoint exposes each reward type (block, attestation, DAL, VDF, nonce) as four
protocol-defined sub-fields. Verbatim from the TzKT model
(`Tzkt.Api/Models/Baking/BakerRewards.cs`):

| Sub-field        | Where the protocol credits it                                          | Who actually receives it           |
|------------------|------------------------------------------------------------------------|------------------------------------|
| `Delegated`      | "received on baker's **liquid balance**"                                | Baker (must redistribute off-chain) |
| `StakedOwn`      | "received on baker's **own staked balance**"                            | Baker (frozen deposit, auto)        |
| `StakedEdge`     | "received on baker's **own staked balance**" (commission on ext stake) | Baker (frozen deposit, auto)        |
| `StakedShared`   | "received on baker's **external staked balance**"                       | External stakers (frozen, auto)     |

`block_fees` has **no** four-bucket split — it's a single value paid to the
baker's liquid balance.

**Key fact**: `StakedOwn`, `StakedEdge`, and `StakedShared` are all distributed
**by the protocol** directly into frozen deposits (Octez staking docs:
*"participation rewards are automatically shared between delegates and their
stakers"*). `octez-manager` does **not** need to redistribute any of them. The
**only** bucket the baker has to redistribute off-chain is `Delegated`.

What `octez-manager` does today
(`src/rewards/cycle_data.ml:29-34`, `src/rewards/reward_calculator.ml:30-90`):

1. Sums all four buckets per reward type into one lump (`attestation_rewards`,
   `block_rewards`, …).
2. Treats every entry under TzKT's `delegators` array (which contains both
   pure delegators *and* external stakers — anyone with non-zero
   `delegated_balance` or `staked_balance` toward this baker) uniformly:
   `balance = delegated + staked`.
3. Distributes the entire lump proportionally to `balance / total_staking`.

Net effect, every cycle:

- Delegators receive a share of `StakedOwn`, `StakedEdge`, `StakedShared` —
  rewards they have **no** protocol claim to. (`StakedOwn` and `StakedEdge`
  are baker-only; `StakedShared` already went to stakers via the protocol.)
- External stakers receive a share of `Delegated` — rewards they have no
  claim to (their stake earned via the staking pool, not delegation).
- External stakers receive a *second* payment from `StakedShared` —
  **double-paid**, since the protocol already credited their frozen deposit.

Real money, real over-payments, every cycle.

## Goal

Make per-cycle off-chain payout attribution match what the baker is actually
responsible for distributing:

- The pool to distribute = sum, across the five reward types, of the
  `Delegated` sub-field only.
- Distribute that pool by `delegator.delegated_balance / total_delegated_balance`
  (where `total_delegated_balance = own_delegated_balance + external_delegated_balance`).
  An entry with `delegated_balance = 0` (pure external staker) receives **zero**
  from octez-manager — by design — because the protocol already paid them.
- The baker keeps: their own_delegated share of the `Delegated` pool, plus
  every `*StakedOwn` and `*StakedEdge` (already in their frozen deposit, so
  this is just an accounting display, not a payout), plus `block_fees`
  (see open question below).

The four buckets remain visible in the cycle-detail UI per the user's
original ask ("per cycle we need to distinguish ... rewards delegated,
staked own, stake edged and shared"). Display-only for the three staking
buckets; the `Delegated` bucket drives the payout math.

## Out of scope

- Retroactive correction of past payouts already sent on-chain. Fix forward
  only, with a CHANGELOG warning.
- Reworking the cycle-detail UI beyond exposing the four buckets.
- Touching the protocol-level staking flow at all. We do not pay stakers.
- Changing the on-disk payout config schema.

## Distribution pool — exact composition

The pool to distribute is:

```
block_rewards.delegated
  + attestation_rewards.delegated
  + dal_rewards.delegated
  + block_fees
```

Excluded from the pool:

- All staking sub-fields (`*StakedOwn`, `*StakedEdge`, `*StakedShared`) —
  the protocol credits these directly into frozen deposits. Redistribution
  by octez-manager would be a no-op for `StakedOwn`/`StakedEdge` (already
  the baker's) and a double-payment for `StakedShared` (already on-chain
  with the staker).
- VDF and nonce revelation rewards (all four sub-fields). These accrue
  from baker-specific protocol operations (revealing seed nonces,
  computing the VDF) and are treated as baker income.

`block_fees` enter the pool unconditionally (no scaling, no toggle): the
protocol pays them entirely to the baker's liquid balance, and the
baker's policy is to share them with delegators pro-rata to delegated
stake.

## Design

### Data model (`src/rewards/rewards.ml`)

```ocaml
type reward_split = {
  delegated     : Int64.t;
  staked_own    : Int64.t;
  staked_edge   : Int64.t;
  staked_shared : Int64.t;
}

type cycle_rewards = {
  ...
  block_rewards         : reward_split;
  attestation_rewards   : reward_split;
  dal_rewards           : reward_split;
  vdf_rewards           : reward_split;       (* split out from "other"        *)
  nonce_rewards         : reward_split;       (* split out from "other"        *)
  block_fees            : Int64.t;            (* single field, no split        *)
  ...
}

val total_of_split : reward_split -> Int64.t
val delegated_pool : cycle_rewards -> Int64.t   (* sum of all .delegated   *)
val total_earned   : cycle_rewards -> Int64.t   (* unchanged externally *)
```

VDF and nonce are split out of the previous `other_rewards` lump so the UI
can show all five reward types with full per-bucket breakdown.

### Fetch (`src/rewards/cycle_data.ml`)

- Drop `sum_reward_fields`. Add `parse_reward_split json prefix` returning
  a `reward_split` from `<prefix>{Delegated,StakedOwn,StakedEdge,StakedShared}`.

### Attribution (`src/rewards/reward_calculator.ml`)

```ocaml
let delegated_pool cr =
  Int64.add cr.block_rewards.delegated
  @@ Int64.add cr.attestation_rewards.delegated
  @@ Int64.add cr.dal_rewards.delegated
  @@ cr.block_fees
  (* VDF + nonce: baker keeps *)

let total_delegated cr =
  Int64.add cr.own_delegated_balance cr.external_delegated_balance

let gross_for cr d =
  let pool = delegated_pool cr in
  let total = total_delegated cr in
  if total = 0L then 0L
  else
    Int64.of_float
      (Int64.to_float pool *. Int64.to_float d.delegated_balance
       /. Int64.to_float total)
```

A delegator entry with `delegated_balance = 0` (pure external staker)
naturally gets `gross = 0`. No payout, by design. `staked_balance` no longer
participates in the off-chain payout math at all.

**Overdelegation cap**: today applied to `delegated + staked`; under the
corrected model only `delegated` matters. Cap remains
`own_staked_balance × 9` applied to the delegated component only. (The
baker's actual on-chain delegation ceiling is documented separately; this
matches the existing protective behavior.)

**Baker share / bond income**: the baker's own slice of the Delegated pool,
i.e. `pool × own_delegated_balance / total_delegated_balance`. Plus, for
display in the cycle summary (not payout): the `*StakedOwn` and `*StakedEdge`
totals — these are accounting-visible baker income but already in the
baker's frozen deposit, so no payout action.

### UI (`src/ui/pages/rewards/rewards_overview.ml`)

For each of the five reward types, show all four buckets:

```
Block         delegated 1,234 │ own 567 │ edge 12 │ shared 89
Attestation   delegated 4,321 │ own 890 │ edge 23 │ shared 765
DAL           delegated   100 │ own  10 │ edge  1 │ shared  20
VDF           delegated     5 │ own   1 │ edge  0 │ shared   2
Nonce         delegated     8 │ own   2 │ edge  0 │ shared   3
Block fees    32 (baker)
```

Add a footer line clarifying "staking rewards (own/edge/shared) are paid by
the protocol; this view is for accounting only".

`rewards_delegators.ml` unchanged for this PR.

### CSV (`src/rewards/payout_report.ml`)

Per-delegator CSV columns unchanged (the bug fix produces *correct*
per-delegator numbers in the existing columns). Append two cycle-summary
columns to the cycle-summary CSV: `delegated_pool`, `share_block_fees`,
so audits can reconstruct the math.

### Tests

- **Unit (rewrite)**: `test/test_reward_calculator.ml`
  - `pure_delegator_gets_only_delegated`: delegator with `delegated=1000,
    staked=0` against a cycle with `Delegated=100, StakedOwn=200,
    StakedEdge=50, StakedShared=300` and `external_delegated_balance=1000`,
    `external_staked_balance=2000` → gross = 100 (whole delegated pool, since
    they're the only delegator and own_delegated=0). Asserts they get **none**
    of the staked buckets.
  - `pure_external_staker_gets_zero`: entry with `delegated=0, staked=5000`
    → gross = 0. Confirms protocol-paid stakers are never paid by us.
  - `mixed_delegator_staker`: entry with `delegated=500, staked=500` →
    gross = share of Delegated pool weighted by 500 / total_delegated only.
    The 500 staked balance contributes nothing.
  - `baker_share_correct`: baker's own_delegated=2000, external_delegated=
    8000, pool=10000 → baker gets 2000.
  - `vdf_and_nonce_excluded_from_pool`: setting `*RewardsDelegated` to a
    non-zero value for VDF / nonce in the cycle does not change any
    delegator's `gross_reward`. Confirms revelation rewards stay with the baker.
  - `block_fees_in_pool`: pool includes 100% of block_fees (no toggle).

- **Unit (parser, new)**: `test/test_cycle_data.ml`
  - Parse a fixture JSON with all four sub-fields populated for each reward
    type; assert each `reward_split` matches.

- **Integration**: no changes (cycle-detail integration test asserts UI
  presence, not numbers).

### CHANGELOG

```
### Fixed
- **Rewards over-distribution (BREAKING for payout amounts)**: per-cycle
  payouts are now computed from only the `Delegated` portion of block,
  attestation, and DAL rewards (plus 100% of block fees), distributed
  pro-rata to each delegator's `delegated_balance`. Previously,
  octez-manager pooled all four protocol buckets (`Delegated`,
  `StakedOwn`, `StakedEdge`, `StakedShared`) for every reward type and
  distributed the whole lump weighted by
  `delegated_balance + staked_balance`. This over-paid delegators (giving
  them shares of `StakedOwn` and `StakedEdge` which belong to the baker)
  and double-paid external stakers (whose `StakedShared` rewards the
  protocol already credited to their frozen deposits). VDF and nonce
  revelation rewards now stay with the baker. Operators MUST review the
  next cycle's blueprint before sending; payout amounts will drop
  substantially for bakers with significant external staking.
  (fixes #<TBD>)

### Added
- Cycle overview shows the four protocol buckets (delegated / own / edge /
  shared) per reward type, plus block fees and revelation rewards as
  baker-only income, for accounting visibility.
```

## Commits (atomic, each compiles)

1. `refactor(rewards): split reward bucket type — pure refactor, lump → reward_split`
   - Introduces `reward_split`. `cycle_data.ml` parses 4 fields. View code keeps
     current displayed numbers via `total_of_split`.
   - **Behavior unchanged.**
2. `fix(rewards): attribute payouts only from delegated pool`
   - Rewrites `generate_blueprint`. Pool = `block.delegated +
     attestation.delegated + dal.delegated + block_fees`, distributed by
     `delegated_balance / total_delegated`. Updates tests.
   - **This is the behavior change.**
3. `feat(rewards): show four-bucket breakdown in cycle overview`
4. `docs(changelog): note rewards attribution fix`

## Verification

- `dune build` — every commit independently.
- `dune runtest` — new tests pass; old tests rewritten for correct math.
- `dune fmt`, `./scripts/check-copyright.sh` — clean.
- `make completions` — no CLI surface change, run anyway.
- `tmux` smoke at 220×50 and 100×30: cycle overview renders.

## Risks

- **Operators see substantially lower payouts after upgrade.** Intentional
  but surprising. Mitigated by CHANGELOG warning + UI breakdown.
- **`external_delegated_balance = 0`**: division-by-zero if a baker has only
  stakers, no delegators. Guard returns zero pool distribution.
- **Float round-trip**: same approach as today; no precision regression.

## PR submission

Branch: `fix/rewards-bucket-attribution`. Open against `main` after all
commits build, test, lint cleanly. No force-push.
