# Custom Baker Key Support in Rewards Page — Plan

Date: 2026-04-27
Source brief: `briefs/custom-baker-key-rewards-research-brief.md`
Status: Proposed (awaiting human validation per `.claude/rules/human-validation.md`)

## tl;dr

- Allow the Rewards page TUI to accept an arbitrary baker PKH (custom baker) without requiring a managed `Service.t` row.
- Persist custom bakers in a new JSON file `<registry_root>/rewards/custom_bakers.json`.
- Wire the existing `Rewards_page` baker selector, `Rewards_scheduler`, and `Payout_executor` paths to source `octez-client`, endpoint, base-dir, and payout key from the new registry when no managed service is found.
- TUI-only for MVP; CLI parity and continual-mode systemd timers are deferred (follow-up issue filed in C6).

## Binding decisions (carried verbatim from research brief §7)

These are not subject to re-clarification; sub-briefs must respect them as-is.

- **§7.1** Persistence: JSON at `<registry_root>/rewards/custom_bakers.json`.
- **§7.2** Wallet binding at creation: modal collects `payout_key_alias` AND `base_dir`; stored in JSON; Configuration tab can edit later.
- **§7.3** RPC endpoint: free-form `host:port` at creation; validate non-empty host and port in `1..65535`.
- **§7.4** `octez-client` resolution cascade: `Directory_registry` (most-recent `App_bin_dir`) → `Binary_registry` (newest managed Octez version) → `$PATH` lookup. Materialize at creation; fail if none found.
- **§7.5** TUI-only. Do NOT modify `Cmd_rewards.resolve_baker`. Do NOT run `make completions`.
- **§7.6** Continual mode out of MVP — file follow-up issue in C6.
- **§7.7/§7.8** Instance handle = `custom-<network>-<pkh-prefix-8>`. Networks must be sanitized to alphanumeric + `_-.` per `Systemd.validate_instance_name`. PKH prefixes (`tz1/tz2/tz3/tz4`) only — KT1 rejected. Reject collisions against managed services, `OM_TEST_BAKER`, and existing custom-baker entries.

## Module / file placement

`Custom_baker_registry` lives in **`src/rewards/`** (not `src/ui/`), with `.mli` first.

Justification:
- It is consumed by both the UI (`Rewards_page`, `Rewards_scheduler`) and the payout pipeline (resolution helpers for `octez_client_bin`, `endpoint`, `base_dir`, `payout_key_alias`). Putting it in `src/rewards/` lets the scheduler and executor depend on it without dragging UI code into the rewards lib.
- `Payout_config` already lives in `src/rewards/` and uses the same `<registry_root>/rewards/...` namespace; co-locating keeps the on-disk layout and OCaml module layout aligned.
- `src/ui/AGENTS.md` discourages putting non-UI persistence logic under `src/ui/`.

## Commit-by-commit decomposition

Each commit must build independently (`git rebase --exec 'dune build' main`). Tier 1 gates (`dune build`, `dune runtest`, `dune fmt`, `./scripts/check-copyright.sh`) must pass on each commit. `make completions` is NOT required (no CLI surface change).

### C1 — Expose PKH validation helpers

**Type:** refactor + test (pure)

- Expose `Payout_config.is_valid_tz_address` in `src/rewards/payout_config.mli`.
- Add a new `Payout_config.is_valid_baker_pkh : string -> bool` that accepts only `tz1/tz2/tz3/tz4` and explicitly rejects `KT1` and any other prefix; expose in `.mli`.
- Add unit tests in `test/test_payout_config.ml` covering: `tz1`, `tz2`, `tz3`, `tz4` accepted; `KT1`, empty string, malformed, `tz5` rejected.
- No call sites are migrated yet (those happen in C2/C5 where the new helpers are needed).

**Why first:** Tiny, isolated, and unblocks C2 + C5 by making the validators available.

### C2 — `Custom_baker_registry` module + on-disk format + binary cascade

**Type:** feat (new module, not yet wired)

- Add `src/rewards/custom_baker_registry.mli` and `.ml` exposing:
  - `type entry = { instance : string; baker_pkh : string; network : string; label : string option; endpoint : string; payout_key_alias : string; base_dir : string; octez_client_bin : string; added_at : string }`
  - `val list : unit -> entry list`
  - `val find : instance:string -> entry option`
  - `val add : entry -> (unit, string) result` (rejects collisions vs. managed services, `OM_TEST_BAKER`, existing custom entries; validates instance name via `Systemd.validate_instance_name`)
  - `val remove : instance:string -> (unit, string) result`
  - `val build_instance_handle : network:string -> baker_pkh:string -> (string, string) result` (sanitizes network, slices PKH prefix, returns `Error _` on invalid network/PKH)
  - `val resolve_octez_client_bin : unit -> (string, string) result` implementing the §7.4 cascade.
  - `val validate_endpoint : string -> (unit, string) result` (host non-empty, port `1..65535`).
- File path: `<Paths.registry_root ()>/rewards/custom_bakers.json` with shape `{ "bakers": [ <entry> ... ] }`. Atomic write (write-then-rename). Use `Yojson.Safe`.
- Unit tests in a new `test/test_custom_baker_registry.ml`:
  - Round-trip add/list/remove against a `tmp_dir` registry.
  - Collision rejection against a fake `OM_TEST_BAKER` ("network/pkh") and against an existing custom entry.
  - `build_instance_handle` produces `custom-<network>-<8 chars>`; rejects KT1; rejects networks with disallowed chars.
  - `validate_endpoint` accepts `host:8732`, rejects `:8732`, `host:0`, `host:99999`, `host`.
  - `resolve_octez_client_bin` cascade: stub each tier and assert priority order; failure mode when nothing resolves.
- Test wiring registered in the appropriate `dune` stanza.
- Copyright headers on new files (run `./scripts/check-copyright.sh --fix`).
- Before writing helpers, run `dune exec tools/arch_query.exe -- search "<keyword>"` for `resolve_octez_client_bin`, `validate_endpoint`, etc., to avoid duplication.

**Non-goals:** No UI wiring. No scheduler changes. Service-registry collision detection must use `Service_registry.list ()` (read-only) — do not modify the registry.

### C3 — Enumerate custom bakers in `Rewards_page` + scheduler

**Type:** feat (read-only integration)

- `src/ui/pages/rewards/rewards_page.ml` `load_baker_instances`: append `Custom_baker_registry.list () |> List.map (fun e -> (e.instance, e.baker_pkh))` to the existing union of services + `OM_TEST_BAKER`.
- `src/ui/rewards_scheduler.ml`:
  - `tick`: extend the polling loop so each custom-baker entry is polled with its `network` and `instance` from the JSON.
  - `refresh_baker`: when `Service_registry.find ~instance` returns `None`, fall back to `Custom_baker_registry.find ~instance` to obtain `network` (and any other fields used by this function).
- No new UI affordance yet. The `b` baker-cycle keybinding will pick up custom bakers automatically once they are in `state.baker_instances`.
- Verify in this commit that `OM_TEST_BAKER` continues to work (no regression).
- No new tests required for this commit (covered by C5 headless test); but `dune runtest` must still pass.

**Non-goals:** Do not introduce I/O in view functions. The JSON is loaded inside `load_baker_instances` (called from `init`/`refresh`) and inside the scheduler tick — never from a `view` function.

### C4 — Custom-baker tolerance in payout/run path

**Type:** feat

- `src/ui/pages/rewards/rewards_page.ml` `run_payout_in_background` (lines ~653–769): when `Service_registry.find ~instance` is `None`, source `octez_client_bin`, `endpoint`, `base_dir`, `payout_key_alias`, `network` from `Custom_baker_registry.find ~instance` and build `Payout_executor.context` from those fields.
- Same fallback in any other site within `rewards_page.ml` that previously hard-required `Service_registry.find` (the brief flags `network` lookup at lines 153–170 — verify it already tolerates `"unknown"` and adapt if needed by sourcing the network from the custom-baker entry instead).
- Surface a clear toast/error when a custom-baker entry is missing required fields (defensive — the creation flow in C5 enforces them, but a hand-edited JSON file may not).
- No `Cmd_rewards.resolve_baker` changes (per §7.5).
- Tests: extend an existing executor test or add a small unit test that exercises building a `Payout_executor.context` from a `Custom_baker_registry.entry` (only if the construction is non-trivial enough to warrant it). If construction is a straight field copy, defer coverage to the C5 headless test.

**Non-goals:** No systemd payout timer changes (continual mode is C6).

### C5 — "Add custom baker" modal + keybinding + headless test

**Type:** feat (UI)

- `src/ui/pages/rewards/rewards_page.ml`:
  - Register `'a'` keybinding (verify no conflict with existing bindings around lines 286–326 and 1067–1097; if `a` is taken, pick the next least-loaded printable key and document the choice in the commit body).
  - Add the binding to `keymap`, `handled_keys`, and `key_hints` (lines ~1143–1180).
  - Modal flow using `src/ui/modal_helpers.ml` primitives (`prompt_validated_text_modal`, `open_choice_modal`):
    1. Prompt for baker PKH (validated via `Payout_config.is_valid_baker_pkh`).
    2. Prompt for network (sanitized via the same alphanumeric + `_-.` rule as `Systemd.validate_instance_name`).
    3. Prompt for endpoint `host:port` (validated via `Custom_baker_registry.validate_endpoint`).
    4. Prompt for `base_dir`.
    5. Prompt for `payout_key_alias` (text input — wallet picker via `Wallets_page.get_all_keys ()` is a nice-to-have but not required by §7.2; if used, keep it scoped to display).
    6. Optional label.
    7. Resolve `octez_client_bin` via the cascade; on failure show a clear error and abort.
    8. Compute instance handle via `Custom_baker_registry.build_instance_handle`.
    9. Call `Custom_baker_registry.add`; on collision show a clear error.
    10. On success, refresh `state.baker_instances` and select the new entry.
- Headless TUI test under `test/` (model after `test_instances_page.ml` per `src/ui/AGENTS.md`):
  - Open Rewards page, send `a`, drive the modal with valid inputs, assert the new baker shows in the selector.
  - Negative case: KT1 rejected.
  - Negative case: collision rejected.
- Add a CHANGELOG entry under `[Unreleased] / Added`. Mention the TUI-only limitation (CLI parity deferred).

**Non-goals:** No CLI changes. Do not run `make completions`.

### C6 (chore) — File follow-up issue for continual mode

**Type:** chore

- Run `gh issue create --title "Continual mode (systemd payout timer) for custom bakers" --body "<see brief §7.6>"` describing the gap: `src/systemd.ml` (`write_payout_service`, `write_payout_timer`) hard-codes `--baker INSTANCE` resolved through the service registry; custom bakers cannot be installed as systemd timers without rework. Reference the merged PR.
- This is a process step, not a code change. No commit is created in the repo.

## Quality gates (per commit)

```bash
dune build
dune runtest
dune fmt
./scripts/check-copyright.sh
git rebase --exec 'dune build' main   # before pushing the branch
dune exec tools/arch_query.exe -- search "<keyword>"   # before adding any new helper
```

`make completions` is intentionally NOT in the gate set.

## Risks / open issues

None beyond §7. If the implementer discovers a new ambiguity (e.g., `network` field in `rewards_page.ml` lines 153–170 cannot tolerate `"unknown"` cleanly when a custom baker is selected), they MUST stop and surface it rather than guess.

## Sub-briefs

- `briefs/custom-baker-key-rewards-c1-implementer.md`
- `briefs/custom-baker-key-rewards-c2-implementer.md`
- `briefs/custom-baker-key-rewards-c3-implementer.md`
- `briefs/custom-baker-key-rewards-c4-implementer.md`
- `briefs/custom-baker-key-rewards-c5-implementer.md`
- `briefs/custom-baker-key-rewards-c6-implementer.md`
- `briefs/custom-baker-key-rewards-reviewer.md`
- `briefs/custom-baker-key-rewards-qa.md`
