# Research Brief: Custom Baker Key Support in Rewards Page

## 1. Goal

Allow users of the octez-manager TUI Rewards page to run payouts for **any baker
public key hash (PKH)** — including bakers that are NOT registered as managed
service instances of octez-manager. Today, the Rewards page only enumerates
baker keys discovered via the service registry (services with `role = "baker"`)
plus an undocumented `OM_TEST_BAKER` env-var hatch. The user's request is that
they should be able to enter an arbitrary baker PKH (e.g. `tz1...`) in the TUI
and run reward computations / payouts against it without first installing a
managed baker service.

A "custom baker" is a baker PKH for which:
- There is NO `Service.t` entry with `role="baker"` in the service registry
  whose delegate is this PKH.
- The user knows the PKH and (for actual broadcast) has a payout signing key
  available in some octez-client wallet (`--base-dir`).

## 2. Scope Boundary

### IN SCOPE
- A way for the user to add a "custom baker" entry in the Rewards page TUI
  (e.g. via a new modal / keybinding that prompts for baker PKH + network +
  optional label).
- That custom baker appears in the existing baker selector alongside managed
  bakers (current `(instance, pkh)` pair model).
- Persistence: custom-baker entries survive across TUI restarts (probably a
  small JSON file under `Paths.registry_root ()` — analogous to how the rewards
  config already lives at `<registry_root>/rewards/<instance>/config.json`).
- The Overview, Delegators, History, and Configuration tabs all work for a
  custom baker (read-only data via TzKT works; payouts require a payout-key
  alias and an octez-client base-dir resolved without `Service_registry.find`).
- The payout pipeline (`Payout_executor.context`) needs a way to obtain
  `octez_client_bin`, `endpoint`, `base_dir`, `payout_key_alias` for a baker
  that has no `Service.t` row. Decisions for these need to be made and
  documented in the plan.
- Validation: PKH format check (reuse `Payout_config.is_valid_tz_address`,
  exposed if needed) and de-duplication against existing managed bakers.
- The CLI `octez-manager rewards ...` family currently requires `--baker
  INSTANCE` resolved through `Service_registry.find`. Whether to extend the
  CLI to accept a raw PKH OR to mirror the TUI's custom-baker registry is an
  **open question** (see §7) — minimum viable change is TUI-only.

### OUT OF SCOPE
- Replacing the service registry baker-discovery path: managed bakers continue
  to surface as today.
- The keys/wallets page or directory registry (touched only enough to let a
  custom baker pick a wallet/base-dir for signing).
- Continual mode (systemd timer) for custom bakers — the existing systemd
  service template hard-codes `--baker INSTANCE` and assumes a service exists.
  Continual mode for custom bakers is explicitly punted to a follow-up issue
  unless the human chooses otherwise during clarification.
- Replacing `OM_TEST_BAKER`. The new persisted "custom bakers" mechanism
  supersedes its main use case but the env hatch can stay.
- New CLI subcommand surface area beyond what is strictly necessary to
  preserve behavior.

## 3. Relevant Files (with snippets and line numbers)

### TUI page state
- `src/ui/pages/rewards/rewards_state.ml` (lines 22–33): the `state` record
  carries `baker_instances : (string * string) list` — `(instance_name,
  baker_pkh)` pairs. **Adding a custom-baker source must produce entries in
  the same shape.**
- `src/ui/pages/rewards/rewards_state.ml:67–78`: `selected_baker_instance`,
  `selected_baker_pkh`, `selected_instance_name` accessors. All consumers
  treat `instance_name` as a string handle that round-trips to disk paths.

### TUI page logic
- `src/ui/pages/rewards/rewards_page.ml:34–70`: `load_baker_instances ()`
  combines services with `role="baker"` and `OM_TEST_BAKER`. **This is the
  enumeration entry point** — extending it is the cleanest way to inject
  custom bakers.
- `src/ui/pages/rewards/rewards_page.ml:653–769`: `run_payout_in_background`
  hard-requires `Service_registry.find ~instance` to obtain
  `app_bin_dir`, `rpc_addr`, `network`. This must be made tolerant of a
  custom-baker instance that lacks a service entry.
- `src/ui/pages/rewards/rewards_page.ml:153–170`: the `network` lookup for
  blueprint generation falls back to `"unknown"` if no service — this works
  but every dependent codepath must accept "unknown" gracefully.
- `src/ui/pages/rewards/rewards_page.ml:899–912`: `b` keybinding cycles baker
  selection; should also remain usable.
- `src/ui/pages/rewards/rewards_page.ml:286–326`, `1067–1097`: `keymap` and
  `handled_keys` — a new keybinding (e.g. `a` for "Add custom baker") needs
  registration here AND in `key_hints` (lines 1143–1180) AND in `handled_keys`
  (lines 1067–1097).

### Scheduler
- `src/ui/rewards_scheduler.ml:175–304`: `poll_baker ~instance ~network`.
  This already works for `OM_TEST_BAKER` with a synthetic instance like
  `test-mainnet`. The same path is reusable for custom bakers, BUT it persists
  the inferred config back to disk via `Payout_config.save ~instance` (lines
  264–283). This will create `<registry_root>/rewards/<custom-instance>/config.json`,
  which is fine, but the `instance` string used for custom bakers must be safe
  for filesystem paths (cf. `Systemd.validate_instance_name` for the format).
- `src/ui/rewards_scheduler.ml:343–381`: `parse_test_bakers` and `tick`
  enumerate bakers to poll. Custom bakers must be added to this loop.
- `src/ui/rewards_scheduler.ml:335–341`: `refresh_baker` looks up network via
  `Service_registry.find` — needs a fallback for custom bakers.

### Payout pipeline (rewards lib)
- `src/rewards/payout_config.ml:43–77`: `Payout_config.default ~baker_pkh`
  yields a config with `payout_key_alias = baker_pkh`. For custom bakers
  this default is even less useful than for managed bakers (typically the
  signing key alias differs from the PKH), so the UX must surface the
  Configuration tab to set `payout_key_alias` before any payout.
- `src/rewards/payout_config.ml:440–479`: persistence is keyed by
  `~instance` and lives in `<registry_root>/rewards/<instance>/`. Custom
  bakers will use a synthetic instance name (e.g. `custom-<sanitized-pkh>` or
  user-chosen label).
- `src/rewards/payout_config.ml:85–95`: `is_valid_tz_address`,
  `is_valid_address` — currently NOT exported in the `.mli`. **Needs to be
  exposed for input validation in the TUI.**
- `src/rewards/payout_executor.ml:10–17`: `context` record requires
  `octez_client_bin`, `endpoint`, `base_dir`, `payout_key_alias`, `instance`.
  All of these are currently sourced from a `Service.t`. Custom-baker payout
  flow must source them from somewhere else (the registry record, the
  directory registry, or user input).
- `src/rewards/payout_blueprint.ml`: blueprint generation only needs `instance`,
  `baker`, `network`, `cycle`. It loads `Payout_config` from disk by `instance`
  and reads cycle data from TzKT — no `Service.t` lookup. **Already works for
  custom bakers as long as a config file exists.**

### Where managed-baker-only assumptions live
- `src/cli/cmd_rewards.ml:30–56`: `resolve_baker` requires the instance to be
  in `Service_registry` with `role="baker"`. The TUI bypasses this for
  computation, only the executor needs it.
- `src/installer/removal.ml:94`: removing a baker service disables the
  payout timer. Custom bakers don't have payout timers (out of scope), so
  no impact.
- `src/systemd.ml:444–500` (`write_payout_service`, `write_payout_timer`):
  systemd unit names use `octez-manager-payout@<instance>` and the unit
  invokes `octez-manager rewards continual run --baker <instance>`. If
  continual mode were supported for custom bakers, the CLI's `resolve_baker`
  path would need extending too. **Confirms: continual mode for custom
  bakers is non-trivial and excluded from MVP.**

### TUI helpers we'll need
- `src/ui/modal_helpers.ml`: `prompt_validated_text_modal` (used in
  `rewards_config_tab.ml:160–174`) is the right primitive for prompting the
  PKH and any other text fields. `open_choice_modal` is fine for picking
  network from a list.
- `src/ui/pages/wallets_page.mli:30`: `get_all_keys ()` returns
  `(pkh, alias, base_dir)` triples discovered across all known base-dirs.
  This is already-loaded knowledge that the custom-baker flow can reuse to
  let the user pick a `payout_key_alias` and `base_dir` from a list rather
  than typing them.
- `src/key_aliases.mli`: OM-level alias overrides per `(base_dir, pkh)`.
  Useful display detail; not load-bearing.

### Test surface
- `test/test_payout_config.ml`, `test/test_payout_executor.ml`,
  `test/test_payout_continual.ml`, `test/test_payout_report.ml`,
  `test/test_rewards_pure.ml` — pure unit tests; safe to extend with
  validation tests for the new custom-baker registry helpers.
- No headless TUI test exists for `rewards_page.ml` today (verified by
  searching `test/`). A new headless test exercising "open custom-baker
  modal → submit PKH → baker appears in selector" is expected.
- No integration tests under `test/integration/cli-tester/tests/` cover
  rewards — no shard registration impact unless we add a new CLI command.

## 4. Architecture Notes

### Current data model (load_baker_instances)
```
Service registry (role=baker)
       │
       ▼
  (instance, pkh) ──┐
                    ├──► state.baker_instances : (string * string) list
                    │
OM_TEST_BAKER env ──┘
"network/pkh"
synthesizes (test-<network>, pkh)
```

### Proposed data model
```
Service registry (role=baker) ──┐
                                │
OM_TEST_BAKER env ──────────────┼──► state.baker_instances
                                │
NEW: Custom baker registry      │
<registry_root>/rewards/        │
  custom_bakers.json            │
{                               │
  "bakers": [                   │
    { "instance": "...",   ─────┘
      "baker_pkh": "...",
      "network": "...",
      "label": "...",
      "added_at": "..." }
  ]
}
```

The synthetic `instance` string is the unique handle that flows through:
- `Payout_config.rewards_dir ~instance` → on-disk config + reports dir
- `Payout_executor.context.instance`     → blueprint and report scoping
- `state.baker_instances` selector       → TUI display

A safe choice: instance = `custom-<sanitized-pkh-prefix>` (e.g.
`custom-tz1abc123`). User-supplied labels are display-only and stored in the
custom-bakers JSON, not used as the `instance` handle (because the handle
must be filesystem-safe and immutable; cf. `Systemd.validate_instance_name`).

### Render-loop discipline
The Rewards page already routes everything through `Rewards_scheduler` caches
for view-time reads. New custom-baker enumeration MUST NOT add I/O to view
functions — the JSON load happens in `init`, in `refresh`, or once per
scheduler tick (preferred). The scheduler tick (`tick ()`,
`rewards_scheduler.ml:363–381`) already iterates services + test bakers; it
should iterate the custom-bakers list too.

### Instance namespace collisions
A managed baker instance and a custom baker entry must never collide on the
`instance` string. Collisions would corrupt the `Payout_config` and reports
on disk. The implementation must reject custom-baker creation when the
proposed instance name already exists in the service registry, in
`OM_TEST_BAKER`, or in the custom-bakers list. Sanitization rules: same as
`Systemd.validate_instance_name` (alphanumeric + `_-.`).

### Key/wallet integration
For custom bakers, `payout_key_alias` and `base_dir` cannot be inferred from a
`Service.t`. Two acceptable UX paths:
1. **Manual**: user sets `payout_key_alias` via the Configuration tab as
   today; `base_dir` is asked at custom-baker creation time and stored in
   the custom-bakers JSON.
2. **Picker**: when creating a custom baker, offer a choice of available
   wallets via `Wallets_page.get_all_keys ()` (which already enumerates
   keys across all known base-dirs) and let the user pick `(alias, base_dir)`
   directly.
   
The MVP path is whichever the human selects in clarification — see §7. The
plan must be specific.

### app_bin_dir for the executor
`Payout_executor.context.octez_client_bin` is currently
`<svc.app_bin_dir>/octez-client`. For custom bakers we need a fallback. Two
options:
- Reuse the most recent `App_bin_dir` from `Directory_registry.list
  ~dir_type:App_bin_dir`.
- Reuse a managed binaries dir from `Binary_registry`.
- Fall back to `octez-client` on `$PATH` if neither is available.

Decision required (see §7).

### Endpoint resolution
Currently uses `svc.rpc_addr` and falls back to
`Delegate_scheduler.get_baker_node_endpoint ~instance`. For custom bakers we
need the user to either pick a managed node (instance) to use as the RPC
endpoint, or input a raw `host:port`. Storing the chosen endpoint in the
custom-bakers JSON is the cleanest.

## 5. Docs / Specs to Read

- `AGENTS.md` (root) — atomic commits, interface-first, no I/O in view path,
  `make completions` after CLI changes, copyright headers, conventional
  commits.
- `src/ui/AGENTS.md` — render loop, scheduler discipline, golden-path test
  rules. **No new install form is added (no golden-path impact).**
- `tools/AGENTS.md` — must run `dune exec tools/arch_query.exe -- search` for
  any new function before writing it; CI metrics gate blocks duplicate
  groups, large files, missing docs, missing `.mli`.
- `test/integration/AGENTS.md` — only relevant if adding integration tests
  (shard registration); not required if changes stay TUI-only.
- `.github/AGENTS.md` — verification check pattern. Not directly relevant
  unless new CI checks are introduced (they are not).
- `docs/agents/parallel-work.md` — useful if work is parallelized across
  worktrees (probably not needed for this single-PR feature).

## 6. Quality Gate Commands (exact)

Run all of these before declaring any commit done:

```bash
# Build and test (Tier 1 — non-negotiable)
dune build
dune runtest
dune fmt
./scripts/check-copyright.sh
make completions   # only if any CLI surface (cmd_rewards.ml) changes

# Per-commit independent build (must pass for the whole branch)
git rebase --exec 'dune build' main

# Architecture / duplication gate
dune exec tools/arch_query.exe -- search "<keyword>"   # before writing
                                                       # any new function
dune exec tools/arch_query.exe -- metrics -o /tmp/metrics-current.json
# CI compares against main baseline — local sanity check only
```

## 7. Open Questions (HUMAN MUST RESOLVE)

These are decisions that change the shape of the implementation. They must be
answered before the planner can produce sub-briefs.

1. **Persistence vs. session-only.** Should custom-baker entries persist
   across TUI restarts (a JSON file under `registry_root`), or only live for
   the current session? **Default proposed: persistent JSON file.**

2. **Wallet binding at creation.** When the user adds a custom baker, do we:
   - (a) ask for `payout_key_alias` + `base_dir` as part of the creation
     modal and store them in the custom-bakers JSON, OR
   - (b) only store `(pkh, network, label, endpoint)` and rely on the
     existing Configuration tab to set `payout_key_alias` (with `base_dir`
     being asked separately or sourced from the directory registry)?
   **Default proposed: (a) — atomic creation; less footguns for users about to
   broadcast a payout.**

3. **RPC endpoint for blueprint/payout.** The blueprint pipeline only reads
   from TzKT, so it works with no node endpoint. But payout execution uses
   `octez-client --endpoint <node>`. Do we:
   - (a) require the user to pick a managed node (by instance) at
     custom-baker creation, OR
   - (b) accept a free-form `host:port`, OR
   - (c) defer the choice to payout time?
   **Default proposed: (a) with (b) as a fallback if no managed node exists.**

4. **`octez-client` binary resolution.** For custom bakers, where does
   `octez_client_bin` come from? Pick one:
   - (a) `Directory_registry` most-recent `App_bin_dir`.
   - (b) `Binary_registry` newest managed Octez version.
   - (c) `$PATH` lookup of `octez-client` (system install).
   **Default proposed: (a) → (b) → (c) cascade, materialized into the
   custom-bakers JSON entry at creation.**

5. **CLI parity.** Should `octez-manager rewards <subcommand> --baker
   <instance>` accept a custom-baker `instance` string the same way it
   accepts a managed baker today? Implementing this means
   `Cmd_rewards.resolve_baker` learns about the custom-bakers JSON.
   **Default proposed: NO for MVP — keep CLI managed-only; document
   limitation; file follow-up issue if the user wants it.**

6. **Continual mode for custom bakers.** Confirmed OUT of MVP because the
   systemd unit template hard-codes `--baker <instance>` resolved through
   the service registry. Confirm with the human or drop the limitation.
   **Default proposed: OUT of MVP, follow-up issue.**

7. **Trap-question target / riskiest assumption.** The riskiest assumption is
   that **the synthetic `instance` string for a custom baker can be derived
   from the PKH alone (e.g. `custom-tz1abc...`) without user input.** If two
   networks (mainnet + a testnet) host the same PKH (impossible for tz1 but
   common with `OM_TEST_BAKER`-style synthetic setups), the on-disk paths
   collide. Mitigation: include the network in the instance handle
   (`custom-<network>-<pkh-prefix>`). Verify this with the human.

8. **PKH validation strictness.** Should we accept only `tz1/tz2/tz3/tz4`,
   or also `KT1` (smart contract) addresses? Bakers cannot be `KT1` on
   Tezos, so the validator should reject `KT1`. Confirm.

## Validation Quiz (post-brief, pre-spawn)

Before the planner is spawned, the following will be asked of the human in
the conversation (NOT inline in this file):

- **Comprehension:** *"Where is the boundary between code that already
  works for arbitrary baker PKHs and code that needs changing?"* (Expected:
  the rewards-lib payout pipeline already accepts arbitrary `~instance` +
  `~baker`; the breakage is in the TUI's enumeration source
  (`load_baker_instances`) and the executor's reliance on
  `Service_registry.find` for `app_bin_dir`/`endpoint`/`base_dir`.)

- **Clarification (binding):** *"Pick MVP semantics for questions 1–6
  above."* The chosen answers update §2 / §4 of this brief and become the
  contract for the planner.

- **Trap:** A deliberately wrong recommendation about scope, framing varied
  from prior tasks; not stated here to preserve mechanism.
