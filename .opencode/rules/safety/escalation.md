---
name: escalation
description: Pause and ask the human before destructive, high-impact, or OCaml-API-breaking actions.
scope: global
category: safety
version: 1.0.0
---

# Escalation Triggers

Pause and ask the human for explicit confirmation before performing any of the following:

## Destructive Operations

- **Destructive file operations:** `rm -rf`, mass file deletion, overwriting files outside the current task scope.
- **Destructive git operations:** `git reset --hard`, `git push --force` / `git push -f` to any branch, `git clean -f`.
- **Force-pushing** to any branch, including feature branches.

## CI/CD and Build Changes

- **CI/CD pipeline modifications:** Changing workflow files (`.github/workflows/`), build configs, deployment scripts, or pipeline triggers.
- **Weakening CI checks:** Disabling lints, skipping hooks (`--no-verify`), relaxing thresholds, removing tests, or modifying `.metrics-accept` to accept regressions.
- **Pre-commit hook changes:** Modifying `scripts/install-git-hooks.sh` or the hook scripts themselves.

## OCaml API and Architecture Changes

- **Public `.mli` interface changes:** Modifying or removing function signatures in `.mli` files that other modules depend on. Adding is fine; changing or removing requires confirmation.
- **Golden path test count changes:** Modifying `submit_form ~downs:N` values in `test/test_golden_path_tui_v2.ml`. These are CI-only tests that cannot be verified locally.
- **Architecture metric threshold changes:** Modifying `.metrics-accept` or metrics baseline files.
- **Scheduler modifications:** Adding, removing, or changing tick rates of background schedulers (`Rpc_scheduler`, `System_metrics_scheduler`, `Delegate_scheduler`, `Data.refresh_cache`). These affect the TUI render loop performance.
- **Adding I/O to a render path:** Any I/O operation (file, network, shell) in a function reachable from a `view` function. This violates the no-I/O-in-views rule and causes visible UI lag.

## Security and Secrets

- **Auth and security changes:** Modifying permissions, access tokens, secrets, or auth configuration.
- **Logging sensitive data:** Adding log statements that might include keys, passwords, or tokens.

## Shared Infrastructure

- **Shared infrastructure:** Any action affecting resources used by other people or services (databases, DNS, monitoring configs in `monitoring/`).
- **Integration test port allocation:** Changing ports in existing integration tests (risk of collision with parallel shards).

## Process

When escalating, state:
1. **What** you intend to do
2. **Why** it's necessary
3. **Blast radius** — what could break if it goes wrong
4. **Reversibility** — how to undo if needed

Do not proceed until the human confirms.
