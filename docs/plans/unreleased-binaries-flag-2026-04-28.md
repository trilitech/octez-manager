# `--unreleased-binaries` flag

**Date:** 2026-04-28
**Status:** Proposed

## Goal

Expose RC/beta/prerelease versions of Octez, Signatory, and octez-index binaries
behind a single CLI/TUI flag `--unreleased-binaries`. When absent (default),
behavior is unchanged: only stable releases are surfaced.

## Background

The three downloaders already model prereleases:

- `Binary_downloader` (Octez) — `is_rc : bool`, `?include_rc:bool`
- `Signatory_downloader` — `is_prerelease : bool`, `?include_prerelease:bool`
- `Octez_index_downloader` — `is_prerelease : bool`, `?include_prerelease:bool`

But every call site outside `om binaries list-remote --all` hardcodes the
"include" parameter to `false`. The TUI install/upgrade modal, version
schedulers, and update banners therefore never offer prereleases.

## Design

### Storage

A new module `Prerelease_flag` holds a process-wide `Atomic.t bool` (default
`false`). `set : bool -> unit` is called once during CLI/TUI startup; every
prerelease-aware call site reads `get : unit -> bool`.

Process-global state is appropriate because:

- Background schedulers (`versions_scheduler`, `signatory_versions_scheduler`,
  `system_metrics_scheduler`) refresh on their own ticks — there is no
  per-call boundary to thread a parameter through.
- The flag's lifetime is one process invocation. **It is not persisted**;
  users opt in fresh each time they want it.

### CLI wiring

A shared cmdliner term `Cli_helpers.unreleased_binaries_flag : bool Term.t`.
Each affected subcommand `$`'s it onto its term and calls
`Prerelease_flag.set` at the top of its `run`:

- `cmd_binaries.ml`: `list-remote`, `download octez`, `download signatory`
- `cmd_self_update.ml`: self-update version check
- `cmd_install_*` commands that resolve "latest" through `Cli_helpers`

Existing `om binaries list-remote --all / -a` is kept as a deprecated alias —
it will set the same flag (added to the `info` names list) so old scripts
continue to work.

### TUI wiring

Add `--unreleased-binaries` to `ui_term` in `main.ml`. Set the atomic before
the Eio loop starts so schedulers see the right value on their first tick.

### Call sites to update

All hardcoded `~include_rc:false` / `~include_prerelease:false`:

| File | Line | Context |
|---|---|---|
| `src/cli/cmd_binaries.ml` | 219 | `download octez latest` resolver |
| `src/cli/cli_helpers.ml` | 32, 215, 413 | shared CLI version lookups |
| `src/version_checker.ml` | 60 | Octez update banner |
| `src/ui/versions_scheduler.ml` | 37 | TUI Octez version cache |
| `src/ui/signatory_versions_scheduler.ml` | 37 | TUI Signatory version cache |
| `src/ui/system_metrics_scheduler.ml` | 522 | TUI octez-index latest detection |
| `src/ui/system_metrics_scheduler.ml` | 493 | filtering on `is_rc` |
| `src/ui/modal_helpers.ml` | 2199 | "Download other version" modal |

Each becomes `~include_rc:(Prerelease_flag.get ())` (or the equivalent for the
prerelease parameter).

### "latest" semantics

`om binaries download octez latest`:

- Without the flag: newest stable, as today.
- With `--unreleased-binaries`: newest version among the *full* list,
  including RCs.

This is the natural reading of the flag (it changes what "available" means),
and matches `list-remote --all` already showing RCs as part of the list.

## Out of scope

- Persistent preference (no `Check_prefs` schema change).
- Separate dismissal bucket for prerelease update banners.
- Beta/alpha support for Octez itself — `versions.json` only models `rc`, so
  Octez gets RCs only. Signatory and octez-index already cover beta/alpha via
  GitHub/GitLab `prerelease` tags.

## Verification

- `dune build`
- `dune runtest`
- `dune fmt`
- `./scripts/check-copyright.sh`
- `make completions` (CLI flag added)
- Manual: `./octez-manager binaries list-remote --unreleased-binaries`
- Manual: launch TUI with the flag, confirm RCs appear in the version picker.

## Changelog

Single `Added` entry under `[Unreleased]`.
