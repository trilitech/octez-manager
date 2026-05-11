# Changelog

All notable changes to this project will be documented in this file.

The format is based on [Keep a Changelog](https://keepachangelog.com/en/1.1.0/).

## [Unreleased]

### Changed

- **Build dependencies sourced from opam**: `miaou-*` (>= 0.5.2) and `ppx_forbid` / `ppx_enforce` (>= 0.1.0) are now installed from the regular opam repository instead of git pins. The `pin-depends` block, the `octez-manager.opam.template`, the per-package `opam pin add` steps in CI (`ci.yml`, `coverage.yml`, `integration-tests.yml`, `Dockerfile.ci-debian`, `test/integration/build-static.sh`, `Makefile`), and the `.github/miaou-version` cache-buster have all been removed. `opam install . --deps-only` is sufficient.

### Added

- **Rewards page: Add custom baker**: The Rewards page now supports payouts for arbitrary baker keys via a new "Add custom baker" modal (press `a`). Custom bakers do not require a managed Octez service — only a PKH, network, RPC endpoint, octez-client base directory, and payout key alias are needed. The Add wizard offers list-based pickers backed by the same sources as the install flows (Teztnets networks, public + local node endpoints, registered base directories plus those discovered from baker/accuser env files, and keys read from the chosen base directory). The Configuration tab surfaces the custom-baker fields (Label / RPC Endpoint / Base Directory / Payout Key) for in-place editing; PKH and Network are read-only. Press `x` on a custom baker to remove it — the systemd payout timer, saved Payout_config, and registry entry are torn down together. CLI subcommands (`rewards status`, `pay`, `generate`, `history`, `continual …`, `notify test`, `config import`) accept custom-baker instances, and the systemd payout timer runs correctly against them.
- **Cascade import in TUI**: Import wizard now supports cascade import mode, allowing users to import services with all their dependencies or dependents in the correct order. Toggle with 'c' in the configure step. (fixes #886)
- **Rewards first-run guidance**: When no payout configuration exists for the selected baker, the Rewards Overview shows a "Set up rewards" panel pointing to the Configuration tab; the Configuration tab shows a `[c: create]` status line until the first save. Action shortcuts that require config (`g`/`p`/`d`/`t`) are hidden on the Overview until then.
- **Indexer URL configuration in TUI**: New "Indexer URL" field in the Rewards Configuration tab. Editing it opens a picker listing local octez-index services for the baker's network, the network-appropriate TzKT URL, and a "Custom URL..." entry for free-form input.
- **DAL rewards in per-cycle info**: The Rewards page's *Last Completed* and per-cycle detail panels now show DAL attestation rewards on their own row, alongside Block / Attestation / Block Fees. The earned-rewards total now includes DAL.
- **Per-delegate payout breakdown in cycle detail**: The cycle-detail view on the Rewards Overview now lists every payout attempt (paid ✓ / failed ✗) read back from `payouts.csv`, with the per-delegate amount and the failure reason from octez-client. A separate "Excluded" section lists pre-execution skips (below min payout/balance, ignored, etc.) from `invalid.csv`.
- **Delegate sub-row per reward type in cycle detail**: The Last Completed and cycle-detail boxes now expose, under each of block, attestation, and DAL rewards, the delegate-pool slice — the portion octez-manager actually distributes off-chain — so operators can see at a glance how much of each reward type is destined for delegators.
- **Payout key picker in Rewards configuration**: Editing the Payout Key on the Rewards Configuration tab now opens a base-dir picker (default `~/.tezos-client`, registered base dirs, and base dirs discovered from baker/accuser service env files) followed by a key picker that lists the keys in the chosen directory, falling back to free-text entry when none are found.

### Changed

- **Rewards: dedicated "Failed" payout status**: A cycle whose payouts all failed is now shown as `Failed` (red) instead of `Partial`. `Partial` is reserved for cycles where some delegates were paid and some were not.

### Removed

- **Rewards Configuration tab: dead and cosmetic fields**: removed `Payout Mode` (Actual/Ideal), `Baker Pays TX Fee`, and `Baker Pays Alloc Fee` toggles, plus the unused `gas_buffer`, `kt_gas_buffer`, `deser_gas_buffer`, `fee_buffer`, `kt_fee_buffer`, `min_delay_blocks`, `max_delay_blocks`, `rpc_fallback_pool`, and `explorer_url` fields, along with the `baker_pays_tx_fee` / `baker_pays_alloc_fee` per-delegator overrides. None of these affected payout behavior — they were stored, serialized, and shown to the user but never read by the calculator or executor. The Configuration tab now only exposes settings that actually drive the system. Fee pass-through tracked in #924, ideal-mode payouts tracked in #925, explorer links in payout reports tracked in #926.
- **Rewards: surface octez-client failure reason**: When a batch transfer exits 0 but produces no operation hash, the per-delegate Note now contains the actual error reason (e.g. "Balance of contract … too low") instead of the placeholder "no operation hash in output". Internally, payout execution now captures both stdout and stderr from octez-client.
- **Diagnostics page moved to Experimental tab**: The Diagnostics page is now accessible via the Experimental features modal (press `6`) instead of having its own top-level tab.
- **Rewards page: History tab removed**: The standalone History tab and its "Trends (Last 30 Cycles)" sparkline panel are gone. Cycle navigation lives on the Overview tab — the *Recent Cycles* table is now interactive (`j/k` to move the cursor, `Enter` to drill into a cycle's detail). The action shortcuts `g`/`p`/`d` (Generate / Pay / Dry-run) target the highlighted cycle when one is selected, falling back to the latest fetched cycle otherwise. Tab numbering shifts: Configuration is now `3`, not `4`.

### Fixed

- **Installer and release downloads now fail closed on integrity checks**: release install docs no longer recommend piping the mutable `main` installer directly into a shell, `install.sh` verifies the downloaded binary against `sha256sums.txt` before installing, release `.deb` artifacts publish checksum files, and Octez/Signatory downloads now abort on missing or mismatched checksum entries.
- **Rewards payouts no longer mark failed cycles as paid**: real payout runs now write `summary.json` only when all intended transfers succeed with operation hashes, keep all-failed and partial cycles retryable, serialize same-cycle execution under the cycle lock, and report summary write failures instead of silently continuing.
- **Rewards notification secrets are no longer passed on the command line**: Discord, Telegram, and webhook bearer credentials are written to private temporary curl config files, and saved payout configs are written with `0600` permissions.
- **System-mode service hardening is documented and tested**: system-mode guidance now distinguishes root management actions from unprivileged daemon runtime, and systemd unit tests assert hardening directives such as `NoNewPrivileges`, `PrivateTmp`, and `ProtectSystem`.
- **Rewards "Set up rewards" CTA no longer advertises notification channels**: The first-run CTA shown when a baker has no payout configuration listed `notification channels (Discord/Telegram/...)` as something to set in the Configuration tab, but that setting is not actually exposed there. The line has been removed; tracked for real implementation in #930.
- **Rewards over-distribution (BREAKING for payout amounts)**: per-cycle payouts are now computed from only the `Delegated` portion of block, attestation, and DAL rewards (plus 100% of block fees), distributed pro-rata to each delegator's `delegated_balance`. Previously the rewards engine pooled all four protocol buckets — `Delegated`, `StakedOwn`, `StakedEdge`, `StakedShared` — for every reward type and distributed the lump weighted by `delegated_balance + staked_balance`. That over-paid delegators (giving them shares of `StakedOwn` and `StakedEdge`, which the protocol credits to the baker) and double-paid external stakers (whose `StakedShared` rewards the protocol already credits directly to their frozen deposits). VDF and nonce revelation rewards are now baker-only income. An external staker (a delegator entry with `delegated_balance = 0`) receives nothing from octez-manager — by design — because the protocol has already paid them. **Operators MUST review the next cycle's blueprint before sending; payout amounts will drop substantially for bakers with significant external stake or own staked balance.**
- **Rewards CSV reports corrupted by multi-line failure notes**: When `octez-client` returned a multi-line error (e.g. "Command failed: …\nOutput:\n…"), the writer dumped it raw into the `note` column with literal newlines, breaking each delegator's record into ~100 fake rows. The cycle detail then showed "0/872 paid" with garbage addresses and amounts when only 8 delegators actually existed. The writer now sanitizes newlines/tabs in the `note` field, the executor stores only the last non-empty line of octez-client output as the note, and the reader has been switched to a streaming CSV parser that recovers existing corrupt files (multi-line quoted fields are read as a single field).
- **Rewards page falsely reports payouts as "Partial"**: `extract_op_hash` only recognized operation hashes whose first two characters were `oo`, but Tezos operation hashes are 51 base58 characters starting with a single `o` and a varying second character (`op…`, `on…`, `or…`, …). Successful payouts were therefore recorded with `success = false`, leading the rewards page to label fully-paid cycles as "Partial" with `paid_delegators: 0` and `distributed_rewards: 0` even when on-chain transfers had completed and were visible on tzkt. Hash extraction now matches any well-formed 51-char base58 operation hash. Existing summary files for affected cycles are not rewritten — re-running a payout for those cycles is required to refresh the on-disk summary.

## [1.0.0] - 2026-04-16

### Added

- **Directory picker: type a path directly**: Form directory pickers now include a "Type a path directly..." option for entering arbitrary paths without navigating the filesystem tree. Useful for paths on remote mounts or outside home directories. (closes #800)
- **`om list --internal` flag**: Lists only managed services, complementing `--external`. Useful for scripting and automation.
- **Experimental features tab**: Beta features (Sandbox, Rewards) now live under a new "Experimental" tab (press `7`) with descriptions and badges.
  - **octez-index support**: octez-index (TzKT-compatible indexer) is now a first-class managed service. Install, edit, start/stop/restart, and import external systemd units.
  - **Local indexer compare mode**: `om ui --local-indexer <URL>` registers a local TzKT-compatible indexer for rewards/delegation queries. `--compare-indexers` logs divergences vs public TzKT.
  - **`rewards continual tick` command**: One-shot command for external schedulers (cron, systemd timers) to dispatch due payouts. `rewards continual start` can optionally install a systemd timer.
  - **Baker multi-node support**: Bakers can specify extra node instances or RPC endpoints for redundancy via a multi-select modal in the install/edit form.
  - **Sandbox improvements**: Yes-wallet integration for automatic delegate generation; restored multi-node/multi-baker topology parameters.

### Changed

- **Tab-based main navigation**: The app now opens with a tab navigation bar (Instances, Wallets, Diagnostics, Topology, Sandboxes, Experimental). Switch tabs with number keys. The global `K` shortcut, "New Instance" dropdown, and redundant `m` global menu have been removed in favor of the tab system and inline "Add new" ghost entries.
- **Instances page redesign**: Visual action buttons replace plain text labels; a view-mode toggle row shows By Role / By Group; contextual help footer changes based on cursor zone; services grouped by role in bordered containers.
- **Help modal (`?`) shows per-page shortcuts**: Displays both global and page-specific shortcuts with concise action names.
- **Wallet network picker**: When a network has both a local node and public endpoints, separate entries are shown. Syncing nodes are greyed out with "(syncing..)" and cannot be selected.
- **Keys page renamed to Wallets**: The Keys management page is now called "Wallets" throughout the UI.
- **Log export runs in background**: Exporting instance logs no longer freezes the UI. A progress bar and success toast are shown.
### Fixed

- **Wallet balances not fetched on page load**: Balances are now fetched proactively on startup and refreshed every 30 seconds.
- **Wallet operations hanging on non-bootstrapped nodes**: Operations now pass `--wait none` to prevent blocking indefinitely.
- **Wallets page missing baker/accuser wallets**: Wallet directories from installed baker and accuser services are now discovered automatically.
- **Wallet fold/unfold keybinding**: Changed from Space to Tab; Space now switches between list and detail panels.
- **Wallet modal misleading error with no delegates**: Shows "No delegates found in wallet" instead of a generic node-unreachable error.
- **Network names shown as URLs in wallet actions**: Network picker shows clean names (e.g., "shadownet") instead of raw URLs.
- **Validated text modals not enforcing validation**: Pressing Enter can no longer bypass validation in text input modals.
- **Unmanaged Instances keyboard navigation**: Cursor now correctly moves into the "Unmanaged Instances" section and is visible on external service entries.
- **Instances page arrow key navigation**: Fixed navigation to "Add new" entries in empty service lists and multi-column layouts.
- **Baker form delegates reset on base dir change**: Changing base dir now clears stale delegate selections.
- **DAL node data dir not tracking instance name**: Data directory now stays in sync with instance name.
- **DAL import preserves original config**: Importing a DAL node no longer rewrites its configuration on every startup. (closes #793)
- **Snapshot kind display**: No longer shows redundant `(kind)` suffixes; empty labels fall back to kind slug. (closes #113)
- **`ensure_dir_path` permissions**: Directory permissions are now always applied correctly, even without root privileges. (closes #744)
- **`--version` reports build version**: Now reports version from dune build system instead of hardcoded string.
- **Rewards non-mainnet TzKT routing**: Fixed incorrect rewards data for non-mainnet bakers.
- **Imported key not visible until restart**: Key list updates immediately after importing a watch-only address.
- **Global shortcuts not accessible from RPC browser/log viewer**: `?`, `C-t`, and other global shortcuts now work on all pages.
- **Download progress bar** now updates in real time. (fixes #798)
- `octez-manager instance <name>` with unknown name now shows "Unknown instance" instead of misleading "ACTION required"
- `octez-manager baker list` no longer crashes when no baker instances are installed
- Shell tab-completion for command groups now correctly offers subcommands
- Zsh tab-completion for commands with colons no longer crashes the shell
- **External baker/accuser command structure**: Fixed crash-loop when importing external baker/accuser services.
- **Import with environment variables**: Import now works when systemd services use environment variables in their configuration.
- **Cascade import DAL connection loss**: Baker no longer loses its DAL node connection when imported with `--cascade`.
- **Import stopped nodes**: Network detection reads from `config.json` when RPC is not accessible.
- **Baker/accuser purge preserves node data**: Shared data directories are no longer deleted. (fixes #727)
- **RPC Browser responsiveness**: RPC browser no longer freezes the UI during slow network responses. (fixes #673)

## [0.3.0] - 2026-02-11

### Added

- **RPC Browser streaming**: Monitor/streaming RPC endpoints (e.g., `/monitor/heads/main`) now display live-updating content instead of timing out. Streaming is detected automatically and uses the pager's streaming mode with incremental JSON syntax highlighting.
- **Web interface**: `octez-manager web` starts an HTTP server with browser-based terminal access via xterm.js over WebSocket. Supports controller/viewer roles with optional password authentication (`--password`, `--viewer-password`).
- Coverage badge on README, updated automatically from CI via dynamic gist-based shield
- Fuzz testing with adversarial generators for core parsers (Env_file_parser, Execstart_parser, Teztnets, Snapshots) — 13 properties at 10k iterations each
- Property-based testing with QCheck: 57 properties covering parsers, validators, formatters, and data transformers (#587)
- TUI now shows notification when octez-manager update is available and recommends running `self-update` command
- **RPC Browser**: Interactive RPC endpoint explorer accessible via `r` key
  - Navigate endpoint tree with cursor keys and Enter
  - JSON syntax highlighting for responses
  - Response time and size displayed in result header
  - Quick access shortcuts (1-5) for common endpoints
  - Smart defaults for dynamic segments (chain_id → "main", block_id → "head")
  - Instance selection via Tab to query any managed node
  - Browse RPC action in node context menu
- **RPC CLI**: Command-line RPC tools with `octez-manager rpc` subcommands
  - `rpc get -i INSTANCE PATH` - Execute GET request
  - `rpc list -i INSTANCE [PATH]` - List available endpoints
  - `rpc interactive -i INSTANCE` - Interactive mode with tab completion
  - `rpc instances` - List available node instances
- **Architecture index tools**: `arch_index` scans `.cmt`/`.cmti` files to populate `docs/architecture.db` with modules, functions (with type signatures and doc comments), types (with record fields and variant constructors). `arch_query` CLI provides fuzzy search on intents, type-shape search, duplicate detection, and code health queries.
- **OpenCode configuration**: `opencode.json` provides OCaml LSP, auto-formatting, custom commands (`/build`, `/test`, `/fmt`, `/copyright`, `/pre-commit`, `/index`, `/archdb`), and pre-allowed permissions for safe commands.

### Changed

- **Integration test isolation harness**: Added automatic resource tracking and cleanup to `lib.sh` via EXIT trap. Tests use `test_init`, `register_instance`, `register_external_service`, `register_data_dir`, `register_process`, and `alloc_port` for deterministic port allocation and guaranteed cleanup on exit. All 56 integration tests migrated. Fixes parallel test interference where leaked resources from one test could cause false failures in another
- **CI fix**: Fixed opam cache never being used due to HOME directory mismatch between Docker image (`/root/.opam`) and GitHub Actions (`/github/home/.opam`). Set explicit `OPAMROOT=/root/.opam` so the pre-built switch from the Docker image is found. Expected to reduce Build and Test job from ~13 min to ~2-3 min
- **Miaou 0.3 migration prep**: raised Miaou package minimum versions to 0.3.0, removed `"__EXIT__"` sentinel-based quit navigation from the instances page, and typed Context pending navigation requests (`Goto|Back|Quit`) instead of raw string payloads
- Documented Miaou 0.3 matrix driver tuning environment variables (`MIAOU_MATRIX_TPS`, `MIAOU_MATRIX_FPS`, `MIAOU_MATRIX_SCRUB_FRAMES`) in README with practical presets
- `arch-query compare` now shows per-item detail under each regressed (or improved) metric, listing the specific functions, files, or modules responsible (fixes #683)
- Updated Miaou TUI library to 0.2.6 with new `Key_event.result` API (`on_key`, `on_modal_key`, `key_hints`)
- Migrated all background I/O from per-task OS domains to a fixed-size Eio fiber pool (head monitors, background runner, binary downloads, and schedulers now share 4 pooled domains)
- Renamed "linked directories" to "registered directories" (CLI commands: `binaries link` → `binaries register`, `binaries unlink` → `binaries unregister`)
- **CI optimization**: Coverage collection now runs only on main branch pushes, not on PRs. This reduces PR feedback time from ~45 minutes to ~10-12 minutes while maintaining coverage baseline tracking on main.
- **CI optimization**: Build stage now ~80% faster (5 min → 1 min) through improved cache logic, pre-installed miaou packages in CI container, and parallel opam operations
- **CI optimization**: Integration tests within each shard now run in parallel (3 jobs per shard), reducing per-shard time by ~40-50%
- **CI optimization**: Docker layer caching for integration tests reduces container build time by 40-60% (2-3 min per run)
- Diagnostics page now uses bordered boxes (Box_widget) for cleaner visual section separation
- Binaries page sections (Managed Versions, Registered Directories, Available for Download) now render with Box_widget Rounded borders in distinct colors instead of manual ASCII art
- RPC browser now uses Grid_layout for side-by-side panel rendering
- Instances page now uses Grid_layout for multi-column layout merging
- Form navigation now supports Tab/Shift+Tab cycling between fields via Miaou Focus_ring
- **Code deduplication**: Consolidated duplicate functions into canonical locations, reducing duplicate_groups metric from 63 to 23. Round 1 (PRs #652–#657): extracted shared utilities into Common, Service_registry, Systemd, Helpers, Config, Modal_helpers, Form_builder_bundles. Round 2: extracted Check_prefs module from self_update_checker/version_checker, removed dead Common.is_port_in_use code, replaced hand-rolled with_lock with stdlib Mutex.protect. Architecture index now detects delegation aliases (`let f = Module.f`) via typed AST and excludes them from duplicate metrics
- **God module split**: Split `common.ml` (916 lines, ~50 functions) into 5 focused submodules in `lib/common/`: `Paths` (filesystem paths, XDG dirs), `Cmd_runner` (shell execution), `File_ops` (file/directory operations), `Download` (HTTP downloads, checksums), `String_utils` (formatting, editor, string helpers). All 436 call sites across 79 files migrated from `Common.*` to direct submodule calls
- **God module split**: Split `instances_actions.ml` (1414 lines, 38 functions) into 4 focused submodules: `Instances_helpers` (shared action helpers), `Instances_lifecycle` (start/restart with cascade, edit), `Instances_external` (external/unmanaged service actions), `Instances_update` (version update, cascade update, rollback). Core module reduced to 348 lines
- **God module split**: Split `binaries_page.ml` (879 lines, 38 functions) into 4 focused submodules in `ui/pages/binaries/`: `Binaries_types` (shared types), `Binaries_data` (data loading, item building), `Binaries_actions` (side-effecting action handlers), `Binaries_view` (rendering). Core module reduced to 282 lines
- **Test helper dedup**: Extracted 9 duplicate substring-search helpers (`contains_substring`, `string_contains`) into shared `test_string_helpers_lib` and replaced 4 duplicate `make_service` helpers in RPC browser tests with `Mock_service_helpers.mock_service`. Fixes an empty-needle bug in `test_cli_progress.ml`
- **Version handling consolidation**: Replaced duplicate `parse_version` and `is_rc_or_dev` in `system_metrics_scheduler.ml` with canonical `Version_utils` functions. Version comparison now correctly considers patch versions (previously only compared major.minor)
- **Binary help explorer refactor**: Extracted deeply-nested modal modules from `binary_help_explorer.ml` (3-level nesting → top-level). `Flags_modal` is now a standalone module, `open_value_modal`/`open_toggle_modal`/`edit_row_value` are top-level functions, and `open_modal` is a thin 19-line wrapper. Removed dead `_scroll_indicator` code
- **Documentation coverage**: Added doc comments to 138 exposed functions across 25 `.mli` files, raising doc_coverage_pct from 54.8% to 62.7% and reducing missing_docs from 139 to 1

### Fixed

- RPC Browser: toggling focus in side-by-side result mode now always targets an existing pager ID (avoids invalid focus state after pager removal)
- Multi-column layout: pressing Up from the first instance now correctly navigates to "Browse RPCs" instead of jumping to "Install new instance"
- Weeklynet instance names now include the full date (e.g., `node-weeklynet-2026-02-04`) instead of being truncated to `node-weeklynet-2026-0` (fixes #640)
- Weeklynet node installation no longer fails with 404 when teztnets.com API is unreachable (removed stale fallback URL)
- Weeklynet node installation fails because snapshot URL includes dated suffix (e.g. `weeklynet-2026-02-04`) which does not exist on tzinit.org; now stripped to `weeklynet` (fixes #675)
- Ghostnet network can no longer be selected despite deprecation (removed from all network selection code paths)
- RPC Browser: Ghostnet URLs no longer appear in public nodes list (previously showed as "Unknown" network)
- RPC Browser: Local instances now display network name (e.g., "Shadownet") instead of full URL (e.g., "https://teztnets.com/shadownet")
- RPC Browser: Fixed duplicate network names in instance display when selecting target endpoints (fixes #599)
- Architecture index: intent restoration no longer fails on doc comments containing special characters (used prepared statements instead of string interpolation)
- System metrics (CPU, memory, disk) start populating without the previous startup delay
- `make completions-check` no longer modifies the working directory when completions are out of date
- Systemd service template warnings about unknown keys `StartLimitIntervalSec` and `StartLimitBurst` in [Service] section (moved to [Unit] section)
- Pressing or holding Esc on modals no longer accidentally quits the application (Miaou Esc cooldown after modal close)
- Application shutdown could hang for up to 10 minutes when background schedulers were sleeping; now exits within ~0.5s
- Install-node and import-service jobs no longer time out after 120 seconds; long-running operations like snapshot downloads now run without a timeout (fixes #676)
- Install/edit forms no longer flash with default values when submitting

### Removed

- Ghostnet support - Ghostnet testnet has been deprecated and is no longer available as a network option

## [0.2.0] - 2026-01-29

### Added

- **Self-Update System**: Check for and install octez-manager updates
  - `octez-manager self-update` CLI command with `--check` (check only) and `--force` options
  - `octez-manager version` shows current version and checks for updates
  - TUI displays "Upgrade octez-manager" button when updates are available
  - Background version polling (every 10 minutes) for non-intrusive notifications
  - SHA256 checksum verification for downloaded binaries
  - Smart detection of install method (package, binary, manual) with appropriate upgrade path

- **Binary Management**: Download and manage official Octez binary releases
  - New TUI page accessible via `b` key from the instances screen
  - CLI commands:
    - `binaries list` - Show installed versions and linked directories
    - `binaries download <version>` - Download from GitLab releases
    - `binaries remove <version>` - Remove an installed version
    - `binaries prune` - Remove all unused versions with disk space preview
    - `binaries link <alias> <path>` - Create alias for custom build directory
    - `binaries unlink <alias>` - Remove a linked directory alias
    - `binaries list-remote` - Show available versions from GitLab
  - Progress display during downloads with speed and ETA
  - Disk space calculation and display when pruning
  - Binaries stored in `~/.local/share/octez-manager/binaries/` (user mode) or `/var/lib/octez-manager/binaries/` (system mode)

- **Installer Integration**: Use managed binaries when installing services
  - New flags for all install commands: `--octez-version <version>` and `--bin-dir-alias <alias>`
  - TUI binary selector shows managed versions and linked directories
  - Inline download prompt when selecting an uninstalled version in interactive mode
  - Priority: `--octez-version` > `--bin-dir-alias` > `--app-bin-dir` > auto-detect from PATH

- **Version Notifications**: Get notified when new Octez versions are available
  - Toast notification on TUI startup when newer versions exist
  - Dismissible per-version (won't show again for dismissed versions)
  - Preferences stored in `~/.config/octez-manager/version-check.json`

- **Update Version Action**: Change the binary version used by running services
  - New "Update Version" option in instance action menu
  - Select from managed versions or linked directories
  - Version filtering prevents accidental downgrades

- **Cascade Update and Rollback**: Update services along with their dependents
  - Automatically detects dependent services (bakers/accusers depending on a node)
  - Offers cascade update to update all related services together
  - Finds transitive dependencies (if A depends on B and B depends on C, updating C includes both)
  - Automatic rollback if any service fails to start after update
  - Rollback only restarts services that were running before the update
  - Options on failure: Rollback, View Logs, or Keep Stopped

- **Unmanaged Instances Detection**: Automatically detect Octez services not installed by octez-manager
  - Detects systemd services and standalone processes (Docker, tmux, manual launches)
  - Shows real-time metrics (CPU, memory, sync status, head level) for all detected services
  - Supports Start/Stop/Restart for systemd services, view-only for standalone processes
  - Network detection via RPC probing
  - Appears in dedicated "Unmanaged Instances" section in TUI (below managed services)
  - CLI: `octez-manager list --external` to include unmanaged services

- **Import External Services**: Convert detected external services into managed instances
  - `octez-manager import <service-name>` CLI command with options:
    - `--as <name>`: Custom instance name (default: auto-generated)
    - `--network <network>`: Override network if not detected
    - `--strategy takeover|clone`: Takeover disables original (default), clone keeps it running
    - `--dry-run`: Preview import plan without making changes
    - `--cascade`: Import service and all its dependencies in correct order
    - `--interactive`: Review and edit configuration before import
  - TUI: Select unmanaged service → Enter → "Import to Managed"
  - Validates service before import (checks for required fields, conflicts)
  - Automatic rollback if import fails (re-enables and restarts original service)
  - Preserves existing data directories (no re-sync required)
  - Preserves original service user and file ownership
  - Auto-increments ports for Clone strategy to avoid conflicts
  - **Note**: When using Takeover strategy, verify no other services depend on the imported service

### Changed

- Default network changed from mainnet to shadownet for new installations
- Modal titles are now single-line for better layout consistency

### Fixed

- Import wizard navigation and error handling improvements
- Binary download progress no longer causes display glitches
- Warning now displayed when `$EDITOR` is not set in interactive edit mode
- File browser `h` key now properly toggles hidden files
- Extra arguments preserved correctly during import
- Tallinnnet network detection support
- Version display refreshes immediately after self-upgrade
- `?` help shortcut now works from all screens
- Imported services no longer appear in unmanaged instances list
- Failed systemd services now detected and displayed correctly
- Nonexistent client config path removed from baker/accuser instance details (fixes #483)
- Clean exit without hanging on quit

## [0.1.1] - 2026-01-15

### Added

- One-liner install script (`curl -fsSL ... | sh`)
- Binary accessibility validation for service users
- Tab shortcut hint in TUI for folding sections
- Ubuntu and Debian `.deb` packages in releases

### Fixed

- Hero button text color in dark mode
- Temp directory cleanup on install script error
- Use official Tezos logo

### Changed

- Documentation examples now target shadownet instead of mainnet
- Documentation styling aligned with Tezlink/Tezos design

## [0.1.0] - 2026-01-14

### Added

- Export logs command for diagnostics (`octez-manager instance <name> export-logs`)
- Disk space check before snapshot download
- UI as default command (run `octez-manager` instead of `octez-manager ui`)
- Snapshot integrity verification option
- Auto-generated instance names for all service types
- System monitor graphs for accusers
- Comprehensive documentation with demo GIFs
- Integration test suite with Docker-based testing
- TUI flow tests using headless driver

### Changed

- Documentation now focuses on Shadownet for examples
- Improved form field ordering and validation
- Better snapshot handling with loading states and error recovery
- Filter out full50 snapshots from selection list
- Snapshot download enabled by default for new nodes

### Fixed

- Cache invalidation after editing instances
- Instance naming no longer includes redundant "node-" prefix
- File browser navigation and selection
- Port validation caching to avoid I/O during form render
- Service failure detection and status display
- DAL node uses correct binary (octez-dal-node)
- Dependency tracking when renaming instances
- Form field truncation on narrow terminals

### Documentation

- Added Nomadic Labs and Trilitech credits
- Comprehensive CLI reference with examples
- TUI guide focused on workflows
- Baker setup guide with Octez documentation links
- Dark mode support for logos

## [0.0.2] - 2025-12-19

### Added

- Matrix layout for instances on wide terminals with per-column scrolling
- Active column highlighting in matrix layout
- Instances folded by default for cleaner view
- Debounced validation in validated text modals
- Smart binary path detection for form defaults
- Directory registry with LRU ordering and size limit
- Scheduler tick timing in diagnostics
- Cache module with unit tests

### Changed

- Miaou repository URL updated to trilitech/miaou

### Fixed

- Empty columns handling in matrix layout
- Preserve extra flags when reopening modal
- Kill active download and head monitor curl processes on app exit
- SIGPIPE handling in UI to prevent crash on exit
- Non-blocking cache for form validators during typing
- Head monitor stop() now non-blocking for fast exit
- Use cached services in maybe_refresh to avoid blocking

### Performance

- Optimized polling for hidden instances
- Cached baker config and highwatermarks to avoid I/O in render
- Centralized cache management with non-blocking access

## [0.0.1] - 2025-12-18

Initial release of octez-manager, a terminal UI for managing Octez services.

### Features

- **Service Management**: Install, configure, start, stop, and remove Octez services
  - Node (with snapshot import support)
  - Baker
  - Accuser
  - DAL node
  - Signer

- **Interactive UI**: Terminal-based interface with keyboard navigation
  - Collapsible instance groups
  - Real-time service status monitoring
  - RPC status display for nodes

- **Configuration**: Form-based service configuration
  - Network selection (mainnet, ghostnet, etc.)
  - Custom ports and directories
  - Flag customization with help explorer

- **Monitoring**: Live service health and metrics
  - Systemd service status
  - Node sync status via RPC
  - System metrics (CPU, memory)

- **Wallet Management**: Basic wallet operations via octez-client

### Technical

- Built with OCaml and the Miaou TUI framework
- Systemd integration for service management
- Background job scheduling for long-running operations
- CI workflow with formatting checks
