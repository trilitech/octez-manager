# Changelog

All notable changes to this project will be documented in this file.

The format is based on [Keep a Changelog](https://keepachangelog.com/en/1.1.0/).

## [Unreleased]

### Added

- **Local indexer compare mode**: `om ui --local-indexer <URL>` registers a local TzKT-compatible indexer as the preferred source for all rewards and delegation queries. Add `--compare-indexers` to simultaneously query public TzKT and log any divergences, useful for validating a self-hosted indexer. The `--indexer-network` flag (default: `mainnet`) identifies which network the local indexer serves.
- **`rewards continual tick` command**: One-shot command that checks all baker instances with continual mode enabled and dispatches payouts for any cycles that are due. Intended for use with external schedulers (cron, systemd timers). When `rewards continual start` is called, octez-manager optionally installs a systemd timer to call `tick` automatically.

### Fixed

- **Version reported by `--version`**: `octez-manager --version` now reports the version from the dune build system rather than a hardcoded string. In development builds (not installed via opam), it reports `dev`.

- **Rewards non-mainnet TzKT routing**: Fixed incorrect TzKT API base URL used when generating or paying rewards for non-mainnet bakers. Network names that are full URLs (e.g. from Teztnets picker) are now normalized to slugs before constructing the TzKT endpoint. The continual scheduler and payout executor now correctly resolve the baker's base directory from environment variables (`OCTEZ_CLIENT_BASE_DIR`, `OCTEZ_BAKER_BASE_DIR`).

- **`om list --internal` flag**: New flag to list only managed (internal) services, complementing the existing `--external` flag. While `om list` shows managed services by default, `--all` shows both, and `--external` shows only external services, the new `--internal` flag explicitly lists only managed services without external service detection. Useful for scripting, automation, and situations where you want to ensure you're only working with octez-manager-controlled services.
- **Tab-based main navigation shell**: The app now opens with a five-tab navigation bar (Instances, Wallets, Diagnostics, Topology, Sandboxes) at the top. Switch tabs with number keys `1`–`5`, or use the existing shortcuts `K` (Wallets), `d` (Diagnostics), `t` (Topology). Each tab preserves its state (cursor position, fold state, scroll offset) across switches — returning to a tab shows exactly what you left.
- **Instances page: visual action buttons**: The three action buttons (Install new instance, Manage binaries, Browse RPCs) in the Instances button bar are now rendered as focusable `Button_widget` entries with highlighted selection, replacing the plain `[ Label ]` text style.
- **Instances page: view-mode toggle row**: A visible "View: (X) By Role  ( ) By Group" radio row now appears below the button bar, showing the current view mode at a glance. Switch modes with `←`/`→` or `h`/`l` when the cursor is on the row, or continue using `g` anywhere on the page.
- **Instances page: contextual help footer**: The footer now shows context-sensitive hints that change as you move through different zones (button bar, service list, service with failure). The "?" key opens a richer per-service markdown help modal.

### Changed

- **Keys page renamed to Wallets**: The Keys management page is now called "Wallets" throughout the UI. Press `K` to open it. The tab label reads "Wallets".
- **Instances page: Manage Sandboxes button removed**: The "Manage sandboxes" button has been removed from the Instances button bar. Sandboxes are now accessible via the dedicated Sandboxes tab (`5` key).
- **Sandbox mode**: Create isolated Tezos sandbox environments for local testing and development. A sandbox spins up a complete node + baker pair using yes-crypto (any secret key can sign for any public key), automatically generating a yes-wallet with the top N active delegates from the network. TUI sandbox page (`sandbox` in navigation) lists all sandboxes with node/baker status, head level, and detail panel. Key bindings: `c` create, `s`/`S` start/stop, `d` destroy (with confirmation), `a` add account, `r` open RPC browser. CLI commands: `om sandbox create|list|status|start|stop|destroy|add-account`. Instance `set-env`/`get-env` commands allow setting arbitrary environment variables on any managed service instance.
- **Rewards & Payouts engine**: Built-in reward distribution system for Tezos bakers, replacing the need for external tools like third-party payout tools. TUI dashboard with 4 tabs (Overview, Delegators, History, Configuration) accessible from the main menu. Features include: cycle rewards fetching from TzKT API, proportional reward calculation with per-delegator fee overrides, overdelegation protection (9x cap), payout preview generation, real payout execution via octez-client transfers, dry-run simulation with wallet balance checking, CSV/JSON report writing (standard format), double-payment prevention via file locks, external config.hjson import with HJSON parser, notification dispatch (Discord webhooks, Telegram Bot API, generic webhooks, external scripts), and continual mode for automatic payouts on cycle transitions with configurable random delay and multi-cycle intervals. CLI commands: `om rewards status|generate|history|pay|config import|notify test|continual start/stop/status`.
- **Signatory remote signer support**: Complete integration for managing Signatory remote signer instances. Install and configure Signatory services via CLI (`om install-signatory`) or TUI wizard. Download Signatory binaries from official GitHub releases. Configure per-key operation permissions (block, attestation, preattestation, generic) for fine-grained access control. Bakers can use Signatory instances or external signer URIs instead of local keys, with automatic systemd dependency management. Comprehensive documentation includes setup guides, backend comparisons (File, YubiHSM, AWS/Azure/GCP KMS), security best practices, and baker integration workflows. Health monitoring displays service status, authorized key count, backend type, and request metrics. (closes #702, #703, #704, #705, #706, #709)
- **Baker wallet operations**: New `om baker` command group and TUI wallet modal for managing delegate operations directly from octez-manager. Features include viewing wallet status (balances, staking parameters, pending unstakes, consensus key), staking/unstaking tez, finalizing unstake requests, transferring tez, registering as delegate, setting delegate parameters (staking limit, baking edge), updating consensus key, and governance voting (proposal submission, ballot casting with period-aware behavior). Accessible via "Wallet" action in the TUI instance menu for baker services, or via CLI commands (`om baker <instance> status|register|stake|unstake|finalize-unstake|transfer|set-delegate-params|update-consensus-key|vote`). All operations include fee estimation, confirmation prompts, and JSON output support.
- **Instance groups**: Services can now be organized into logical groups that share configuration (network, binary version, service user). New `om group` CLI with create/list/show/delete/add/remove/start/stop/restart/upgrade subcommands. TUI instances page supports group-based view (toggle with `g` key) showing collapsible group headers with name, network, and binary version. All install forms include a Group field for assigning services to groups at creation time. Group lifecycle operations start/stop services in dependency order. (closes #335)
- **Keys & Wallet Manager**: Comprehensive key management page with split-panel layout showing grouped keys on the left and rich detail on the right. Features include: enriched key metadata with key kind detection (unencrypted, encrypted, ledger, remote), background balance/delegation/consensus-key fetching every 30s, inline search (`/`), sort modes (`s`: alias/balance/network), force refresh (`r`), key creation with crypto scheme picker (`+`/`n`), watch-only address import, wallet operations (transfer, delegate, undelegate, register as delegate) via action modal (`Enter`), PKH copy to clipboard (`y`/`c`), receive info modal with tzkt explorer link (`Q`), visual multi-select mode (`v`) with batch operations, tzkt alias resolution for known delegates, and transfer MRU persistence. (closes #752, #753, #754, #755, #756, #757, #758, #759, #760, #761, #762, #763, #764, #765, #766, #767)
- **Theme system with live preview**: New Ctrl+T theme picker with live preview - themes apply instantly as you navigate, Enter confirms, Esc restores original. 13 built-in themes available: dark, light (octez-manager) plus catppuccin-mocha/latte, dracula, nord/nord-light, gruvbox-dark/light, tokyonight/tokyonight-day, opencode, oled (from Miaou 0.4.0). Theme preference persists across sessions. All UI components use semantic themed colors.
- **ppx_forbid**: New compile-time PPX linter that forbids unsafe or deprecated function calls. Project-wide rules ban `Obj`, blocking `Unix.*` process/sleep calls (use Eio equivalents), and `Thread.create`. TUI-specific rules additionally forbid direct `print_*`/`Printf.printf` (corrupts terminal), hardcoded `Widgets.fg`/`Widgets.bg` (use themed helpers), and deprecated `Vsection.render`. Suppressible with `[@allow_forbidden "reason"]`.
- **Mutable pattern detection**: Architecture index now tracks usage of `ref`, `:=`, `!`, `Atomic`, and mutable record fields. New `arch_query mutables` command shows summary of mutable patterns across the codebase. CI blocks PRs that increase `mutable_fields` or `functions_with_mutables` metrics.
- **Network topology page**: Canvas-rendered visualization of service dependency relationships, accessible via 't' key from instances page. Shows nodes as bordered boxes with status indicators, connected by dependency lines. Adapts layout for narrow terminals (vertical stack) and wide terminals (side-by-side roots).

### Changed

- **Documentation**: Updated prerequisites in installation and baker setup guides to clarify that Octez binaries are no longer required to be manually installed — octez-manager can download and manage them automatically
- **TUI: Removed redundant "m" global menu**: The global "m" shortcut that opened a duplicate service installation menu has been removed. All service installation now goes through the "c" (Create Service) menu on the instances page, providing a single consistent installation path.
- Instances page now groups services by role (Nodes, Bakers, Accusers, DAL nodes, Signatory) with each group wrapped in a Box_widget container with distinct colors
- Diagnostics dashboard now uses Canvas-rendered header with live status indicators (metrics server, recorder, privilege level) and bordered title panel
- Diagnostics dashboard uses Flex_layout for side-by-side panel arrangement: Real-Time Metrics + Metrics Recorder in one row, Metrics Server + System Information in another row, reducing vertical scrolling
- Instance details page now renders service details and file paths sections in Box_widget Rounded borders with distinct colors (service details in color 12, file paths in color 14)
- Instance details page now uses Description_list widget for key-value displays with improved alignment and automatic value wrapping

### Fixed

- **Pre-commit hook switch selection**: The pre-commit hook now always uses the project's local OCaml 5.3 switch instead of the shell's active switch. Previously the `opam env` fallback could pick up the global `octez-setup` switch (OCaml 5.2.1), causing `arch_index.ml` compilation failures because it requires the OCaml 5.3 Typedtree API.
- **External baker/accuser command structure**: Integration test helper `create_external_service` now generates correct octez-baker command syntax with global flags (`--endpoint`, `--base-dir`) placed BEFORE the `run` subcommand instead of after. The incorrect order caused external baker and accuser services to crash-loop with "Unrecognized command" errors. Also adds missing `--base-dir` parameter to accuser services.
- **Import with environment variables**: Import command now properly expands shell variables in systemd ExecStart commands when EnvironmentFile is configured. Previously, importing a service with `--base-dir "${OCTEZ_BAKER_BASE_DIR}"` would fail with "Required field 'base_dir' is missing" even when the environment file existed and contained the variable. The fix includes: improved EnvironmentFile property parsing (handles space/semicolon/newline separators and optional files with `-` prefix), variable detection before marking fields as "detected", proper logging for environment file read operations (debug for success, warn for failures), and validation that variable expansion actually succeeded before marking fields as "known". Fields with unexpanded variables are now correctly marked as "Unknown" instead of "Detected", allowing the import command to properly report missing required fields and guide users to provide overrides or fix environment file issues.
- **Cascade import DAL connection loss**: Fixed bug where baker lost its DAL node connection during cascade import (`--cascade` flag). When importing a node with cascade enabled, dependent services (baker, accuser, dal-node) are imported in topological order. The baker import correctly detected the DAL dependency and set `dal_config = Dal_auto`, but failed to resolve the actual DAL endpoint address during installation. This caused the baker to start without the `--dal-node` flag, resulting in "Please connect a running DAL node using '--dal-node <endpoint>'" errors. The fix resolves the DAL instance's RPC address from the service registry when `dal_config = Dal_auto` and sets the proper endpoint in the `OCTEZ_DAL_CONFIG` environment variable, ensuring the baker connects to the DAL node on startup.
- **Import stopped nodes**: Network detection now reads from `config.json` for stopped nodes when RPC is not accessible. Previously, importing a stopped node without `--network` flag would fail with "Network could not be detected (RPC not accessible)" even though the network could be inferred from the node's configuration file. The import process now checks `config.json` first (faster, works for stopped nodes), then falls back to RPC probe if the service is active.
- **Baker/accuser purge preserves node data**: Purging a baker or accuser instance no longer deletes the node's blockchain data directory. Previously, `om instance purge <baker>` would delete the node's data even though the node instance still existed, causing data loss. The fix detects when a data directory is shared by multiple services and skips deletion, preventing scenarios where purging one baker breaks other services using the same node. (fixes #727)
- **Coverage workflow cache**: Fixed main branch CI failure by including `octez-manager.opam` in the coverage workflow cache key. The workflow was using a stale opam cache that didn't include new dependencies (like yaml), causing "Library not found" build errors. (fixes #742)
- **RPC Browser responsiveness**: HTTP requests and endpoint listing now run in background worker pool, preventing UI freezes during slow network responses (fixes #673)

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
