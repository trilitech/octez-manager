# Changelog

All notable changes to this project will be documented in this file.

The format is based on [Keep a Changelog](https://keepachangelog.com/en/1.1.0/).

## [Unreleased]

### Added

- **Comprehensive Signatory documentation**: Added detailed setup guides covering all aspects of Signatory integration. New documentation includes [Signatory Setup Guide](/guides/signatory-setup/) with backend comparisons (file, YubiHSM, AWS/Azure/GCP KMS), security best practices (key management, network security, access control), troubleshooting procedures, performance tuning, and monitoring guidance. Added [Baker with Signatory Guide](/guides/baker-with-signatory/) with step-by-step baker setup using remote signers, including key generation, Signatory configuration, baker installation, connectivity verification, and production deployment checklist. CLI reference updated with complete `install-signatory` command documentation and examples for all backends. (closes #706)
- **Signatory per-key permission configuration**: Authorized keys in signatory instances can now have individual operation permissions configured. Each key can be restricted to specific operations: `block` (baking), `attestation`, `preattestation`, `attestation_with_dal` (consensus operations), and `generic` (manager operations like transactions). CLI syntax: `--authorized-keys 'tz1abc:block,attestation tz2def:generic'`. TUI install form includes permission configuration when adding keys. Default behavior grants all permissions (maintaining current functionality). This enables security-focused deployments where keys are granted only the minimum required permissions for their role.
- **Baker wallet operations**: New `om baker` command group and TUI wallet modal for managing delegate operations directly from octez-manager. Features include viewing wallet status (balances, staking parameters, pending unstakes, consensus key), staking/unstaking tez, finalizing unstake requests, transferring tez, registering as delegate, setting delegate parameters (staking limit, baking edge), updating consensus key, and governance voting (proposal submission, ballot casting with period-aware behavior). Accessible via "Wallet" action in the TUI instance menu for baker services, or via CLI commands (`om baker <instance> status|register|stake|unstake|finalize-unstake|transfer|set-delegate-params|update-consensus-key|vote`). All operations include fee estimation, confirmation prompts, and JSON output support.
- **Instance groups**: Services can now be organized into logical groups that share configuration (network, binary version, service user). New `om group` CLI with create/list/show/delete/add/remove/start/stop/restart/upgrade subcommands. TUI instances page supports group-based view (toggle with `g` key) showing collapsible group headers with name, network, and binary version. All install forms include a Group field for assigning services to groups at creation time. Group lifecycle operations start/stop services in dependency order. (Implements #335)
- **Keys & Wallet Manager**: Comprehensive key management page with split-panel layout showing grouped keys on the left and rich detail on the right. Features include: enriched key metadata with key kind detection (unencrypted, encrypted, ledger, remote), background balance/delegation/consensus-key fetching every 30s, inline search (`/`), sort modes (`s`: alias/balance/network), force refresh (`r`), key creation with crypto scheme picker (`+`/`n`), watch-only address import, wallet operations (transfer, delegate, undelegate, register as delegate) via action modal (`Enter`), PKH copy to clipboard (`y`/`c`), receive info modal with tzkt explorer link (`Q`), visual multi-select mode (`v`) with batch operations, tzkt alias resolution for known delegates, and transfer MRU persistence. (closes #752, #753, #754, #755, #756, #757, #758, #759, #760, #761, #762, #763, #764, #765, #766, #767)
- **Signatory authorized keys selection**: When adding authorized keys to a Signatory instance, users can now select from existing keys discovered across all base directories. The "Add" button in the authorized keys field offers two options: "Select from existing keys" (shows all keys from ~/.tezos-client and managed base dirs) or "Enter manually" (text input for new keys). This improves UX by eliminating manual copy-paste for keys already managed by octez-client.
- **Theme system with live preview**: New Ctrl+T theme picker with live preview - themes apply instantly as you navigate, Enter confirms, Esc restores original. 13 built-in themes available: dark, light (octez-manager) plus catppuccin-mocha/latte, dracula, nord/nord-light, gruvbox-dark/light, tokyonight/tokyonight-day, opencode, oled (from Miaou 0.4.0). Theme preference persists across sessions. All UI components use semantic themed colors.
- **ppx_forbid**: New compile-time PPX linter that forbids unsafe or deprecated function calls. Project-wide rules ban `Obj`, blocking `Unix.*` process/sleep calls (use Eio equivalents), and `Thread.create`. TUI-specific rules additionally forbid direct `print_*`/`Printf.printf` (corrupts terminal), hardcoded `Widgets.fg`/`Widgets.bg` (use themed helpers), and deprecated `Vsection.render`. Suppressible with `[@allow_forbidden "reason"]`.
- **Signatory health monitoring**: Signatory instances now appear in the instances list with comprehensive health indicators and metrics. The TUI displays service health (healthy/down/degraded/unknown), server address, authorized key count, and backend type. Background scheduler polls every 5 seconds, checking systemd service state, HTTP endpoint reachability (/authorized_keys), and Prometheus metrics from the utility endpoint. Health status is degraded when the service is running but not responding to HTTP requests. Request metrics (total, successful, failed) are parsed from the metrics endpoint when available. (Part of #705)
- **Baker-Signatory key selection UX**: Baker installation wizard now provides interactive key selection when using a Signatory instance. Opening the "Delegates" field shows checkboxes for all keys configured in the selected Signatory's `signatory.yaml`, allowing users to select multiple keys without manual copy-paste. Keys are displayed with their aliases (e.g., "alice (tz1VSUr...)") when available from octez-client directories, making it easier to identify keys visually. Form validation warns if delegate keys are not present in the Signatory's authorized_keys list, preventing runtime failures. The delegates field displays the selected Signatory instance name in the modal title for clarity.
- **Baker remote signer support**: Bakers can now use remote signers (Signatory instances or external URIs) instead of local keys for signing operations. New `--remote-signer-instance` and `--remote-signer-uri` CLI flags for `om install-baker`. TUI install wizard includes remote signer selection modal after delegates configuration. Systemd dependency tracking ensures Signatory instances start before dependent bakers. Supports both managed Signatory instances (with automatic dependency resolution) and external signer URIs. (Part of milestone #7, closes #704)
- **TUI: Install Signatory form**: New installation wizard in the TUI for creating Signatory remote signer instances. Accessible via "Create Service" menu (C key from instances page). Features include File backend selection, keys directory configuration, authorized Tezos keys editor (tz1/tz2/tz3/tz4 validation), HTTP and metrics address configuration, watermark storage options (memory or file), and full edit mode support for modifying existing instances. Integrates with the existing form_builder system for consistent UX across all install wizards. (Part of milestone #7, closes #703)
- **Signatory install command**: New `om install-signatory` command for installing Signatory remote signer services. Supports File backend for key storage with customizable keys directory, address configuration (default 127.0.0.1:6732), metrics endpoint (default 127.0.0.1:9583), and watermark storage options (memory or file). Interactive prompts guide users when flags are omitted. Integrates with managed Octez versions via `--octez-version` flag. (Part of milestone #7, closes #702)
- **Signatory binary download**: TUI Binaries page now supports downloading Signatory binaries alongside Octez. Use the new Octez/Signatory tabs to switch between binary types. Background scheduler fetches available Signatory versions from GitHub releases (>= 1.3.0). CLI commands `om binaries download signatory VERSION` and `om binaries list-remote signatory` provide command-line access. Signatory binaries install to `~/.local/share/octez-manager/signatory-binaries/` with checksum verification and atomic installation. (Part of milestone #7, issue #709)
- **Signatory binary download**: Unified binary management commands now support both Octez and Signatory binaries. Use `om binaries download octez VERSION` or `om binaries download signatory VERSION` to download from official sources. `om binaries list-remote` shows available versions for both types (Octez >= 23.0, Signatory >= 1.3.0). `om binaries remove octez VERSION` or `om binaries remove signatory VERSION` removes installations. Signatory binaries install to `~/.local/share/octez-manager/signatory-binaries/` with checksum verification and atomic installation. (Part of milestone #7, issue #709)
- **Signatory service installer**: New installer module for Signatory remote signer services. Supports File backend for key storage, with address validation (host:port format), Tezos key validation (tz1/tz2/tz3/tz4 prefixes), and YAML config generation. Creates systemd service with secure directory structure and 0o700 permissions for key directories. Supports both Memory and File watermark backends. (Part of milestone #7, issue #701)
- **Mutable pattern detection**: Architecture index now tracks usage of `ref`, `:=`, `!`, `Atomic`, and mutable record fields. New `arch_query mutables` command shows summary of mutable patterns across the codebase. CI blocks PRs that increase `mutable_fields` or `functions_with_mutables` metrics.
- **Network topology page**: Canvas-rendered visualization of service dependency relationships, accessible via 't' key from instances page. Shows nodes as bordered boxes with status indicators, connected by dependency lines. Adapts layout for narrow terminals (vertical stack) and wide terminals (side-by-side roots).

### Changed

- **Signatory service name**: Signatory services now use `signatory@instance` instead of `octez-signatory@instance` as the systemd service name. The "octez-" prefix was redundant since the signatory binary is not part of the Octez project. External service detector now recognizes both naming patterns for backward compatibility.
- **TUI: Signatory install form improvements**: Backend selection now uses modal-based UI (File, YubiHSM, Azure KMS, AWS KMS, GCP KMS, Vault) instead of text input. Form validates signatory binary exists before allowing submission. Watermark field renamed to "Watermark Storage" for clarity. Defaults to latest managed Signatory version when available. Prevents duplicate authorized keys.
- **TUI: Removed redundant "m" global menu**: The global "m" shortcut that opened a duplicate service installation menu has been removed. All service installation now goes through the "c" (Create Service) menu on the instances page, providing a single consistent installation path.
- **Documentation**: Updated prerequisites in installation and baker setup guides to clarify that Octez binaries are no longer required to be manually installed — octez-manager can download and manage them automatically
- Instances page now groups services by role (Nodes, Bakers, Accusers, DAL nodes) with each group wrapped in a Box_widget container with distinct colors
- Diagnostics dashboard now uses Canvas-rendered header with live status indicators (metrics server, recorder, privilege level) and bordered title panel
- Diagnostics dashboard uses Flex_layout for side-by-side panel arrangement: Real-Time Metrics + Metrics Recorder in one row, Metrics Server + System Information in another row, reducing vertical scrolling
- Instance details page now renders service details and file paths sections in Box_widget Rounded borders with distinct colors (service details in color 12, file paths in color 14)
- Instance details page now uses Description_list widget for key-value displays with improved alignment and automatic value wrapping

### Fixed

- **Baker/accuser data_dir with "with local node" mode**: Fixed baker and accuser service creation to use the correct `data_dir` (base directory for keys) instead of the node's blockchain data directory. Previously, when using "with local node" mode, the service's `data_dir` field was incorrectly set to the node's data directory, causing `om list` to show the wrong path and potentially breaking operations that relied on the correct base_dir. The fix ensures baker/accuser services always use their own base_dir for the service `data_dir` field, while still correctly accessing the node's blockchain data when needed.
- **Test: Baker/accuser cascade import with "with local node"**: Fixed integration test infrastructure where `create_external_service` helper was incorrectly using the baker/accuser's data directory for both `--base-dir` and `with local node` syntax. The baker's "with local node" should reference the node's data directory, not the baker's own base directory. This was masking potential bugs in cascade import where baker/accuser services would fail to start with "invalid base-dir" errors. Tests 47, 53, and 68 now correctly pass the node's data directory as a 9th parameter.
- **Import stopped nodes**: Fixed import failure for stopped nodes by reading network configuration from `config.json` when RPC is not accessible. Previously, importing a stopped octez-node or dal-node would fail with "Network could not be detected (RPC not accessible). Please specify --network" even though the network information was available in the node's config.json file. The detector now falls back to reading from config.json for Node and DAL node roles when the network cannot be determined from command-line arguments or RPC probe.
- **Keys page duplicate wallets**: Fixed bug where wallets appeared twice in the key management page (press 'K') when the default `~/.tezos-client` directory was also registered in the directory registry. The page now deduplicates directories before scanning for keys, handling both exact matches and paths with/without trailing slashes.
- **Coverage workflow cache**: Fixed main branch CI failure by including `octez-manager.opam` in the coverage workflow cache key. The workflow was using a stale opam cache that didn't include new dependencies (like yaml), causing "Library not found" build errors. (fixes #742)
- **Signatory config location**: Signatory YAML configuration files (`signatory.yaml`) are now stored alongside keys in `~/.local/share/octez/signatory/<instance>/` instead of split across two directories. Previously, configs were incorrectly placed in `~/.local/share/octez/instances/<instance>/` (or `~/.config/octez/instances/` briefly). This consolidates all signatory-specific files (configs + keys) in one directory, simplifying backup and management. Purging the last signatory instance now also cleans up the empty `signatory/` parent directory. Users with existing signatory instances should move `signatory.yaml` to the signatory data directory.
- **Baker/accuser purge preserves node data**: Purging a baker or accuser instance no longer deletes the node's blockchain data directory. Previously, `om instance purge <baker>` would delete the node's data even though the node instance still existed, causing data loss. The fix detects when a data directory is shared by multiple services and skips deletion, preventing scenarios where purging one baker breaks other services using the same node. (fixes #727)
- **Signatory systemd dependencies**: Baker and accuser services now correctly reference `signatory@instance.service` instead of `octez-signatory@instance.service` in systemd dependencies. Previously, this incorrect naming caused "Unit not found" errors during installation, preventing bakers/accusers from installing when configured to use a signatory. Integration tests added to validate the fix and prevent regression. (closes #707)
- **Signatory key storage**: `secret.json` is now generated as valid JSON (empty array `[]`) instead of with `#` comment instructions that caused JSON parse errors. Instructions moved to separate `secret.json.README` file in the keys directory. Users with existing installations should remove `#` comment lines from their `secret.json` files to avoid "invalid character" errors that prevent Signatory from loading keys.
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
