# Changelog

All notable changes to this project will be documented in this file.

The format is based on [Keep a Changelog](https://keepachangelog.com/en/1.1.0/).

## [Unreleased]

## [0.2.0] - 2026-01-23

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
  - User preferences stored in `~/.config/octez-manager/version-check.json`
  - Smart semantic version comparison (handles RC versions, multi-digit, padding)

- **Update Version Action**: Change the binary version used by running services
  - New "Update Version" option in instance action menu
  - Select from managed versions or linked directories
  - Version filtering prevents accidental downgrades
  - Extracts version from non-managed binaries for comparison

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
  - Efficient process scanning with pgrep and PID caching for minimal system impact

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

- **Graceful Shutdown**: Background schedulers now shut down cleanly on exit
  - All background workers (RPC, delegate, system metrics, external services) stop properly
  - Prevents orphaned threads and resource leaks

### Changed

- Default network changed from mainnet to shadownet for new installations
- Modal titles are now single-line for better layout consistency

### Fixed

- Warning displayed when `$EDITOR` is not set in interactive edit mode
- Multiple import wizard navigation and state management issues
- Binary downloader stderr output no longer pollutes TUI display
- External services detection filters out child processes (validator, validator-hypervisor)
- Process scanner uses `/proc/PID/exe` for reliable binary path detection
- `du` command stderr output suppressed to prevent TUI error messages
- File browser `h` key now properly toggles hidden files
- Extra arguments use space-separated format instead of `=` syntax for compatibility

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
