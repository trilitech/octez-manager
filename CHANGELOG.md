# Changelog

All notable changes to this project will be documented in this file.

The format is based on [Keep a Changelog](https://keepachangelog.com/en/1.1.0/).

## [Unreleased]

### Added

- **Unmanaged Instances Detection**: Automatically detect and manage Octez services not installed by octez-manager
  - Detects systemd services and standalone processes (Docker, tmux, manual launches)
  - Shows real-time metrics (CPU, memory, sync status, head level) for all detected services
  - Supports Start/Stop/Restart for systemd services, view-only for standalone processes
  - Network detection via RPC probing
  - Appears in dedicated "Unmanaged Instances" section in TUI (below managed services)
  - CLI commands: `octez-manager list --external`, `octez-manager external <instance> <action>`
  - Efficient process scanning with pgrep and PID caching for minimal system impact

- **Import External Services**: Convert detected external services into managed instances
  - `octez-manager import <service-name>` CLI command with options:
    - `--as <name>`: Custom instance name (default: auto-generated from detected name)
    - `--network <network>`: Override network if not detected
    - `--strategy takeover|clone`: Takeover disables original (default), clone keeps it running
    - `--dry-run`: Preview import plan with generated file contents
  - Interactive TUI wizard with live progress and error display
  - Validates service before import (checks for required fields, conflicts)
  - Automatic rollback if import fails (re-enables and restarts original service)
  - Preserves existing data directories (no re-sync required)
  - Preserves original service user and file ownership
  - Smart field resolution with confidence tracking
  - Auto-increments ports for Clone strategy to avoid conflicts
  - **Known limitation**: Dependency chains not tracked yet (see #360)
    - Clone strategy: safe, preserves original dependency chain
    - Takeover strategy: may break services that depend on the imported service
    - Manual verification recommended before using Takeover on services with dependents

### Fixed

- Suppress `du` command stderr output to prevent error messages in TUI

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
