---
title: Using the TUI
description: The recommended way to install and monitor your Tezos infrastructure
---

The TUI is the recommended way to use Octez Manager. It provides an intuitive interface for installing services, monitoring their status, and managing your infrastructure — no memorizing commands required.

This guide walks through common workflows using Shadownet as an example.

> **Tip:** Press `?` at any time to see available actions for the current screen.

> **Remote access:** For browser-based access, see [Web Interface](/guides/web-interface).

## Getting Started

Launch the TUI:

```bash
# User mode (services run as your user)
octez-manager

# System mode (dedicated service users, production)
sudo octez-manager
```

> **Note:** User mode and system mode are independent. Instances created in one mode are not visible in the other.

## What You Can Do

The TUI provides access to all Octez Manager features:

| Feature | Description | Key |
|---------|-------------|-----|
| **Install services** | Deploy nodes, bakers, accusers, DAL nodes, Signatory, octez-index | `Enter` on "Add new" |
| **Monitor status** | Real-time service status, sync progress, delegate activity | - |
| **View logs** | Live log streaming with search and filtering | `Enter` on instance then select logs |
| **Control services** | Start, stop, restart, and remove instances | `Enter` |
| **Wallets** | View balances, transfer, stake, delegate, manage keys | Tab `2` |
| **Manage binaries** | Download and manage Octez versions | Tab `3` |
| **Diagnostics** | System metrics, service states, background queue | Tab `4` |
| **Topology** | Visual map of service dependencies | Tab `5` |
| **Experimental** | Sandbox environments, Rewards engine | Tab `7` |
| **Themes** | Switch between 13 built-in color themes | `Ctrl+T` |
| **Import services** | Bring external services under management | `Enter` on unmanaged |

## Installing Your First Node

From the main screen, select **[ Install new instance ]** and choose **Node**.

![Install Node](/gifs/install_node.gif)

The installation wizard guides you through:

1. **Instance name** — A unique identifier (e.g., `shadownet`)
2. **Network** — Select `shadownet` for testing
3. **History mode** — `rolling` is recommended for most users
4. **Binary selection** — Choose from managed versions, registered directories, or custom path
5. **RPC/Net addresses** — Keep defaults unless you need specific ports
6. **Bootstrap method** — `Snapshot` downloads pre-built state (faster)

Once installed, the node appears on your dashboard and begins syncing.

## The Main Dashboard

The dashboard shows all your instances organized by type:

- **Nodes** — L1 blockchain nodes
- **Bakers** — Block producers and attesters
- **Accusers** — Double-baking detectors
- **DAL Nodes** — Data Availability Layer nodes
- **Signatory** — Remote signer service
- **octez-index** — TzKT-compatible indexer
- **Unmanaged Instances** — External services detected on your system

Each instance displays:
- Status indicator
- Instance name and network
- Real-time metrics

Select any instance with arrow keys and press `Enter` to see details, or use the action keys shown at the bottom of the screen. Press `g` to toggle grouping mode.

### Upgrade Notification

When a new version of octez-manager is available, an **[ Upgrade octez-manager → vX.X ]** button appears at the top of the dashboard. Select it and press `Enter` to upgrade.

## Adding a Baker

After your node is synced, you can add a baker. Select **[ Install new instance ]** → **Baker**.

![Install Baker](/gifs/install_baker.gif)

The wizard will:
1. Ask which node to connect to (select your Shadownet node)
2. Auto-suggest a name like `baker-shadownet`
3. Prompt for delegate addresses (your baker keys)
4. Configure liquidity baking vote and DAL settings

## Updating Service Versions

To change the Octez version used by a service:

1. Select the instance and press `Enter`
2. Choose **Update Version** from the menu
3. Select a new version from the list (managed versions, registered directories, or custom path)

![Update Version](/gifs/update_version.gif)

## Managing Binaries

Press `3` from the main dashboard to open the **Binaries** page.

![Download Binaries](/gifs/download_binaries.gif)

From here you can:
- **Download** official Octez releases from GitLab
- **Register** custom build directories (e.g., dev builds)
- **Remove** specific versions
- **Prune** all unused versions (shows disk space to be freed)

Downloaded binaries are stored in:
- User mode: `~/.local/share/octez-manager/binaries/`
- System mode: `/var/lib/octez-manager/binaries/`

When installing or editing services, you can select from your managed versions in the binary selection step.

## Viewing Logs

Select an instance and press `Enter`, then choose the logs option:

- Logs stream in real-time (follow mode)
- Press `/` to search for specific text
- Press `t` to switch between journald and daily log files
- Press `w` to toggle line wrapping

## Service Control

From the dashboard, you can control any service:

- **Start/Stop** — Toggle the service state
- **Restart** — Stop and start the service
- **Remove** — Uninstall the service (keeps data by default)

The TUI shows dependent services and handles them automatically. For example, stopping a node will prompt about dependent bakers.

## Unmanaged Instances

Octez Manager automatically detects Octez services running on your system that weren't installed by octez-manager:

- Systemd services (e.g., manually configured octez-node units)
- Standalone processes (Docker containers, tmux sessions, manual launches)

These appear in the **Unmanaged Instances** section at the bottom of the dashboard with real-time metrics.

**For systemd services**, you can:
- View status and metrics
- Start, Stop, Restart
- View logs
- **Import to Managed** — Convert to a managed instance

**For standalone processes**, you can:
- View status and metrics (read-only)

### Importing External Services

To bring an unmanaged systemd service under octez-manager's control:

1. Select the unmanaged instance
2. Press `Enter` to open the action menu
3. Choose **Import to Managed**
4. Follow the import wizard

![Import Wizard](/gifs/import_wizard.gif)

The wizard will:
- Detect the service configuration automatically
- Let you choose an instance name
- Offer **Takeover** (disable original) or **Clone** (keep original running)
- Preview changes before applying

After import, the service appears in your managed instances with full control.

## Installing a Complete Shadownet Setup

Here's a typical workflow for a complete Shadownet baking setup:

1. **Download Octez binaries** (optional but recommended)
   - Press `3` to open Binaries page
   - Download the version you want to use

2. **Install a node**
   - Network: `shadownet`
   - History mode: `rolling`
   - Binary: Select your downloaded version
   - Bootstrap: `Snapshot`

3. **Wait for sync** — The dashboard shows sync progress

4. **Install a DAL node** (optional, for DAL attestations)
   - Connect to your Shadownet node

5. **Install a baker**
   - Connect to your Shadownet node
   - Add your delegate address(es)
   - Optionally connect to your DAL node

6. **Install an accuser** (recommended)
   - Monitors for double-baking

![Install Accuser](/gifs/install_accuser.gif)

## Diagnostics

Press `4` from the main dashboard to open the **Diagnostics** page.

This page shows system-level information useful for troubleshooting:

- **Service states** — Overview of all managed services and their current status
- **Background queue** — Sparkline showing background task queue depth over time
- **System metrics** — CPU and memory usage trends
- **Metrics server** — Address of the Prometheus metrics endpoint (if enabled)

The diagnostics page is read-only and refreshes automatically.

## Keyboard Shortcuts

Press `?` on any page to see the available keyboard shortcuts for the current context.

## Wallets

Press `2` to open the **Wallets** page.

The split-panel layout shows wallet directories on the left and key details on the right:

- **View balances** — Background refresh every 30 seconds
- **Transfer tez** — Send to any address via a local or public node
- **Stake/Unstake** — Manage staking directly from the wallet
- **Delegate** — Set or change delegation
- **Import keys** — Create new keys or import watch-only addresses
- **Search and sort** — Press `/` to search, `s` to cycle sort modes

Wallet directories from all installed services (nodes, bakers, accusers) are discovered automatically.

## Topology

Press `5` to open the **Topology** page.

A visual map of service dependency relationships rendered on a canvas. Nodes appear as bordered boxes with status indicators, connected by dependency lines. The layout adapts for narrow terminals (vertical stack) and wide terminals (side-by-side).

## Themes

Press `Ctrl+T` to open the theme picker. Themes apply instantly as you navigate — press `Enter` to confirm or `Esc` to restore the original. 13 built-in themes are available including dark, light, catppuccin, dracula, nord, gruvbox, tokyonight, and oled. Your preference persists across sessions.

## RPC Browser

Press `r` from the Instances page to open the **RPC Browser** — an interactive explorer for Tezos node RPC endpoints.

Features:
- Navigate the RPC endpoint tree with arrow keys
- Quick access shortcuts (`1`-`5`) for common endpoints like `/version` and `/chains/main/blocks/head`
- JSON syntax highlighting for responses
- Response time and size metrics
- Switch between node instances with `Tab`
- Stream live data from monitor endpoints

See [RPC Browser Guide](/guides/rpc-browser/) for details.
