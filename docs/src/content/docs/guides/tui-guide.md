---
title: Using the TUI
description: The recommended way to install and monitor your Tezos infrastructure
---

The TUI is the recommended way to use Octez Manager. It provides an intuitive interface for installing services, monitoring their status, and managing your infrastructure — no memorizing commands required.

This guide walks through common workflows using Shadownet as an example.

> **Tip:** Press `?` at any time to see available actions for the current screen.

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
| **Install services** | Deploy nodes, bakers, accusers, and DAL nodes | `Enter` on Install |
| **Monitor status** | Real-time service status, sync progress, delegate activity | - |
| **View logs** | Live log streaming with search and filtering | `l` |
| **Edit configuration** | Modify instance settings without redeploying | `e` |
| **Control services** | Start, stop, restart, and remove instances | `Enter` |
| **Manage binaries** | Download and manage Octez versions | `b` |
| **Import services** | Bring external services under management | `Enter` on unmanaged |

## Installing Your First Node

From the main screen, select **[ Install new instance ]** and choose **Node**.

![Install Node](/gifs/install_node.gif)

The installation wizard guides you through:

1. **Instance name** — A unique identifier (e.g., `shadownet`)
2. **Network** — Select `shadownet` for testing
3. **History mode** — `rolling` is recommended for most users
4. **Binary selection** — Choose from managed versions, linked directories, or custom path
5. **RPC/Net addresses** — Keep defaults unless you need specific ports
6. **Bootstrap method** — `Snapshot` downloads pre-built state (faster)

Once installed, the node appears on your dashboard and begins syncing.

## The Main Dashboard

The dashboard shows all your instances organized by type:

- **Nodes** — L1 blockchain nodes
- **Bakers** — Block producers and attesters
- **Accusers** — Double-baking detectors
- **DAL Nodes** — Data Availability Layer nodes
- **Unmanaged Instances** — External services detected on your system

Each instance displays:
- Status indicator (`●` running, `○` stopped, `!` failed)
- Instance name and network
- Real-time metrics (sync status, memory, signing activity)

Select any instance with arrow keys and press `Enter` to see details, or use the action keys shown at the bottom of the screen.

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

## Editing an Instance

Need to change delegates or other settings? Select the instance and press `e` to edit.

![Edit Baker](/gifs/edit_baker.gif)

Changes take effect after saving. The TUI will restart the service if needed.

## Updating Service Versions

To change the Octez version used by a service:

1. Select the instance and press `Enter`
2. Choose **Update Version** from the menu
3. Select a new version from the list (managed versions, linked directories, or custom path)

If the service has dependents (e.g., bakers depending on a node), you'll be offered a **cascade update** to update all related services together.

If the update fails to start, you can:
- **Rollback** — Restore the previous version and restart
- **View Logs** — Diagnose the issue
- **Keep Stopped** — Leave for manual intervention

## Managing Binaries

Press `b` from the main dashboard to open the **Binaries** page.

From here you can:
- **Download** official Octez releases from GitLab
- **Link** custom build directories (e.g., dev builds)
- **Remove** specific versions
- **Prune** all unused versions (shows disk space to be freed)

Downloaded binaries are stored in:
- User mode: `~/.local/share/octez-manager/binaries/`
- System mode: `/var/lib/octez-manager/binaries/`

When installing or editing services, you can select from your managed versions in the binary selection step.

## Viewing Logs

Select an instance and press `l` to open the log viewer:

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

The wizard will:
- Detect the service configuration automatically
- Let you choose an instance name
- Offer **Takeover** (disable original) or **Clone** (keep original running)
- Preview changes before applying

After import, the service appears in your managed instances with full control.

## Installing a Complete Shadownet Setup

Here's a typical workflow for a complete Shadownet baking setup:

1. **Download Octez binaries** (optional but recommended)
   - Press `b` to open Binaries page
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

Press `d` from the main dashboard to open the **Diagnostics** page.

This page shows system-level information useful for troubleshooting:

- **Service states** — Overview of all managed services and their current status
- **Background queue** — Sparkline showing background task queue depth over time
- **System metrics** — CPU and memory usage trends
- **Metrics server** — Address of the Prometheus metrics endpoint (if enabled)

The diagnostics page is read-only and refreshes automatically.

## Keyboard Shortcuts

| Key | Action |
|-----|--------|
| `↑`/`↓` | Navigate list |
| `Enter` | Select / Open action menu |
| `Tab` | Fold/unfold instance details |
| `b` | Open Binaries page |
| `e` | Edit selected instance |
| `l` | View logs for selected instance |
| `d` | Open Diagnostics page |
| `?` | Show help |
| `Esc` | Go back / Close modal |
| `q` | Quit |

## Tips

- **Use `?` for help** — Every screen has context-specific help
- **Watch the hints** — The bottom bar shows available actions
- **Check metrics** — Expanded instances show CPU, memory, and sync status
- **Monitor bakers** — Baker instances show signing activity, missed slots, and DAL participation
- **Keep binaries updated** — Use the Binaries page to download new Octez versions before updating services
- **Check for updates** — The upgrade button appears when new octez-manager versions are available
