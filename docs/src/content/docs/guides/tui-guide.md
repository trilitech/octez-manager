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

## What You Can Do

The TUI provides access to all Octez Manager features:

| Feature | Description |
|---------|-------------|
| **Install services** | Deploy nodes, bakers, accusers, and DAL nodes |
| **Monitor status** | Real-time service status, sync progress, delegate activity |
| **View logs** | Live log streaming with search and filtering |
| **Edit configuration** | Modify instance settings without redeploying |
| **Control services** | Start, stop, restart, and remove instances |

## Installing Your First Node

From the main screen, select **[ Install new instance ]** and choose **Node**.

![Install Node](/gifs/install_node.gif)

The installation wizard guides you through:

1. **Instance name** — A unique identifier (e.g., `shadownet`)
2. **Network** — Select `shadownet` for testing
3. **History mode** — `rolling` is recommended for most users
4. **RPC/Net addresses** — Keep defaults unless you need specific ports
5. **Bootstrap method** — `Snapshot` downloads pre-built state (faster)

Once installed, the node appears on your dashboard and begins syncing.

## The Main Dashboard

The dashboard shows all your instances organized by type:

- **Nodes** — L1 blockchain nodes
- **Bakers** — Block producers and attesters
- **Accusers** — Double-baking detectors
- **DAL Nodes** — Data Availability Layer nodes

Each instance displays:
- Status indicator (`●` running, `○` stopped, `!` failed)
- Instance name and network
- Real-time metrics (sync status, memory, signing activity)

Select any instance with arrow keys and press `Enter` to see details, or use the action keys shown at the bottom of the screen.

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

## Installing a Complete Shadownet Setup

Here's a typical workflow for a complete Shadownet baking setup:

1. **Install a node**
   - Network: `shadownet`
   - History mode: `rolling`
   - Bootstrap: `Snapshot`

2. **Wait for sync** — The dashboard shows sync progress

3. **Install a DAL node** (optional, for DAL attestations)
   - Connect to your Shadownet node

4. **Install a baker**
   - Connect to your Shadownet node
   - Add your delegate address(es)
   - Optionally connect to your DAL node

5. **Install an accuser** (recommended)
   - Monitors for double-baking

![Install Accuser](/gifs/install_accuser.gif)

## Tips

- **Use `?` for help** — Every screen has context-specific help
- **Watch the hints** — The bottom bar shows available actions
- **Check metrics** — Expanded instances show CPU, memory, and sync status
- **Monitor bakers** — Baker instances show signing activity, missed slots, and DAL participation
