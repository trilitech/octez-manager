---
title: Quick Start
description: Get a Tezos node running in 5 minutes
---

# Quick Start

Get a Tezos node running in 5 minutes using the interactive TUI.

## Launch the TUI

```bash
octez-manager
```

You'll see the main dashboard showing your instances (initially empty).

## Install a Node

1. Press `i` to open the install menu
2. Select **Node** from the list
3. Fill in the form:
   - **Instance name**: `my-node` (or any name you prefer)
   - **Network**: Select `mainnet` or `ghostnet`
   - **History mode**: `rolling` (recommended) or `full`
   - **Bootstrap**: `Snapshot` (faster) or `Genesis` (from scratch)

4. Press `Enter` to submit

The installer will:
- Create the data directory
- Download and import a snapshot (if selected)
- Configure the systemd service
- Start the node

## Monitor Your Node

Once installed, your node appears on the dashboard. You can:

- Press `Enter` on the instance to see details
- Press `l` to view logs
- Press `s` to start/stop the service

## CLI Alternative

You can also install via CLI:

```bash
# Interactive mode
octez-manager install-node

# With flags
octez-manager install-node \
  --instance my-node \
  --network mainnet \
  --history-mode rolling \
  --bootstrap snapshot
```

## Keyboard Shortcuts

| Key | Action |
|-----|--------|
| `i` | Install new instance |
| `Enter` | View instance details |
| `l` | View logs |
| `s` | Start/stop service |
| `r` | Restart service |
| `d` | Delete instance |
| `q` | Quit |
| `?` | Help |

## Next Steps

- [Detailed Node Setup Guide](/octez-manager/guides/node-setup)
- [Becoming a Baker](/octez-manager/guides/baker-setup)
- [TUI Guide](/octez-manager/guides/tui-guide)
