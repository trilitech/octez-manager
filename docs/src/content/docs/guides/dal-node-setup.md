---
title: DAL Node Setup
description: Set up a Data Availability Layer node
---

# DAL Node Setup

The Data Availability Layer (DAL) is Tezos's scalability solution. Running a DAL node allows your baker to attest to DAL slots. This guide uses Shadownet as an example.

## Prerequisites

1. **Running Node**: A synced Tezos node (e.g., your Shadownet node)
2. **Baker** (optional): To attest DAL slots, you need to be a baker

## Installation via TUI

![Install DAL Node](/octez-manager/gifs/install_dal_node.gif)

1. Launch `octez-manager`
2. Select **[ Install new instance ]** → **DAL Node**
3. Configure:
   - **Node**: Select your Shadownet node
   - **Instance name**: Auto-suggested as `dal-shadownet`
   - **RPC address**: `127.0.0.1:10732` (default)
   - **Net address**: `0.0.0.0:11732` (default)

> Press `?` at any time to see available actions.

## Installation via CLI

```bash
octez-manager install-dal-node \
  --instance dal-shadownet \
  --node-instance shadownet
```

### Custom Configuration

```bash
octez-manager install-dal-node \
  --instance dal-shadownet \
  --node-instance shadownet \
  --rpc-addr 127.0.0.1:10732 \
  --net-addr 0.0.0.0:11732
```

### Using Remote Node Endpoint

```bash
octez-manager install-dal-node \
  --instance dal-shadownet \
  --node-instance http://localhost:8732
```

## Connecting Baker to DAL Node

After setting up your DAL node, connect your baker to enable DAL attestations.

### Edit Existing Baker

In the TUI, select your baker and press `e` to edit, then select your DAL node.

Via CLI:
```bash
octez-manager instance baker-shadownet edit
```

### During Baker Installation

```bash
octez-manager install-baker \
  --instance baker-shadownet \
  --node-instance shadownet \
  --delegate tz1... \
  --dal-endpoint dal-shadownet \
  --liquidity-baking-vote pass
```

## Ports

| Port | Protocol | Purpose |
|------|----------|---------|
| 10732 | HTTP | RPC interface |
| 11732 | TCP | P2P network |

Ensure these ports are accessible for DAL network participation.

## Monitoring

### Check Status

```bash
octez-manager instance dal-shadownet show
```

### View Logs

```bash
# TUI: select DAL node, press 'l'
# CLI:
octez-manager instance dal-shadownet logs
```

### Check DAL Node RPC

```bash
curl -s http://127.0.0.1:10732/health
```

## Troubleshooting

### DAL node not syncing

1. Verify the L1 node is fully synced
2. Check network connectivity on port 11732
3. Review logs for peer connection issues

### Baker not attesting DAL slots

1. Verify DAL node is running and synced
2. Check baker configuration includes DAL node
3. Restart baker after DAL node configuration changes
