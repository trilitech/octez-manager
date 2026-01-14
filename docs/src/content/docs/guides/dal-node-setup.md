---
title: DAL Node Setup
description: Set up a Data Availability Layer node
---

# DAL Node Setup

The Data Availability Layer (DAL) is Tezos's scalability solution. Running a DAL node allows your baker to attest to DAL slots.

## Prerequisites

1. **Running Node**: A synced Tezos node
2. **Baker** (optional): To attest DAL slots, you need to be a baker

## Installation via TUI

1. Launch `octez-manager`
2. Press `i` → Select **DAL Node**
3. Configure:
   - **Instance name**: `my-dal-node`
   - **Node**: Select your local node or enter endpoint
   - **RPC address**: `127.0.0.1:10732` (default)
   - **Net address**: `0.0.0.0:11732` (default)

## Installation via CLI

```bash
# Using local node
octez-manager install-dal-node \
  --instance my-dal-node \
  --node-instance my-node \
  --rpc-addr 127.0.0.1:10732 \
  --net-addr 0.0.0.0:11732

# Using remote endpoint
octez-manager install-dal-node \
  --instance my-dal-node \
  --node-instance http://localhost:8732 \
  --rpc-addr 127.0.0.1:10732
```

## Connecting Baker to DAL Node

After setting up your DAL node, connect your baker by editing its configuration:

```bash
# Edit existing baker configuration
octez-manager instance my-baker edit
```

Or specify during baker installation:
```bash
octez-manager install-baker \
  --instance my-baker \
  --node-instance my-node \
  --delegate tz1... \
  --dal-endpoint my-dal-node \
  --liquidity-baking-vote pass
```

## Ports

| Port | Protocol | Purpose |
|------|----------|---------|
| 10732 | HTTP | RPC interface |
| 11732 | TCP | P2P network |

Ensure these ports are accessible for DAL network participation.

## Monitoring

```bash
# Check status
octez-manager instance my-dal-node show

# View logs
octez-manager instance my-dal-node logs

# Check DAL node RPC
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
