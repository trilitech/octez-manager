---
title: Becoming a Baker
description: Set up a Tezos baker with Octez Manager
---

# Becoming a Baker

This guide covers setting up a baker to participate in Tezos consensus. We'll use Shadownet as an example.

## Prerequisites

1. **Running Node**: A synced Tezos node (e.g., your Shadownet node)
2. **Staked Tez**: Minimum 6,000 tez staked to your baker address (use the faucet on testnets)
3. **Baker Key**: Your baker's secret key imported into the client

## Import Your Baker Key

Before setting up the baker, import your key:

```bash
octez-client import secret key my-baker unencrypted:edsk...
```

Or use a ledger:
```bash
octez-client import secret key my-baker "ledger://..."
```

## Installation via TUI

![Install Baker](/octez-manager/gifs/install_baker.gif)

1. Launch `octez-manager`
2. Select **[ Install new instance ]** → **Baker**
3. Configure:
   - **Node**: Select your Shadownet node
   - **Instance name**: Auto-suggested as `baker-shadownet`
   - **Delegates**: Your baker address(es)
   - **Liquidity baking vote**: `on`, `off`, or `pass`
   - **DAL node**: Optional, for DAL attestations

> Press `?` at any time to see available actions.

## Installation via CLI

```bash
octez-manager install-baker \
  --instance baker-shadownet \
  --node-instance shadownet \
  --delegate tz1... \
  --liquidity-baking-vote pass
```

### Multiple Delegates

```bash
octez-manager install-baker \
  --instance multi-baker \
  --node-instance my-node \
  --delegate tz1abc... \
  --delegate tz1def... \
  --delegate tz1ghi...
```

### With DAL Attestations

```bash
octez-manager install-baker \
  --instance my-baker \
  --node-instance my-node \
  --delegate tz1... \
  --dal-endpoint my-dal-node
```

## Liquidity Baking Vote

| Option | Description |
|--------|-------------|
| `on` | Vote to continue liquidity baking |
| `off` | Vote to stop liquidity baking |
| `pass` | Abstain from voting |

## Monitoring

### Check Baker Status

```bash
octez-manager instance my-baker show
```

### View Baking Rights

```bash
octez-client get baking rights for tz1... --cycle <current_cycle>
```

### View Logs

```bash
# TUI: select baker, press 'l'
# CLI:
octez-manager instance my-baker logs
```

## Security Considerations

- **Key Management**: Consider using a remote signer or Ledger
- **Firewall**: Only expose necessary ports
- **Monitoring**: Set up alerts for missed blocks/attestations
- **Redundancy**: Have a backup baker ready (but not running simultaneously)

## Troubleshooting

### Baker not producing blocks

1. Check node is synced
2. Verify baker key is properly imported
3. Check you have baking rights for current cycle
4. Review baker logs for errors

### Missed attestations

1. Check network latency to node
2. Ensure sufficient system resources
3. Consider using a closer/faster node
