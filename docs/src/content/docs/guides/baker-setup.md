---
title: Becoming a Baker
description: Set up a Tezos baker with Octez Manager
---

# Becoming a Baker

This guide covers setting up a baker with Octez Manager. We'll use Shadownet as an example.

For detailed information about baking on Tezos, see the [Octez documentation on running a delegate](https://octez.tezos.com/docs/introduction/howtorun.html#running-a-delegate).

## Prerequisites

1. **Octez Binaries**: Install the Octez suite (`octez-client`, `octez-baker`, etc.). See [How to get Octez](https://octez.tezos.com/docs/introduction/howtoget.html) for installation options.
2. **Running Node**: A synced Tezos node (e.g., your Shadownet node)
3. **Staked Tez**: Minimum 6,000 tez staked to your baker address (use the faucet on testnets)
4. **Baker Key**: Your baker's secret key imported into the client

## Set Up Baker Directory

Before installing the baker with Octez Manager, you need to create a base directory and import your baker key. This directory holds your baker's configuration and keys.

> **Note:** Future versions of Octez Manager may automate this setup.

```bash
# Create a base directory for your baker
mkdir -p ~/.tezos-client

# Import your key (see Octez docs for details)
octez-client import secret key my-baker unencrypted:edsk...
```

Or use a Ledger hardware wallet:
```bash
octez-client import secret key my-baker "ledger://..."
```

See the [Octez documentation](https://octez.tezos.com/docs/introduction/howtorun.html#running-a-delegate) for detailed instructions on key management and delegate setup.

## Installation via TUI

![Install Baker](/octez-manager/gifs/install_baker.gif)

1. Launch `octez-manager`
2. Select **[ Install new instance ]** → **Baker**
3. Configure:
   - **Node**: Select your Shadownet node
   - **Instance name**: Auto-suggested as `baker-shadownet`
   - **Delegates**: Your baker address(es)
   - **Liquidity baking vote**: `on`, `off`, or `pass`
   - **DAL node**: For DAL attestations

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

- **Key Management**: Consider using a [remote signer](https://octez.tezos.com/docs/user/key-management.html) or Ledger hardware wallet
- **Firewall**: Only expose necessary ports
- **Monitoring**: Set up alerts for missed blocks/attestations

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
