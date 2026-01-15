---
title: Setting Up a Node
description: Complete guide to setting up a Tezos node
---

This guide walks you through setting up a Tezos node with Octez Manager.

## Planning Your Setup

### Network Selection

| Network | Purpose | Recommended For |
|---------|---------|-----------------|
| **Shadownet** | Testnet mirroring mainnet | Testing with mainnet-like conditions |
| **Ghostnet** | Permanent testnet | Development, staging |
| **Mainnet** | Production network | Not recommended yet (see below) |

> **Tip:** Start with Shadownet for testing — it mirrors mainnet conditions. For faster iteration, consider a protocolnet which has shorter block times.

> **Warning:** Octez Manager is experimental software. We recommend using it only on testnets (Shadownet, Ghostnet) until it matures. Mainnet usage is at your own risk.

### History Mode

| Mode | Storage | Use Case |
|------|---------|----------|
| **Rolling** | ~50GB | Most users, bakers |
| **Full** | ~500GB+ | Block explorers, indexers |
| **Archive** | 4TB+ | Complete history access |

### Bootstrap Method

| Method | Time | Description |
|--------|------|-------------|
| **Snapshot** | ~30 min | Download pre-built state (recommended) |
| **Genesis** | Days | Sync from scratch |

## Installation via TUI

![Install Node](/octez-manager/gifs/install_node.gif)

1. Launch `octez-manager`
2. Select **[ Install new instance ]** → **Node**
3. Configure:
   - **Instance name**: Unique identifier (e.g., `shadownet`)
   - **Network**: `shadownet` (or `mainnet`/`ghostnet`)
   - **History mode**: `rolling` (recommended)
   - **RPC address**: `127.0.0.1:8732` (default)
   - **Net address**: `0.0.0.0:9732` (default)
   - **Bootstrap**: `Snapshot` (recommended)

> Press `?` at any time to see available actions.

## Installation via CLI

```bash
octez-manager install-node \
  --instance shadownet \
  --network shadownet \
  --history-mode rolling \
  --snapshot
```

### Custom Snapshot URL

```bash
octez-manager install-node \
  --instance my-node \
  --network mainnet \
  --snapshot \
  --snapshot-uri https://example.com/snapshot.rolling
```

### Custom Data Directory

```bash
octez-manager install-node \
  --instance my-node \
  --network mainnet \
  --data-dir /mnt/fast-ssd/tezos-node
```

## Post-Installation

### Check Status

```bash
# Via CLI
octez-manager instance my-node show

# Via systemctl
systemctl --user status octez-node@my-node
```

### View Logs

```bash
# Via TUI: select instance, press 'l'

# Via CLI
octez-manager instance my-node logs

# Via journalctl
journalctl --user -u octez-node@my-node -f
```

### Verify Sync Progress

```bash
curl -s http://127.0.0.1:8732/chains/main/blocks/head/header | jq .level
```

## Configuration Files

Octez Manager creates (in user mode):

| Path | Description |
|------|-------------|
| `~/.config/octez/instances/<name>/` | Instance configuration (node.env) |
| `~/.local/share/octez/<name>/` | Node data directory |
| `~/.config/systemd/user/octez-node@<name>.service.d/` | Systemd overrides |

In system mode (run as root):

| Path | Description |
|------|-------------|
| `/etc/octez/instances/<name>/` | Instance configuration (node.env) |
| `/var/lib/octez/<name>/` | Node data directory |
| `/etc/systemd/system/octez-node@<name>.service.d/` | Systemd overrides |

## Troubleshooting

### Node won't start

Check logs for errors:
```bash
journalctl --user -u octez-node@my-node -n 50
```

### Sync is slow

- Ensure good network connectivity
- Check disk I/O (SSD recommended)
- Consider using a snapshot if syncing from genesis

### Port conflicts

Ensure ports 8732 (RPC) and 9732 (P2P) are available:
```bash
ss -tulnp | grep -E '8732|9732'
```
