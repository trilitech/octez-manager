---
title: CLI Reference
description: Complete CLI command reference
---

# CLI Reference

Complete reference for all Octez Manager CLI commands.

## Global Options

```bash
octez-manager [OPTIONS] <COMMAND>
```

| Option | Description |
|--------|-------------|
| `--help` | Show help message |
| `--version` | Show version |
| `-y, --yes` | Skip confirmation prompts |

## Commands

### `octez-manager` (no args)

Launch the interactive TUI.

### `install-node`

Install a new Tezos node.

```bash
octez-manager install-node [OPTIONS]
```

| Option | Description | Default |
|--------|-------------|---------|
| `--instance <NAME>` | Instance name | (prompted) |
| `--network <NET>` | Network (mainnet, ghostnet, etc.) | (prompted) |
| `--history-mode <MODE>` | rolling, full, archive | rolling |
| `--data-dir <PATH>` | Data directory | auto |
| `--rpc-addr <ADDR>` | RPC address | 127.0.0.1:8732 |
| `--net-addr <ADDR>` | P2P address | [::]:9732 |
| `--bootstrap <TYPE>` | snapshot or genesis | snapshot |
| `--snapshot-url <URL>` | Custom snapshot URL | auto |
| `--snapshot-no-check` | Skip snapshot integrity check | false |

### `install-baker`

Install a new baker.

```bash
octez-manager install-baker [OPTIONS]
```

| Option | Description | Default |
|--------|-------------|---------|
| `--instance <NAME>` | Instance name | (prompted) |
| `--node-instance <NAME>` | Local node instance | - |
| `--node-endpoint <URL>` | Remote node endpoint | - |
| `--delegates <ADDRS>` | Comma-separated addresses | (prompted) |
| `--liquidity-baking-vote <V>` | on, off, pass | (prompted) |
| `--dal-node <NAME>` | DAL node instance | - |

### `install-accuser`

Install a new accuser.

```bash
octez-manager install-accuser [OPTIONS]
```

| Option | Description | Default |
|--------|-------------|---------|
| `--instance <NAME>` | Instance name | (prompted) |
| `--node-instance <NAME>` | Local node instance | - |
| `--node-endpoint <URL>` | Remote node endpoint | - |

### `install-dal-node`

Install a new DAL node.

```bash
octez-manager install-dal-node [OPTIONS]
```

| Option | Description | Default |
|--------|-------------|---------|
| `--instance <NAME>` | Instance name | (prompted) |
| `--node-instance <NAME>` | Local node instance | - |
| `--node-endpoint <URL>` | Remote node endpoint | - |
| `--rpc-addr <ADDR>` | RPC address | 127.0.0.1:10732 |
| `--net-addr <ADDR>` | P2P address | [::]:11732 |

### `instance`

Manage existing instances.

```bash
octez-manager instance <NAME> <ACTION>
```

Actions:

| Action | Description |
|--------|-------------|
| `start` | Start the service |
| `stop` | Stop the service |
| `restart` | Restart the service |
| `show` | Show instance details |
| `show-service` | Show systemd service status |
| `logs` | View logs |
| `edit` | Edit configuration |
| `remove` | Remove instance |
| `purge` | Remove instance and data |

### `list`

List all instances.

```bash
octez-manager list [OPTIONS]
```

| Option | Description |
|--------|-------------|
| `--json` | Output as JSON |

### `completions`

Generate shell completions.

```bash
octez-manager completions <SHELL>
```

Supported shells: `bash`, `zsh`, `fish`

## Examples

```bash
# Install mainnet node with defaults
octez-manager install-node --instance mainnet --network mainnet -y

# Install baker with multiple delegates
octez-manager install-baker \
  --instance my-baker \
  --node-instance mainnet \
  --delegates tz1abc,tz1def \
  --liquidity-baking-vote pass

# Restart a service
octez-manager instance mainnet restart

# View logs
octez-manager instance mainnet logs

# List all instances as JSON
octez-manager list --json
```
