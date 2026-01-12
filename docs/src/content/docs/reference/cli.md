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

## Commands

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
| `--snapshot` | Bootstrap from snapshot | false |
| `--snapshot-uri <URL>` | Custom snapshot URL | auto |
| `--snapshot-no-check` | Skip snapshot integrity check | false |
| `--service-user <USER>` | System user for the service | current user |
| `--no-enable` | Don't auto-start the service | false |
| `--preserve-data` | Keep existing data directory | false |
| `--extra-arg <ARG>` | Extra argument for octez-node (repeatable) | - |

### `install-dal-node`

Install a new DAL node.

```bash
octez-manager install-dal-node [OPTIONS]
```

| Option | Description | Default |
|--------|-------------|---------|
| `--instance <NAME>` | Instance name | (prompted) |
| `--node-instance <NAME or URL>` | Local node instance name or remote endpoint URL | - |
| `--data-dir <PATH>` | Data directory | auto |
| `--rpc-addr <ADDR>` | RPC address | 127.0.0.1:10732 |
| `--net-addr <ADDR>` | P2P address | [::]:11732 |
| `--service-user <USER>` | System user for the service | current user |
| `--no-enable` | Don't auto-start the service | false |
| `--extra-arg <ARG>` | Extra argument for octez-dal-node (repeatable) | - |

### `install-baker`

Install a new baker.

```bash
octez-manager install-baker [OPTIONS]
```

| Option | Description | Default |
|--------|-------------|---------|
| `--instance <NAME>` | Instance name | (prompted) |
| `--node-instance <NAME or URL>` | Local node instance name or remote endpoint URL | - |
| `--delegate <ADDR>` | Delegate address (repeatable for multiple delegates) | (prompted) |
| `--liquidity-baking-vote <V>` | on, off, pass | (prompted) |
| `--dal-endpoint <NAME or URL>` | DAL node instance name or endpoint URL | - |
| `--base-dir <PATH>` | Baker base directory | auto |
| `--service-user <USER>` | System user for the service | current user |
| `--no-enable` | Don't auto-start the service | false |
| `--extra-arg <ARG>` | Extra argument for octez-baker (repeatable) | - |

### `install-accuser`

Install a new accuser.

```bash
octez-manager install-accuser [OPTIONS]
```

| Option | Description | Default |
|--------|-------------|---------|
| `--instance <NAME>` | Instance name | (prompted) |
| `--node-instance <NAME or URL>` | Local node instance name or remote endpoint URL | - |
| `--base-dir <PATH>` | Accuser base directory | auto |
| `--service-user <USER>` | System user for the service | current user |
| `--no-enable` | Don't auto-start the service | false |

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
| `remove` | Remove instance (keeps data) |
| `purge` | Remove instance and delete data |

### `list`

List all instances.

```bash
octez-manager list [OPTIONS]
```

| Option | Description |
|--------|-------------|
| `--json` | Output as JSON |

### `ui`

Launch the interactive terminal UI.

```bash
octez-manager ui [OPTIONS]
```

| Option | Description |
|--------|-------------|
| `--page <NAME>` | Start on a specific page |
| `--ui-log` | Enable UI debug logs |
| `--ui-logfile <FILE>` | Write UI logs to file |

### `list-available-networks`

Show networks available from teztnets.com.

```bash
octez-manager list-available-networks [OPTIONS]
```

| Option | Description |
|--------|-------------|
| `--json` | Output as JSON |

### `list-snapshots`

List snapshots available from snapshots.tzinit.org.

```bash
octez-manager list-snapshots [OPTIONS]
```

| Option | Description | Default |
|--------|-------------|---------|
| `--network <NET>` | Network to list snapshots for | mainnet |
| `--json` | Output as JSON | false |

### `purge-all`

Purge all registered instances. Removes services, data directories, and log files.

```bash
octez-manager purge-all
```

### `cleanup-orphans`

Remove orphan data directories and log files not associated with any registered service.

```bash
octez-manager cleanup-orphans [OPTIONS]
```

| Option | Description |
|--------|-------------|
| `--dry-run, -n` | Show what would be removed without deleting |

### `cleanup-dependencies`

Remove stale dependency entries from service configurations.

```bash
octez-manager cleanup-dependencies
```

## Examples

```bash
# Install mainnet node with snapshot
octez-manager install-node --instance mainnet --network mainnet --snapshot

# Install baker with multiple delegates
octez-manager install-baker \
  --instance my-baker \
  --node-instance mainnet \
  --delegate tz1abc... \
  --delegate tz1def... \
  --liquidity-baking-vote pass

# Install baker with remote node endpoint
octez-manager install-baker \
  --instance my-baker \
  --node-instance http://localhost:8732 \
  --delegate tz1abc... \
  --liquidity-baking-vote pass

# Restart a service
octez-manager instance mainnet restart

# View logs
octez-manager instance mainnet logs

# List all instances as JSON
octez-manager list --json

# Launch the TUI
octez-manager ui

# List available networks
octez-manager list-available-networks

# List snapshots for ghostnet
octez-manager list-snapshots --network ghostnet
```
