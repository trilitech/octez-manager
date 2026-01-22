---
title: CLI Reference
description: Command reference for scripting and automation
---

The CLI is for advanced users who prefer command-line workflows or need to automate deployments with scripts. For interactive use, see [Using the TUI](/guides/tui-guide).

## User Mode vs System Mode

octez-manager operates differently depending on how it's run:

| Mode | How to run | Services run as | Data location |
|------|-----------|-----------------|---------------|
| **User mode** | `octez-manager` | Your user | `~/.local/share/octez-manager/` |
| **System mode** | `sudo octez-manager` | Dedicated service users | `/var/lib/octez-manager/` |

> **Note:** User mode and system mode are independent. Instances created in one mode are not visible in the other.

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
| `--network <NET>` | Network (mainnet, ghostnet, shadownet, etc.) | shadownet |
| `--history-mode <MODE>` | rolling, full, archive | rolling |
| `--data-dir <PATH>` | Data directory | auto |
| `--rpc-addr <ADDR>` | RPC address | 127.0.0.1:8732 |
| `--net-addr <ADDR>` | P2P address | 0.0.0.0:9732 |
| `--snapshot` | Bootstrap from snapshot | false |
| `--snapshot-uri <URL>` | Custom snapshot URL | auto |
| `--snapshot-no-check` | Skip snapshot integrity check | false |
| `--keep-snapshot` | Keep snapshot file after import | false |
| `--tmp-dir <PATH>` | Temporary directory for snapshot download | /tmp |
| `--service-user <USER>` | System user for the service | current user |
| `--octez-version <VER>` | Use a managed binary version | - |
| `--bin-dir-alias <ALIAS>` | Use a linked directory alias | - |
| `--app-bin-dir <PATH>` | Directory containing Octez binaries | auto |
| `--no-enable` | Don't auto-start the service | false |
| `--preserve-data` | Keep existing data directory | false |
| `--extra-arg <ARG>` | Extra argument for octez-node (repeatable) | - |

**Binary selection priority:** `--octez-version` > `--bin-dir-alias` > `--app-bin-dir` > auto-detect from PATH

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
| `--net-addr <ADDR>` | P2P address | 0.0.0.0:11732 |
| `--service-user <USER>` | System user for the service | current user |
| `--octez-version <VER>` | Use a managed binary version | - |
| `--bin-dir-alias <ALIAS>` | Use a linked directory alias | - |
| `--app-bin-dir <PATH>` | Directory containing Octez binaries | auto |
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
| `--octez-version <VER>` | Use a managed binary version | - |
| `--bin-dir-alias <ALIAS>` | Use a linked directory alias | - |
| `--app-bin-dir <PATH>` | Directory containing Octez binaries | auto |
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
| `--octez-version <VER>` | Use a managed binary version | - |
| `--bin-dir-alias <ALIAS>` | Use a linked directory alias | - |
| `--app-bin-dir <PATH>` | Directory containing Octez binaries | auto |
| `--no-enable` | Don't auto-start the service | false |
| `--extra-arg <ARG>` | Extra argument for octez-accuser (repeatable) | - |

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
| `export-logs` | Export logs to a tar.gz archive |
| `edit` | Edit configuration |
| `remove` | Remove instance (keeps data) |
| `purge` | Remove instance and delete data |

### `list`

List registered instances.

```bash
octez-manager list [OPTIONS]
```

| Option | Description |
|--------|-------------|
| `--external` | Include unmanaged/external services |
| `--all` | Show all details |
| `--json` | Output as JSON |

### `import`

Import an external (unmanaged) Octez service into octez-manager.

```bash
octez-manager import <SERVICE_NAME> [OPTIONS]
```

| Option | Description | Default |
|--------|-------------|---------|
| `--as <NAME>` | Custom instance name | auto-generated |
| `--network <NET>` | Override network if not detected | auto-detected |
| `--strategy <S>` | `takeover` (disable original) or `clone` (keep original) | takeover |
| `--dry-run, -d` | Preview import plan without making changes | false |
| `--cascade, -c` | Import service and all its dependencies | false |
| `--interactive, -i` | Review and edit configuration before import | false |

**Example:**

```bash
# Preview what would be imported
octez-manager import octez-node-mainnet --dry-run

# Import with a custom name
octez-manager import octez-node-mainnet --as my-mainnet-node

# Clone without affecting the original service
octez-manager import octez-node-mainnet --strategy clone
```

### `binaries`

Manage Octez binary versions. Binaries are stored in:
- User mode: `~/.local/share/octez-manager/binaries/`
- System mode: `/var/lib/octez-manager/binaries/`

#### `binaries list`

List installed managed versions and linked directories.

```bash
octez-manager binaries list
```

#### `binaries download`

Download an Octez version from GitLab releases.

```bash
octez-manager binaries download <VERSION> [OPTIONS]
```

| Option | Description |
|--------|-------------|
| `--no-verify` | Skip SHA256 checksum verification |

**Example:**

```bash
octez-manager binaries download 21.0
```

#### `binaries remove`

Remove an installed managed version.

```bash
octez-manager binaries remove <VERSION> [OPTIONS]
```

| Option | Description |
|--------|-------------|
| `--force` | Remove even if in use by a service |

#### `binaries prune`

Remove all unused managed versions (not referenced by any service).

```bash
octez-manager binaries prune [OPTIONS]
```

| Option | Description |
|--------|-------------|
| `--dry-run, -n` | Show what would be removed with disk space |

#### `binaries link`

Create an alias for a custom binary directory (e.g., a dev build).

```bash
octez-manager binaries link <PATH> [OPTIONS]
```

| Option | Description |
|--------|-------------|
| `--alias <NAME>` | Custom alias name (default: directory name) |

**Example:**

```bash
octez-manager binaries link ~/octez/_build/default/src --alias dev-build
```

#### `binaries unlink`

Remove a linked directory alias.

```bash
octez-manager binaries unlink <ALIAS_OR_PATH> [OPTIONS]
```

| Option | Description |
|--------|-------------|
| `--force` | Remove even if in use by a service |

#### `binaries list-remote`

Show available versions from GitLab releases.

```bash
octez-manager binaries list-remote [OPTIONS]
```

| Option | Description |
|--------|-------------|
| `--all` | Show all versions (including older ones) |

### `self-update`

Check for and install octez-manager updates.

```bash
octez-manager self-update [OPTIONS]
```

| Option | Description |
|--------|-------------|
| `--check, -c` | Only check for updates, don't install |
| `--force, -f` | Force check (bypass cache) |

**Behavior depends on install method:**
- **Package install (apt/dpkg):** Shows the appropriate `apt` command to run
- **Binary install (install script):** Downloads and installs automatically
- **Manual install:** Shows link to release page

**Example:**

```bash
# Check if updates are available
octez-manager self-update --check

# Install updates
octez-manager self-update
```

### `version`

Show version information and check for updates.

```bash
octez-manager version
```

### `ui`

Launch the interactive terminal UI (default command).

```bash
octez-manager [OPTIONS]
```

| Option | Description |
|--------|-------------|
| `--page <NAME>` | Start on a specific page |
| `--ui-log` | Enable UI debug logs |
| `--ui-logfile <FILE>` | Write UI logs to file |

> **Note**: When run without arguments, `octez-manager` launches the TUI. Use explicit subcommands for CLI operations.

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
| `--network <NET>` | Network to list snapshots for | shadownet |
| `--json` | Output as JSON | false |

### `purge-all`

Purge all registered instances. Removes services, data directories, and log files.

> **Warning:** This command is destructive and intended for testing purposes. It will permanently delete all instance data and cannot be undone.

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

### Quick Start with Shadownet

```bash
# Install Shadownet node with snapshot (recommended for testing)
octez-manager install-node \
  --instance shadownet \
  --network shadownet \
  --history-mode rolling \
  --snapshot

# Install baker
octez-manager install-baker \
  --instance baker-shadownet \
  --node-instance shadownet \
  --delegate tz1... \
  --liquidity-baking-vote pass

# Install accuser
octez-manager install-accuser \
  --instance accuser-shadownet \
  --node-instance shadownet
```

### Using Managed Binaries

```bash
# Download a specific Octez version
octez-manager binaries download 21.0

# Install a node using that version
octez-manager install-node \
  --instance shadownet \
  --network shadownet \
  --octez-version 21.0 \
  --snapshot

# Link a local dev build
octez-manager binaries link ~/octez/_build/default/src --alias dev

# Install using the dev build
octez-manager install-node \
  --instance dev-node \
  --network shadownet \
  --bin-dir-alias dev
```

### Service Management

```bash
# List all instances (including external/unmanaged)
octez-manager list --external

# View logs
octez-manager instance shadownet logs

# Restart a service
octez-manager instance shadownet restart

# Launch the TUI
octez-manager
```

### Importing External Services

```bash
# List external services
octez-manager list --external

# Preview import
octez-manager import octez-node-mainnet --dry-run

# Import with takeover (disable original)
octez-manager import octez-node-mainnet --as mainnet

# Import with clone (keep original running)
octez-manager import octez-node-mainnet --strategy clone --as mainnet-copy
```

### Multiple Delegates

```bash
octez-manager install-baker \
  --instance baker-shadownet \
  --node-instance shadownet \
  --delegate tz1abc... \
  --delegate tz1def... \
  --liquidity-baking-vote pass
```

### Remote Node Endpoint

```bash
octez-manager install-baker \
  --instance baker-shadownet \
  --node-instance http://localhost:8732 \
  --delegate tz1abc... \
  --liquidity-baking-vote pass
```

### Network Discovery

```bash
# List available networks
octez-manager list-available-networks

# List snapshots for a network
octez-manager list-snapshots --network shadownet
```

### System Mode (Production)

```bash
# Run as root for production deployments
sudo octez-manager install-node \
  --instance mainnet \
  --network mainnet \
  --service-user tezos \
  --snapshot

# Services will run as dedicated users with proper isolation
sudo octez-manager list
```
