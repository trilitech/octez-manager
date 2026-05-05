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
| `--network <NET>` | Network (mainnet, shadownet, etc.) | (prompted) |
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
| `--bin-dir-alias <ALIAS>` | Use a registered directory alias | - |
| `--app-bin-dir <PATH>` | Directory containing Octez binaries | auto |
| `--no-enable` | Don't auto-start the service | false |
| `--preserve-data` | Keep existing data directory | false |
| `--extra-arg <ARG>` | Extra argument for octez-node (repeatable) | - |

**Binary selection priority:** `--octez-version` > `--bin-dir-alias` > `--app-bin-dir` > auto-detect from PATH

> **Tip:** Use `--octez-version latest` to automatically use the most recent stable Octez release. If the version isn't downloaded yet, you'll be prompted to download it.

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
| `--bin-dir-alias <ALIAS>` | Use a registered directory alias | - |
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
| `--remote-signer <NAME or URL>` | Signatory instance name or remote signer URL | - |
| `--dal-endpoint <NAME or URL>` | DAL node instance name or endpoint URL | - |
| `--base-dir <PATH>` | Baker base directory | auto |
| `--service-user <USER>` | System user for the service | current user |
| `--octez-version <VER>` | Use a managed binary version | - |
| `--bin-dir-alias <ALIAS>` | Use a registered directory alias | - |
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
| `--bin-dir-alias <ALIAS>` | Use a registered directory alias | - |
| `--app-bin-dir <PATH>` | Directory containing Octez binaries | auto |
| `--no-enable` | Don't auto-start the service | false |
| `--extra-arg <ARG>` | Extra argument for octez-accuser (repeatable) | - |

### `install-signatory`

Install a new Signatory remote signer service. [Signatory](https://github.com/ecadlabs/signatory) is a remote signing service for secure key management with bakers, supporting hardware security modules and cloud KMS backends.

For detailed setup instructions, backend options, security best practices, and troubleshooting, see the [Signatory Setup Guide](/guides/signatory-setup/).

```bash
octez-manager install-signatory [OPTIONS]
```

| Option | Description | Default |
|--------|-------------|---------|
| `--instance <NAME>` | Signatory instance name | (prompted) |
| `--backend <TYPE>` | Backend type: `file` (currently only option) | file |
| `--keys-dir <PATH>` | Directory for storing keys (file backend only) | auto |
| `--authorized-keys <KEYS>` | Authorized public key hashes with optional permissions | (prompted) |
| `--address <ADDR>` | HTTP server address | 127.0.0.1:6732 |
| `--metrics-address <ADDR>` | Metrics endpoint address (empty to disable) | - |
| `--watermark <BACKEND>` | Watermark storage: `memory`, `file` | memory |
| `--service-user <USER>` | System user for the service | current user |
| `--signatory-version <VER>` | Use a managed Signatory version | - |
| `--bin-dir-alias <ALIAS>` | Use a registered directory alias | - |
| `--app-bin-dir <PATH>` | Directory containing Signatory binary | auto |
| `--no-enable` | Don't auto-start the service | false |

**Example:**

```bash
# Development setup with file backend
octez-manager install-signatory \
  --instance dev-signer \
  --backend file \
  --authorized-keys tz1abc123... \
  --signatory-version latest
```

**Using with Baker:**

After installing Signatory, reference it when installing a baker:

```bash
octez-manager install-baker \
  --instance my-baker \
  --delegate tz1abc123... \
  --remote-signer dev-signer
```

**Keys Directory:**

The default keys directory location depends on the execution mode:
- **User mode**: `~/.local/share/octez/signatory/<instance>/keys`
- **System mode**: `/var/lib/octez/signatory/<instance>/keys`

After installation, import your baker keys into this directory:

```bash
# Copy from octez-client
cp ~/.tezos-client/secret_keys ~/.local/share/octez/signatory/dev-signer/keys/

# Or import directly
octez-client -d ~/.local/share/octez/signatory/dev-signer/keys \
  import secret key my-baker unencrypted:edsk...
```

**Security Notes:**

- **File backend** stores keys unencrypted on disk — suitable for development/testing only
- For production, use hardware-backed backends (YubiHSM) or cloud KMS (AWS/Azure/GCP)
- Always restrict `--authorized-keys` to only the public key hashes that should be able to sign
- Use `--watermark file` for production to persist watermarks across restarts
- Enable `--metrics-address` for monitoring signing activity

See the [Signatory Setup Guide](/guides/signatory-setup/) for comprehensive documentation and security best practices.

### `baker`

Manage baker wallet operations and delegate actions.

```bash
octez-manager baker <SUBCOMMAND>
```

#### `baker list`

List all baker instances.

```bash
octez-manager baker list [OPTIONS]
```

| Option | Description |
|--------|-------------|
| `--json` | Output as JSON |

#### `baker <instance> status`

Show wallet state for a baker instance (balances, staking parameters, consensus key).

```bash
octez-manager baker <INSTANCE> status [OPTIONS]
```

| Option | Description |
|--------|-------------|
| `--delegate <PKH>` | Target delegate public key hash (default: first delegate) |
| `--json` | Output as JSON |

**Example:**

```bash
# Show status for default delegate
octez-manager baker baker-shadownet status

# Show status for specific delegate
octez-manager baker baker-shadownet status --delegate tz1abc...
```

#### `baker <instance> register`

Register as a delegate.

```bash
octez-manager baker <INSTANCE> register [OPTIONS]
```

| Option | Description |
|--------|-------------|
| `--delegate <PKH>` | Target delegate public key hash (default: first delegate) |
| `--json` | Output as JSON |
| `--yes`, `-y` | Skip confirmation prompt |

#### `baker <instance> stake`

Stake tez for a baker delegate.

```bash
octez-manager baker <INSTANCE> stake <AMOUNT> [OPTIONS]
```

| Option | Description |
|--------|-------------|
| `--delegate <PKH>` | Target delegate public key hash (default: first delegate) |
| `--json` | Output as JSON |
| `--yes`, `-y` | Skip confirmation prompt |

**Example:**

```bash
octez-manager baker baker-shadownet stake 1000 --yes
```

#### `baker <instance> unstake`

Unstake tez (amount or "everything").

```bash
octez-manager baker <INSTANCE> unstake <AMOUNT> [OPTIONS]
```

| Option | Description |
|--------|-------------|
| `--delegate <PKH>` | Target delegate public key hash (default: first delegate) |
| `--json` | Output as JSON |
| `--yes`, `-y` | Skip confirmation prompt |

**Example:**

```bash
# Unstake specific amount
octez-manager baker baker-shadownet unstake 500

# Unstake everything
octez-manager baker baker-shadownet unstake everything
```

#### `baker <instance> finalize-unstake`

Finalize pending unstake requests.

```bash
octez-manager baker <INSTANCE> finalize-unstake [OPTIONS]
```

| Option | Description |
|--------|-------------|
| `--delegate <PKH>` | Target delegate public key hash (default: first delegate) |
| `--json` | Output as JSON |
| `--yes`, `-y` | Skip confirmation prompt |

#### `baker <instance> transfer`

Transfer tez to another address.

```bash
octez-manager baker <INSTANCE> transfer <AMOUNT> <DESTINATION> [OPTIONS]
```

| Option | Description |
|--------|-------------|
| `--delegate <PKH>` | Target delegate public key hash (default: first delegate) |
| `--json` | Output as JSON |
| `--yes`, `-y` | Skip confirmation prompt |

**Example:**

```bash
octez-manager baker baker-shadownet transfer 100 tz1xyz... --yes
```

#### `baker <instance> set-delegate-params`

Set delegate staking parameters.

```bash
octez-manager baker <INSTANCE> set-delegate-params [OPTIONS]
```

| Option | Description |
|--------|-------------|
| `--limit-of-staking-over-baking <N>` | Limit of staking over baking (0-9) |
| `--edge-of-baking-over-staking <N>` | Edge of baking over staking (0-100) |
| `--delegate <PKH>` | Target delegate public key hash (default: first delegate) |
| `--json` | Output as JSON |
| `--yes`, `-y` | Skip confirmation prompt |

**Example:**

```bash
octez-manager baker baker-shadownet set-delegate-params \
  --limit-of-staking-over-baking 5 \
  --edge-of-baking-over-staking 10
```

#### `baker <instance> update-consensus-key`

Update baker consensus key.

```bash
octez-manager baker <INSTANCE> update-consensus-key <KEY> [OPTIONS]
```

| Option | Description |
|--------|-------------|
| `--delegate <PKH>` | Target delegate public key hash (default: first delegate) |
| `--json` | Output as JSON |
| `--yes`, `-y` | Skip confirmation prompt |

**Example:**

```bash
octez-manager baker baker-shadownet update-consensus-key edpk...
```

#### `baker <instance> vote`

Vote on governance (protocol hash during proposal period, yay/nay/pass during exploration/promotion).

```bash
octez-manager baker <INSTANCE> vote <VALUE> [OPTIONS]
```

| Option | Description |
|--------|-------------|
| `--delegate <PKH>` | Target delegate public key hash (default: first delegate) |
| `--json` | Output as JSON |
| `--yes`, `-y` | Skip confirmation prompt |

**Examples:**

```bash
# Vote on a proposal (proposal period)
octez-manager baker baker-shadownet vote PtNairob...

# Vote yay on current proposal (exploration/promotion period)
octez-manager baker baker-shadownet vote yay

# Vote nay
octez-manager baker baker-shadownet vote nay

# Abstain
octez-manager baker baker-shadownet vote pass
```

### `group`

Manage instance groups for organizing and operating multiple services together.

```bash
octez-manager group <SUBCOMMAND>
```

#### `group create`

Create a new instance group.

```bash
octez-manager group create <NAME> [OPTIONS]
```

| Option | Description | Default |
|--------|-------------|---------|
| `--octez-version <VER>` | Managed Octez version (e.g. '24.1' or 'latest') | - |
| `--bin-dir-alias <ALIAS>` | Registered binary directory alias | - |
| `--app-bin-dir <DIR>` | Path to binaries directory | auto |
| `--service-user <USER>` | System user for services | tezos |

**Example:**

```bash
octez-manager group create shadownet-stack --octez-version 21.0
```

#### `group list`

List all instance groups.

```bash
octez-manager group list [OPTIONS]
```

| Option | Description |
|--------|-------------|
| `--json` | Output as JSON |

#### `group show`

Show details of an instance group.

```bash
octez-manager group show <NAME> [OPTIONS]
```

| Option | Description |
|--------|-------------|
| `--json` | Output as JSON |

#### `group delete`

Delete an instance group.

```bash
octez-manager group delete <NAME> [OPTIONS]
```

| Option | Description |
|--------|-------------|
| `--cascade` | Delete all services in the group |
| `--ungroup` | Remove group but keep services (ungroup them) |

**Examples:**

```bash
# Delete empty group
octez-manager group delete shadownet-stack

# Delete group and all services
octez-manager group delete shadownet-stack --cascade

# Ungroup services but keep them
octez-manager group delete shadownet-stack --ungroup
```

#### `group add`

Add an existing service to a group.

```bash
octez-manager group add <NAME> --instance <INSTANCE>
```

**Example:**

```bash
octez-manager group add shadownet-stack --instance shadownet
```

#### `group remove`

Remove a service from a group (keeps the service).

```bash
octez-manager group remove <NAME> --instance <INSTANCE>
```

**Example:**

```bash
octez-manager group remove shadownet-stack --instance shadownet
```

#### `group start`

Start all services in a group (dependency order).

```bash
octez-manager group start <NAME>
```

#### `group stop`

Stop all services in a group (reverse dependency order).

```bash
octez-manager group stop <NAME>
```

#### `group restart`

Restart all services in a group (stop all, then start all).

```bash
octez-manager group restart <NAME>
```

#### `group upgrade`

Upgrade binary version for all services in a group.

```bash
octez-manager group upgrade <NAME> [OPTIONS]
```

| Option | Description |
|--------|-------------|
| `--octez-version <VER>` | New managed Octez version (e.g. '24.1' or 'latest') |
| `--bin-dir-alias <ALIAS>` | New registered binary directory alias |
| `--app-bin-dir <DIR>` | New path to binaries directory |

**Example:**

```bash
octez-manager group upgrade shadownet-stack --octez-version 21.1
```

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
| `set-env <KEY> <VALUE>` | Set environment variable |
| `get-env [KEY]` | Get environment variable(s) |

### `list`

List registered instances.

```bash
octez-manager list [OPTIONS]
```

| Option | Description |
|--------|-------------|
| `--internal`, `-i` | List only managed services (no external detection) |
| `--external`, `-e` | Show only external (unmanaged) services |
| `--all`, `-a` | Show both managed and external services |

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

List installed managed versions and registered directories.

```bash
octez-manager binaries list
```

#### `binaries download`

Download an Octez version from GitLab releases.

```bash
octez-manager binaries download <VERSION> [OPTIONS]
```

Use `latest` as the version to automatically download the most recent stable release.

| Option | Description |
|--------|-------------|
| `--no-verify` | Skip SHA256 checksum verification |

**Examples:**

```bash
# Download a specific version
octez-manager binaries download 21.0

# Download the latest stable version
octez-manager binaries download latest
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

#### `binaries register`

Create an alias for a custom binary directory (e.g., a dev build).

```bash
octez-manager binaries register <PATH> [OPTIONS]
```

| Option | Description |
|--------|-------------|
| `--alias <NAME>` | Custom alias name (default: directory name) |

**Example:**

```bash
octez-manager binaries register ~/octez/_build/default/src --alias dev-build
```

#### `binaries unregister`

Remove a registered directory alias.

```bash
octez-manager binaries unregister <ALIAS_OR_PATH> [OPTIONS]
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
| `--theme <THEME>` | Theme name or path (built-ins: dark, light) |
| `--local-indexer <URL>` | Register local TzKT-compatible indexer endpoint |
| `--indexer-network <NET>` | Network the local indexer serves (default: mainnet) |
| `--compare-indexers` | Log divergences between local and public indexer |

> **Note**: When run without arguments, `octez-manager` launches the TUI. Use explicit subcommands for CLI operations.

### `web`

Start the web interface for browser-based remote management.

```bash
octez-manager web [OPTIONS]
```

| Option | Description | Default |
|--------|-------------|---------|
| `--port <PORT>`, `-p` | TCP port to listen on | 8080 |
| `--password <PW>` | Controller password (full access) | none |
| `--viewer-password <PW>` | Viewer password (read-only access) | same as --password |
| `--page <NAME>` | Start on a specific page | instances |
| `--ui-log` | Enable UI debug logs | false |
| `--ui-logfile <FILE>` | Write UI logs to file | none |

The web interface provides the same functionality as the TUI, accessible from any browser. It uses xterm.js for terminal emulation over WebSocket.

**Authentication:**
- Without passwords: Anyone can connect
- With `--password`: Controller access requires authentication
- With `--viewer-password`: Separate read-only access with different password

Passwords can also be set via environment variables:
- `MIAOU_WEB_PASSWORD` — Controller password
- `MIAOU_WEB_VIEWER_PASSWORD` — Viewer password (falls back to controller password)

**Examples:**

```bash
# Start on default port (8080), no authentication
octez-manager web

# Custom port with password protection
octez-manager web --port 8443 --password mysecret

# Separate passwords for controller and viewer
octez-manager web --password admin123 --viewer-password viewer123
```

### `rpc`

Query RPC endpoints on node instances. Provides both scripted and interactive access to Tezos node RPCs.

#### `rpc get`

Execute a GET request to an RPC endpoint.

```bash
octez-manager rpc get [OPTIONS] <PATH>
```

| Option | Description |
|--------|-------------|
| `--instance <NAME>`, `-i` | Target a local node instance |
| `--public <NODE>` | Target a public node (from `rpc public-nodes`) |
| `--url <URL>` | Target a custom RPC URL |

**Examples:**

```bash
# Query version on a local instance
octez-manager rpc get --instance shadownet /version

# Query bootstrap status on a public node  
octez-manager rpc get --public mainnet /chains/main/is_bootstrapped

# Query a custom endpoint
octez-manager rpc get --url http://localhost:8732 /chains/main/blocks/head/header
```

#### `rpc list`

List available RPC endpoints at a path.

```bash
octez-manager rpc list [OPTIONS] [PATH]
```

| Option | Description |
|--------|-------------|
| `--instance <NAME>`, `-i` | Target a local node instance |
| `--public <NODE>` | Target a public node |
| `--url <URL>` | Target a custom RPC URL |

**Examples:**

```bash
# List root endpoints
octez-manager rpc list --instance shadownet

# List endpoints under /chains/main
octez-manager rpc list --instance shadownet /chains/main
```

#### `rpc interactive`

Start interactive RPC mode with tab completion.

```bash
octez-manager rpc interactive [OPTIONS]
```

| Option | Description |
|--------|-------------|
| `--instance <NAME>`, `-i` | Target a local node instance |
| `--public <NODE>` | Target a public node |
| `--url <URL>` | Target a custom RPC URL |

In interactive mode:
- Use `Tab` for path completion
- Type paths and press `Enter` to execute
- Type `exit` or press `Ctrl-D` to quit

#### `rpc instances`

List available node instances that can be queried.

```bash
octez-manager rpc instances
```

#### `rpc public-nodes`

List available public RPC nodes from Taquito.

```bash
octez-manager rpc public-nodes
```

### `rewards`

Manage baker rewards and payouts.

```bash
octez-manager rewards <SUBCOMMAND>
```

All rewards commands auto-detect the baker instance when only one baker is registered. Use `--baker <INSTANCE>` when multiple bakers exist.

#### `rewards status`

Show current cycle and payout status for a baker.

```bash
octez-manager rewards status [OPTIONS]
```

| Option | Description |
|--------|-------------|
| `--baker <INSTANCE>` | Baker instance name (auto-inferred if only one baker) |

**Example:**

```bash
octez-manager rewards status --baker baker-shadownet
```

#### `rewards generate`

Calculate and display a payout preview for a specific cycle.

```bash
octez-manager rewards generate [OPTIONS]
```

| Option | Description | Default |
|--------|-------------|---------|
| `--baker <INSTANCE>` | Baker instance name | auto-inferred |
| `--cycle <N>` | Target cycle number | latest completed |
| `--force` | Re-generate even if cycle was already paid | false |
| `--json` | Output as JSON | false |

**Example:**

```bash
# Generate payout for latest completed cycle
octez-manager rewards generate

# Generate for specific cycle
octez-manager rewards generate --cycle 123

# Re-generate already-paid cycle
octez-manager rewards generate --cycle 120 --force
```

#### `rewards history`

Show historical payout summaries.

```bash
octez-manager rewards history [OPTIONS]
```

| Option | Description | Default |
|--------|-------------|---------|
| `--baker <INSTANCE>` | Baker instance name | auto-inferred |
| `--cycles <N>` | Number of recent cycles to show | 30 |
| `--json` | Output as JSON | false |

**Example:**

```bash
# Show last 30 cycles
octez-manager rewards history

# Show last 10 cycles
octez-manager rewards history --cycles 10
```

#### `rewards pay`

Execute payout for a specific cycle.

```bash
octez-manager rewards pay [OPTIONS]
```

| Option | Description | Default |
|--------|-------------|---------|
| `--baker <INSTANCE>` | Baker instance name | auto-inferred |
| `--cycle <N>` | Target cycle number | latest completed |
| `--dry-run` | Simulate without broadcasting | false |
| `--confirm` | Skip interactive confirmation (for automation) | false |

**Example:**

```bash
# Pay latest cycle (with confirmation prompt)
octez-manager rewards pay

# Dry-run for specific cycle
octez-manager rewards pay --cycle 123 --dry-run

# Automated payout (no prompt)
octez-manager rewards pay --confirm
```

#### `rewards config import`

Import an external config.hjson file.

```bash
octez-manager rewards config import <PATH> [OPTIONS]
```

| Option | Description |
|--------|-------------|
| `--baker <INSTANCE>` | Baker instance name (auto-inferred if only one baker) |

**Example:**

```bash
octez-manager rewards config import ./config.hjson
```

#### `rewards notify test`

Send a test notification to all configured channels.

```bash
octez-manager rewards notify test [OPTIONS]
```

| Option | Description |
|--------|-------------|
| `--baker <INSTANCE>` | Baker instance name (auto-inferred if only one baker) |

**Example:**

```bash
octez-manager rewards notify test
```

#### `rewards continual start`

Enable continual payouts. Automatically pays due cycles when the scheduler detects cycle transitions.

```bash
octez-manager rewards continual start [OPTIONS]
```

| Option | Description | Default |
|--------|-------------|---------|
| `--baker <INSTANCE>` | Baker instance name | auto-inferred |
| `--interval <N>` | Pay every N cycles | 1 (every cycle) |
| `--offset <N>` | Cycle offset within the interval | 0 |

**Example:**

```bash
# Enable automatic payout every cycle
octez-manager rewards continual start

# Pay every 3 cycles
octez-manager rewards continual start --interval 3

# Pay every 2 cycles, offset by 1
octez-manager rewards continual start --interval 2 --offset 1
```

#### `rewards continual stop`

Disable continual payouts.

```bash
octez-manager rewards continual stop [OPTIONS]
```

| Option | Description |
|--------|-------------|
| `--baker <INSTANCE>` | Baker instance name (auto-inferred if only one baker) |

#### `rewards continual status`

Show continual mode status.

```bash
octez-manager rewards continual status [OPTIONS]
```

| Option | Description |
|--------|-------------|
| `--baker <INSTANCE>` | Baker instance name (auto-inferred if only one baker) |

### `sandbox`

Manage sandbox environments for local testing and development.

```bash
octez-manager sandbox <SUBCOMMAND>
```

#### `sandbox create`

Create a sandbox with a running node and baker.

```bash
octez-manager sandbox create [OPTIONS]
```

| Option | Description | Default |
|--------|-------------|---------|
| `--network <NET>`, `-n` | Tezos network (e.g. ghostnet, mainnet, or teztnets URL) | shadownet |
| `--name <NAME>` | Sandbox name | auto-generated from network |
| `--snapshot <URI>` | Snapshot URL or file path | auto-fetched |
| `--rpc-addr <ADDR>` | Node RPC address host:port | auto-assigned (starting at 18732) |
| `--max-delegates <N>` | Max delegates to impersonate via yes-wallet | 20 |
| `--num-nodes <N>` | Number of nodes to create (nodes 2+ peer to node 1) | 1 |
| `--num-bakers <N>` | Number of bakers to create (delegates split evenly) | 1 |
| `--accuser` | Install an octez-accuser service | false |
| `--app-bin-dir <PATH>` | Directory containing Octez binaries | auto |
| `--octez-version <VER>` | Managed Octez version (e.g. '24.1' or 'latest') | - |
| `--bin-dir-alias <ALIAS>` | Registered binary directory alias | - |
| `--service-user <USER>` | System user for services | current user or 'tezos' |

**Example:**

```bash
# Create a shadownet sandbox with defaults
octez-manager sandbox create

# Create a ghostnet sandbox with 2 nodes and 3 bakers
octez-manager sandbox create \
  --network ghostnet \
  --name my-test \
  --num-nodes 2 \
  --num-bakers 3 \
  --accuser

# Create with specific Octez version
octez-manager sandbox create \
  --network shadownet \
  --octez-version 21.0
```

#### `sandbox list`

List all sandboxes.

```bash
octez-manager sandbox list
```

#### `sandbox status`

Show sandbox status.

```bash
octez-manager sandbox status <NAME>
```

**Example:**

```bash
octez-manager sandbox status shadownet-sandbox
```

#### `sandbox start`

Start all services in a sandbox.

```bash
octez-manager sandbox start <NAME>
```

**Example:**

```bash
octez-manager sandbox start shadownet-sandbox
```

#### `sandbox stop`

Stop all services in a sandbox.

```bash
octez-manager sandbox stop <NAME>
```

**Example:**

```bash
octez-manager sandbox stop shadownet-sandbox
```

#### `sandbox destroy`

Destroy a sandbox (stops services and removes all data).

```bash
octez-manager sandbox destroy <NAME> [OPTIONS]
```

| Option | Description |
|--------|-------------|
| `--yes`, `-y` | Skip confirmation prompt |

**Example:**

```bash
# Destroy with confirmation
octez-manager sandbox destroy shadownet-sandbox

# Destroy without confirmation
octez-manager sandbox destroy shadownet-sandbox --yes
```

#### `sandbox add-account`

Add an account to a sandbox wallet.

```bash
octez-manager sandbox add-account <NAME> <ADDRESS> [OPTIONS]
```

| Option | Description | Default |
|--------|-------------|---------|
| `--alias <ALIAS>` | Alias for the account | derived from address |

**Example:**

```bash
# Add account with auto-generated alias
octez-manager sandbox add-account shadownet-sandbox tz1abc123...

# Add account with custom alias
octez-manager sandbox add-account shadownet-sandbox tz1abc123... --alias my-account
```

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

# Register a local dev build
octez-manager binaries register ~/octez/_build/default/src --alias dev

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

`sudo octez-manager` is required for system-level management operations, but the
managed services themselves run under the configured service user. Generated
systemd units include `User=`, `Group=`, `NoNewPrivileges=yes`,
`PrivateTmp=yes`, and `ProtectSystem=strict`.
