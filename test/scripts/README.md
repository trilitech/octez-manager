# Test Scripts

Helper scripts for testing octez-manager functionality.

## create-unmanaged-services.sh

Creates a complete set of unmanaged Octez services (node, DAL node, baker, accuser) for testing the import functionality.

### Features

- Creates systemd **user services** (no sudo required)
- Supports snapshot import for faster node bootstrap
- Automatically starts all services
- Uses environment files (mimics real deployments)
- Sets up test wallet with `alice` and `bob` keys
- Baker configured with delegates, DAL node, and liquidity baking vote

### Usage

```bash
# Basic usage (no snapshot, shadownet)
./create-unmanaged-services.sh

# With snapshot download (uses default shadownet/ghostnet snapshot URL)
./create-unmanaged-services.sh --snapshot

# With local snapshot file
./create-unmanaged-services.sh --snapshot /path/to/snapshot.rolling

# For a specific network
./create-unmanaged-services.sh --network ghostnet

# With snapshot for mainnet
./create-unmanaged-services.sh --snapshot --network mainnet

# Show help
./create-unmanaged-services.sh --help
```

### Testing Import

After creating the services, you can test various import scenarios:

```bash
# List external services
dune exec -- octez-manager list --external

# Import single service
dune exec -- octez-manager import test-octez-node --strategy takeover

# Import cascade (all dependent services)
dune exec -- octez-manager import-cascade test-octez-node --strategy takeover

# Test dry-run
dune exec -- octez-manager import test-octez-node --strategy takeover --dry-run

# Test clone strategy
dune exec -- octez-manager import test-octez-node --strategy clone --as cloned-node
```

### What Gets Created

**Services:**
- `test-octez-node.service` - Node on ports 18732 (RPC), 19732 (P2P)
- `test-octez-dal-node.service` - DAL node on ports 10732 (RPC), 11732 (P2P)
- `test-octez-baker.service` - Baker with `alice` and `bob` delegates
- `test-octez-accuser.service` - Accuser

**Data directories:**
- `/tmp/octez-unmanaged-test/node-data` - Node data
- `/tmp/octez-unmanaged-test/dal-data` - DAL data
- `/tmp/octez-unmanaged-test/client` - Client wallet

**Configuration:**
- `~/.config/octez-unmanaged-env/` - Environment files
- `~/.config/systemd/user/test-octez-*.service` - Service files

## cleanup-unmanaged-services.sh

Removes all services and data created by `create-unmanaged-services.sh`.

### Usage

```bash
./cleanup-unmanaged-services.sh
```

This will:
1. Stop all test services
2. Disable all test services
3. Remove service files
4. Remove data directories
5. Reload systemd

## Testing Scenarios

These scripts are useful for testing:

### Import Functionality
- Delegate extraction from ExecStart
- Network detection (especially for DAL nodes)
- Environment variable parsing
- Dependency resolution (baker → node, dal → node)
- Cascade import with dependencies

### Import Strategies
- **Takeover**: Import and stop original service
- **Clone**: Import while keeping original running

### Edge Cases
- Services with complex ExecStart commands
- Baker with `run with local node` syntax
- Multiple delegates in argument list
- Liquidity baking vote parsing
- DAL node endpoint configuration

## Maintenance

Update the `BIN_DIR` variable if your octez binaries are in a different location:

```bash
BIN_DIR="${HOME}/.local/share/octez-manager/binaries/v24.0"
```

Default snapshot URLs are configured for:
- **shadownet**: Uses ghostnet snapshot (no dedicated shadownet snapshots)
- **ghostnet**: `https://snapshots.eu.tzinit.org/ghostnet/rolling`
- **mainnet**: `https://snapshots.eu.tzinit.org/mainnet/rolling`
