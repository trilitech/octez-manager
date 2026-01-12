# Integration Tests

Integration tests for octez-manager run in Docker containers with real systemd, allowing us to test actual service management, lifecycle operations, and dependency handling.

## Architecture

```
┌─────────────────────────────────────────────────────────┐
│                    docker-compose                        │
├─────────────────────┬───────────────────────────────────┤
│     sandbox         │           cli-tester              │
│  ┌───────────────┐  │  ┌─────────────────────────────┐  │
│  │ Mock teztnets │  │  │ systemd (PID 1)             │  │
│  │ Mock snapshot │  │  │ octez-manager               │  │
│  │               │  │  │ octez-node/baker/accuser    │  │
│  │  HTTP server  │◄─┼──│ test scripts                │  │
│  └───────────────┘  │  └─────────────────────────────┘  │
└─────────────────────┴───────────────────────────────────┘
```

- **sandbox**: Serves mock teztnets.json and snapshots for network discovery
- **cli-tester**: Runs tests with real systemd, octez binaries, and octez-manager

## Running Tests

### Locally

```bash
# From the integration directory
cd test/integration
./run.sh

# Or with docker-compose directly
docker compose up --build --abort-on-container-exit

# Run specific tests (modify run-tests.sh or use docker exec)
docker compose up -d
docker compose exec cli-tester /tests/node/01-install.sh
```

### In CI

Integration tests run automatically on pull requests via GitHub Actions. The workflow:
1. Builds a static octez-manager binary
2. Starts the sandbox and cli-tester containers
3. Runs all tests in order
4. Reports results

## Test Organization

Tests are organized by service role in `cli-tester/tests/`:

```
tests/
├── lib.sh                    # Shared helper functions
├── node/
│   ├── 01-install.sh
│   ├── 02-show.sh
│   ├── ...
│   └── 19-cascading-dependencies.sh
├── dal/
│   ├── 18-dal-basic-install.sh
│   └── ...
├── baker/
│   ├── 26-baker-basic-install.sh
│   └── ...
└── accuser/
    ├── 35-accuser-basic-install.sh
    └── ...
```

Tests are numbered for execution order. Numbers are globally unique across all directories.

## Writing Tests

### Basic Structure

```bash
#!/bin/bash
# Test: Description of what this test does
set -euo pipefail
source /tests/lib.sh

INSTANCE="test-my-feature"
NODE_RPC="127.0.0.1:18XXX"   # Use unique ports
NODE_NET="0.0.0.0:19XXX"

echo "Test: My feature test"

# Cleanup from previous runs
cleanup_instance "$INSTANCE" || true

# Test logic here
om install-node \
    --instance "$INSTANCE" \
    --network shadownet \
    --service-user tezos \
    --no-enable 2>&1

# Verify expected behavior
if ! instance_exists "$INSTANCE"; then
    echo "ERROR: Instance not created"
    exit 1
fi

# Cleanup
cleanup_instance "$INSTANCE"

echo "My feature test passed"
```

### Helper Functions (lib.sh)

| Function | Description |
|----------|-------------|
| `cleanup_instance <name>` | Remove instance and its data |
| `instance_exists <name>` | Check if instance is registered |
| `service_exists <role> <instance>` | Check if systemd service exists |
| `service_is_active <role> <instance>` | Check if service is running |
| `wait_for_service_active <role> <instance> <timeout>` | Wait for service to start |
| `wait_for_service_stopped <role> <instance> <timeout>` | Wait for service to stop |
| `wait_for_node_ready <rpc_addr> <timeout>` | Wait for node RPC to respond |
| `get_node_level <rpc_addr>` | Get current block level from node |
| `show_service_logs <role> <instance> <lines>` | Print recent service logs |

### Environment Variables

| Variable | Description |
|----------|-------------|
| `SANDBOX_URL` | URL to sandbox HTTP server (for snapshots) |

### Port Allocation

To avoid conflicts, use unique ports for each test. Current allocation:

| Range | Usage |
|-------|-------|
| 18700-18799 | Node RPC ports |
| 19700-19899 | Node P2P ports |
| 10700-10799 | DAL RPC ports |
| 11700-11799 | DAL P2P ports |

### Best Practices

1. **Always cleanup**: Use `cleanup_instance` at start and end
2. **Use unique ports**: Avoid port conflicts between tests
3. **Handle async**: Use `wait_for_*` functions, not `sleep`
4. **Show logs on failure**: Call `show_service_logs` before `exit 1`
5. **Test one thing**: Each test should verify a specific feature
6. **Use `--no-enable`**: Prevent auto-start, control lifecycle explicitly

## Test Categories

### Installation Tests
Verify services install correctly with various configurations:
- Basic install
- Custom data directories
- Custom RPC/P2P addresses
- Extra arguments
- Different history modes (node)

### Lifecycle Tests
Verify service management commands:
- Start/stop/restart
- Service stability (doesn't crash)
- Graceful shutdown

### Dependency Tests
Verify systemd dependencies work correctly:
- BindsTo: dependent stops when parent stops
- After: dependent starts after parent
- Cascading: all dependents stop when node stops
- Error handling: clear error when starting without parent

### Configuration Tests
Verify configuration options:
- DAL endpoint configuration
- Remote vs local node endpoints
- Delegate configuration (baker)
- Liquidity baking vote (baker)

## Debugging

### View container logs

```bash
docker compose logs sandbox
docker compose logs cli-tester
```

### Interactive debugging

```bash
docker compose up -d
docker compose exec cli-tester bash

# Inside container:
om list
systemctl status octez-node@myinstance
journalctl -u octez-node@myinstance -n 50
```

### Common Issues

1. **Port already in use**: Previous test didn't cleanup, or port conflict
2. **Service won't start**: Check `show_service_logs` output
3. **Timeout waiting for node**: Identity generation can take 60-120s on slow CI
4. **Snapshot import fails**: Check sandbox container is running
