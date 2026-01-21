# Test Service Scripts

These scripts create and manage a test octez-node service for testing the import wizard.

## Quick Start

### Create the test service
```bash
./create-test-service.sh
```

This will:
- Create a data directory at `/tmp/test-octez-node-data`
- Create a systemd user service `test-octez-node.service`
- Start a Ghostnet node on RPC port 8733
- Show the service status

### Clean up the test service
```bash
./cleanup-test-service.sh
```

This will:
- Stop the service
- Remove the systemd service file
- Remove the data directory
- Reload systemd

## Service Details

**Default configuration:**
- Service name: `test-octez-node.service`
- Network: Ghostnet
- RPC endpoint: http://127.0.0.1:8733
- Data directory: `/tmp/test-octez-node-data`
- Binary: `~/.local/share/octez-manager/binaries/v24.0/octez-node`

## Customization

To use a different network, RPC port, or binary version, edit the configuration variables at the top of `create-test-service.sh`:

```bash
SERVICE_NAME="test-octez-node"
NETWORK="ghostnet"           # Change to: mainnet, ghostnet, etc.
RPC_PORT="8733"              # Change to any available port
DATA_DIR="/tmp/test-octez-node-data"
BINARY_VERSION="v24.0"       # Change to installed version
```

**Note:** If you change `SERVICE_NAME` or `DATA_DIR`, also update them in `cleanup-test-service.sh`.

## Useful Commands

```bash
# Check service status
systemctl --user status test-octez-node.service

# View live logs
journalctl --user -u test-octez-node.service -f

# Stop service (without cleanup)
systemctl --user stop test-octez-node.service

# Start service (after stopping)
systemctl --user start test-octez-node.service

# Restart service
systemctl --user restart test-octez-node.service
```

## Testing the Import Wizard

1. Create the test service:
   ```bash
   ./create-test-service.sh
   ```

2. Run octez-manager:
   ```bash
   dune exec octez-manager
   ```

3. Navigate to the import wizard (usually under instances → import)

4. The wizard should detect the `test-octez-node.service`

5. Test the import process:
   - Select the service with arrows
   - Press Enter or Tab to advance
   - Choose import strategy (Takeover/Clone/External)
   - Review settings
   - Import the service
   - Verify Esc key exits from success screen

6. Clean up when done:
   ```bash
   ./cleanup-test-service.sh
   ```

## Troubleshooting

### Binary not found error
Make sure you have octez binaries installed:
```bash
ls ~/.local/share/octez-manager/binaries/
```

If no binaries exist, use octez-manager to download them first.

### Port already in use
If port 8733 is already in use, edit `RPC_PORT` in `create-test-service.sh` to use a different port (e.g., 8734).

### Service won't start
Check the logs:
```bash
journalctl --user -u test-octez-node.service -n 50
```

Common issues:
- Data directory permissions
- Network connectivity
- Binary compatibility
