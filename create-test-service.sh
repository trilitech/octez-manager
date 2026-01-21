#!/bin/bash
# Script to create a test octez-node service for import wizard testing

set -e

# Configuration
SERVICE_NAME="test-octez-node"
NETWORK="ghostnet"
RPC_PORT="8733"
DATA_DIR="/tmp/test-octez-node-data"
BINARY_VERSION="v24.0"
BINARY_PATH="$HOME/.local/share/octez-manager/binaries/$BINARY_VERSION/octez-node"

echo "Creating test octez-node service..."
echo "  Service: $SERVICE_NAME.service"
echo "  Network: $NETWORK"
echo "  RPC: http://127.0.0.1:$RPC_PORT"
echo "  Data dir: $DATA_DIR"
echo ""

# Create data directory
echo "Creating data directory..."
mkdir -p "$DATA_DIR"

# Check binary exists
if [ ! -f "$BINARY_PATH" ]; then
	echo "ERROR: Binary not found at $BINARY_PATH"
	echo "Available versions:"
	ls -1 "$HOME/.local/share/octez-manager/binaries/" 2>/dev/null || echo "  (none)"
	exit 1
fi

# Create systemd service directory
echo "Creating systemd user service directory..."
mkdir -p "$HOME/.config/systemd/user"

# Create service file
echo "Creating service file..."
cat >"$HOME/.config/systemd/user/$SERVICE_NAME.service" <<EOF
[Unit]
Description=Test Octez Node ($NETWORK) for Import Testing
After=network.target

[Service]
Type=simple
ExecStart=$BINARY_PATH run --data-dir $DATA_DIR --network $NETWORK --rpc-addr 127.0.0.1:$RPC_PORT
Restart=on-failure
RestartSec=5
Environment="HOME=$HOME"

[Install]
WantedBy=default.target
EOF

# Reload systemd and start service
echo "Reloading systemd daemon..."
systemctl --user daemon-reload

echo "Starting service..."
systemctl --user start "$SERVICE_NAME.service"

# Wait a moment for service to start
sleep 1

# Show status
echo ""
echo "Service status:"
systemctl --user status "$SERVICE_NAME.service" --no-pager

echo ""
echo "✓ Test service created and started!"
echo ""
echo "Useful commands:"
echo "  # Check status"
echo "  systemctl --user status $SERVICE_NAME.service"
echo ""
echo "  # View logs"
echo "  journalctl --user -u $SERVICE_NAME.service -f"
echo ""
echo "  # Stop service"
echo "  systemctl --user stop $SERVICE_NAME.service"
echo ""
echo "  # Restart service"
echo "  systemctl --user restart $SERVICE_NAME.service"
echo ""
echo "To clean up after testing:"
echo "  ./cleanup-test-service.sh"
