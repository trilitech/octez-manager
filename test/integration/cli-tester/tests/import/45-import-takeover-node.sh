#!/bin/bash
# Test: Import with takeover strategy
set -euo pipefail
source /tests/lib.sh

INSTANCE="takeover-node"
DATA_DIR="/var/lib/octez-external/$INSTANCE"
RPC_ADDR="127.0.0.1:18745"

echo "Test: Import node with takeover strategy"

# Cleanup
cleanup_instance "$INSTANCE" || true
rm -rf "$DATA_DIR" || true
systemctl stop "octez-node@${INSTANCE}.service" 2>/dev/null || true
systemctl disable "octez-node@${INSTANCE}.service" 2>/dev/null || true
rm -f "/etc/systemd/system/octez-node@${INSTANCE}.service" || true
systemctl daemon-reload

# Create external service with pre-generated identity
echo "Creating external systemd service..."
mkdir -p "$DATA_DIR"
inject_identity "$INSTANCE" "$DATA_DIR"
chown -R tezos:tezos "$DATA_DIR"
create_external_service "node" "$INSTANCE" "$DATA_DIR" "$RPC_ADDR" "shadownet"
systemctl enable "octez-node@${INSTANCE}.service"

# Start service briefly so it can be detected
systemctl start "octez-node@${INSTANCE}.service"
sleep 2

# Debug: Check what external services are detected
echo "DEBUG: External services detected:"
om list --external 2>&1 || true
echo "DEBUG: Systemd unit status:"
systemctl status "octez-node@${INSTANCE}.service" --no-pager || true
echo "DEBUG: List of octez unit files:"
systemctl list-unit-files "octez-*.service" --no-legend || true

# Import with takeover strategy
echo "Importing with takeover strategy..."
om import "octez-node@${INSTANCE}" --strategy takeover 2>&1

# Stop the service immediately after import to avoid long sync
systemctl stop "octez-node@${INSTANCE}.service" 2>/dev/null || true

# Verify service is now managed
if ! service_is_managed "$INSTANCE"; then
	echo "ERROR: Service is not managed after import"
	om list 2>&1
	exit 1
fi

# Verify original external service is disabled
if ! external_service_disabled "node" "$INSTANCE"; then
	echo "ERROR: Original service should be disabled after takeover"
	systemctl status "octez-node@${INSTANCE}.service" || true
	exit 1
fi

# Verify data directory is preserved
if [ ! -d "$DATA_DIR" ]; then
	echo "ERROR: Data directory should be preserved"
	exit 1
fi

echo "Service successfully imported with takeover"

# Cleanup
cleanup_instance "$INSTANCE"
rm -rf "$DATA_DIR"

echo "Takeover import test passed"
