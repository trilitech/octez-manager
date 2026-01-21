#!/bin/bash
# Test: Detect service-based external instances
set -euo pipefail
source /tests/lib.sh

INSTANCE="external-node-detect"
DATA_DIR="/var/lib/octez-external/$INSTANCE"
RPC_ADDR="127.0.0.1:18743"

echo "Test: Detect service-based external Octez services"

# Cleanup from previous runs
cleanup_instance "$INSTANCE" || true
rm -rf "$DATA_DIR" || true
systemctl stop "octez-node@${INSTANCE}.service" 2>/dev/null || true
systemctl disable "octez-node@${INSTANCE}.service" 2>/dev/null || true
rm -f "/etc/systemd/system/octez-node@${INSTANCE}.service" || true
systemctl daemon-reload

# Create external systemd service
echo "Creating external systemd service..."
create_external_service "node" "$INSTANCE" "$DATA_DIR" "$RPC_ADDR" "shadownet"

# Enable and start it
systemctl enable "octez-node@${INSTANCE}.service"
systemctl start "octez-node@${INSTANCE}.service"

# Wait a bit for it to be detected
sleep 2

# Verify it's detected as external
echo "Checking if external service is detected..."
if ! external_service_detected "$INSTANCE"; then
	echo "ERROR: External service not detected"
	om list 2>&1
	exit 1
fi

echo "External service detected correctly"

# Verify it shows up with 'external' marker when using --all or --external
if ! om list --external 2>&1 | grep -q "$INSTANCE"; then
	echo "ERROR: External service not marked as external"
	om list --external 2>&1
	exit 1
fi

# Cleanup
systemctl stop "octez-node@${INSTANCE}.service" || true
systemctl disable "octez-node@${INSTANCE}.service" || true
rm -f "/etc/systemd/system/octez-node@${INSTANCE}.service" || true
systemctl daemon-reload
rm -rf "$DATA_DIR"

echo "Detect service-based test passed"
