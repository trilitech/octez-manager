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

# Create external service
echo "Creating external systemd service..."
create_external_service "node" "$INSTANCE" "$DATA_DIR" "$RPC_ADDR" "shadownet"
systemctl enable "octez-node@${INSTANCE}.service"

# Wait for it to be running

# Import with takeover strategy
echo "Importing with takeover strategy..."
om import "octez-node@${INSTANCE}" --strategy takeover 2>&1

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
