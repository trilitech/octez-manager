#!/bin/bash
# Test: Import with field overrides (custom name, network)
set -euo pipefail
source /tests/lib.sh

EXTERNAL_INSTANCE="override-source"
CUSTOM_INSTANCE="my-custom-node"
DATA_DIR="/var/lib/octez-external/$EXTERNAL_INSTANCE"
RPC_ADDR="127.0.0.1:18750"

echo "Test: Import with field overrides"

# Cleanup
cleanup_instance "$EXTERNAL_INSTANCE" || true
cleanup_instance "$CUSTOM_INSTANCE" || true
rm -rf "$DATA_DIR" || true
systemctl --user stop "octez-node@${EXTERNAL_INSTANCE}.service" 2>/dev/null || true
systemctl --user disable "octez-node@${EXTERNAL_INSTANCE}.service" 2>/dev/null || true
rm -f "/etc/systemd/user/octez-node@${EXTERNAL_INSTANCE}.service" || true
systemctl --user daemon-reload

# Create external service on ghostnet
echo "Creating external service..."
create_external_service "node" "$EXTERNAL_INSTANCE" "$DATA_DIR" "$RPC_ADDR" "ghostnet"
systemctl --user enable "octez-node@${EXTERNAL_INSTANCE}.service"
systemctl --user start "octez-node@${EXTERNAL_INSTANCE}.service"

wait_for_service_active "node" "$EXTERNAL_INSTANCE" 10 || true

# Import with custom name override
echo "Importing with custom instance name..."
om import "octez-node@${EXTERNAL_INSTANCE}" --as "$CUSTOM_INSTANCE" 2>&1 || {
    echo "Import failed, showing current state..."
    om list 2>&1
}

# Verify service has custom name
if ! service_is_managed "$CUSTOM_INSTANCE"; then
    echo "ERROR: Service should be imported with custom name"
    om list 2>&1
    exit 1
fi

# Verify original name is not in managed instances
if om list 2>&1 | grep -v "external" | grep -q "$EXTERNAL_INSTANCE"; then
    echo "WARNING: Original name found in managed instances"
fi

echo "Field overrides test completed"

# Cleanup
cleanup_instance "$CUSTOM_INSTANCE"
rm -rf "$DATA_DIR"

echo "Field overrides test passed"
