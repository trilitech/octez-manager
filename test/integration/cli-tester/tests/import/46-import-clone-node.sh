#!/bin/bash
# Test: Import with clone strategy
set -euo pipefail
source /tests/lib.sh

EXTERNAL_INSTANCE="clone-source"
CLONED_INSTANCE="clone-dest"
EXTERNAL_DATA="/var/lib/octez-external/$EXTERNAL_INSTANCE"
RPC_ADDR="127.0.0.1:18746"

# Cleanup function
cleanup_test() {
	systemctl stop "octez-node@${EXTERNAL_INSTANCE}.service" 2>/dev/null || true
	systemctl disable "octez-node@${EXTERNAL_INSTANCE}.service" 2>/dev/null || true
	rm -f "/etc/systemd/system/octez-node@${EXTERNAL_INSTANCE}.service" || true
	cleanup_instance "$EXTERNAL_INSTANCE" || true
	cleanup_instance "$CLONED_INSTANCE" || true
	rm -rf "$EXTERNAL_DATA" || true
	systemctl daemon-reload || true
}

# Ensure cleanup on exit
trap cleanup_test EXIT

echo "Test: Import node with clone strategy"

# Initial cleanup
cleanup_test
rm -rf "$EXTERNAL_DATA" || true
systemctl stop "octez-node@${EXTERNAL_INSTANCE}.service" 2>/dev/null || true
systemctl disable "octez-node@${EXTERNAL_INSTANCE}.service" 2>/dev/null || true
rm -f "/etc/systemd/system/octez-node@${EXTERNAL_INSTANCE}.service" || true
systemctl daemon-reload

# Create external service
echo "Creating external systemd service..."
mkdir -p "$EXTERNAL_DATA"
inject_identity "$EXTERNAL_INSTANCE" "$EXTERNAL_DATA"
chown -R tezos:tezos "$EXTERNAL_DATA"
create_external_service "node" "$EXTERNAL_INSTANCE" "$EXTERNAL_DATA" "$RPC_ADDR" "shadownet"
systemctl enable "octez-node@${EXTERNAL_INSTANCE}.service"
systemctl start "octez-node@${EXTERNAL_INSTANCE}.service"
sleep 2

# Import with clone strategy
echo "Importing with clone strategy..."
# Note: Clone will fail to start due to same data-dir/port conflicts
# This is expected - clone strategy copies config but requires manual adjustment of ports/paths
om import "octez-node@${EXTERNAL_INSTANCE}" --strategy clone --as "$CLONED_INSTANCE" 2>&1 || {
	echo "Clone import may fail to start (expected due to port/data-dir conflicts)"
}
# Stop services to avoid long sync
systemctl stop "octez-node@${EXTERNAL_INSTANCE}.service" 2>/dev/null || true
systemctl stop "octez-node@${CLONED_INSTANCE}.service" 2>/dev/null || true

# Verify cloned service is managed (even if it failed to start)
if ! service_is_managed "$CLONED_INSTANCE"; then
	echo "ERROR: Cloned service not found in managed instances"
	om list 2>&1
	exit 1
fi

# Verify original service is still enabled (clone strategy should preserve it)
if ! systemctl is-enabled "octez-node@${EXTERNAL_INSTANCE}.service" >/dev/null 2>&1; then
	echo "ERROR: Original service should still be enabled after clone"
	exit 1
fi

# Note: We don't check if services are active because clone with same data-dir/port will conflict
# The clone strategy correctly preserves the original service and creates a managed copy
# Users would need to adjust data-dir/rpc-addr in the cloned instance before starting it

echo "Service successfully cloned"

echo "Clone import test passed"
