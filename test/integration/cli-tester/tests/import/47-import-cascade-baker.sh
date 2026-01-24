#!/bin/bash
# Test: Import baker with cascade (imports node dependency)
set -euo pipefail
source /tests/lib.sh

NODE_INSTANCE="cascade-node"
BAKER_INSTANCE="cascade-baker"
NODE_DATA="/var/lib/octez-external/$NODE_INSTANCE"
BAKER_DATA="/var/lib/octez-external/$BAKER_INSTANCE"
NODE_RPC="127.0.0.1:18747"

echo "Test: Cascade import - baker with node dependency"

# Cleanup
cleanup_instance "$NODE_INSTANCE" || true
cleanup_instance "$BAKER_INSTANCE" || true
rm -rf "$NODE_DATA" "$BAKER_DATA" || true
for inst in "$NODE_INSTANCE" "$BAKER_INSTANCE"; do
	for role in node baker; do
		systemctl stop "octez-${role}@${inst}.service" 2>/dev/null || true
		systemctl disable "octez-${role}@${inst}.service" 2>/dev/null || true
		rm -f "/etc/systemd/system/octez-${role}@${inst}.service" || true
	done
done
systemctl daemon-reload

# Create external node service
echo "Creating external node service..."
mkdir -p "$NODE_DATA"
inject_identity "$NODE_INSTANCE" "$NODE_DATA"
chown -R tezos:tezos "$NODE_DATA"
create_external_service "node" "$NODE_INSTANCE" "$NODE_DATA" "$NODE_RPC" "shadownet"
systemctl enable "octez-node@${NODE_INSTANCE}.service"
systemctl start "octez-node@${NODE_INSTANCE}.service"

# Wait for node to be actually ready before creating baker
wait_for_node_ready "$NODE_RPC" 30

# Create external baker service that depends on node
echo "Creating external baker service that depends on node..."
create_external_service "baker" "$BAKER_INSTANCE" "$BAKER_DATA" "" "shadownet" "http://$NODE_RPC"

# Note: Baker service should have After= and Requires= for node
# Don't enable the baker yet - let the import command handle that
# (enabling now would create a race condition with import's enable --now)

# Import baker with cascade
echo "Importing baker with cascade (should also import node)..."
om import "octez-baker@${BAKER_INSTANCE}" --cascade --network shadownet 2>&1 || {
	# Stop services to avoid long sync
	systemctl stop "octez-node@${NODE_INSTANCE}.service" 2>/dev/null || true
	systemctl stop "octez-baker@${BAKER_INSTANCE}.service" 2>/dev/null || true
	echo "Import command failed, checking what was imported..."
	om list 2>&1
}

# Verify both node and baker are now managed
if ! service_is_managed "$NODE_INSTANCE"; then
	echo "ERROR: Node should be imported as part of cascade"
	om list 2>&1
	exit 1
fi

if ! service_is_managed "$BAKER_INSTANCE"; then
	echo "ERROR: Baker should be imported"
	om list 2>&1
	exit 1
fi

echo "Cascade import successful"

# Cleanup
cleanup_instance "$BAKER_INSTANCE"
cleanup_instance "$NODE_INSTANCE"
rm -rf "$NODE_DATA" "$BAKER_DATA"

echo "Cascade import test passed"
