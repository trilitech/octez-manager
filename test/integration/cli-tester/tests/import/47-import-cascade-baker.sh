#!/bin/bash
# Test: Import baker with cascade (imports node dependency)
set -euo pipefail
source /tests/lib.sh

test_init "Cascade import - baker with node dependency"

NODE_INSTANCE="cascade-node"
BAKER_INSTANCE="cascade-baker"
NODE_DATA="/var/lib/octez-external/$NODE_INSTANCE"
BAKER_DATA="/var/lib/octez-external/$BAKER_INSTANCE"
NODE_RPC="127.0.0.1:$(alloc_port)"

register_instance "$NODE_INSTANCE"
register_instance "$BAKER_INSTANCE"
register_external_service "node" "$NODE_INSTANCE"
register_external_service "baker" "$BAKER_INSTANCE"
register_external_service "node" "$BAKER_INSTANCE"
register_external_service "baker" "$NODE_INSTANCE"
register_data_dir "$NODE_DATA"
register_data_dir "$BAKER_DATA"

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
create_external_service "baker" "$BAKER_INSTANCE" "$BAKER_DATA" "" "shadownet" "http://$NODE_RPC" "$BAKER_DATA" "$NODE_INSTANCE" "$NODE_DATA"

# Note: Baker service should have After= and Requires= for node
# Don't enable the baker yet - let the import command handle that
# (enabling now would create a race condition with import's enable --now)

# Import baker with cascade
echo "Importing baker with cascade (should also import node)..."
om import "octez-baker@${BAKER_INSTANCE}" --cascade --network shadownet 2>&1 || {
	echo "ERROR: Import command failed"
	# Stop services to avoid long sync
	systemctl stop "octez-node@${NODE_INSTANCE}.service" 2>/dev/null || true
	systemctl stop "octez-baker@${BAKER_INSTANCE}.service" 2>/dev/null || true
	# Clean up any partially imported instances (remove dependent first)
	om instance "$BAKER_INSTANCE" remove 2>&1 || true
	om instance "$NODE_INSTANCE" remove 2>&1 || true
	echo "Import command failed, checking what was imported..."
	om list 2>&1
	exit 1
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

echo "Services imported successfully, now verifying they start correctly..."

# Start the node
echo "Starting node..."
om instance "$NODE_INSTANCE" start

# Wait for node to become active
if ! wait_for_service_active "node" "$NODE_INSTANCE" 30; then
	echo "ERROR: Node service did not start after import"
	show_service_logs "node" "$NODE_INSTANCE" 50
	exit 1
fi

# Wait for node RPC to be ready
if ! wait_for_node_ready "$NODE_RPC" 60; then
	echo "ERROR: Node RPC not ready after import"
	show_service_logs "node" "$NODE_INSTANCE" 50
	exit 1
fi

echo "Node is running and RPC is ready"

# Start the baker
echo "Starting baker..."
om instance "$BAKER_INSTANCE" start

# Wait for baker to become active
if ! wait_for_service_active "baker" "$BAKER_INSTANCE" 30; then
	echo "ERROR: Baker service did not start after import"
	show_service_logs "baker" "$BAKER_INSTANCE" 50
	exit 1
fi

echo "Baker is running"

# Give services a moment to stabilize
sleep 5

# Verify services are still active (didn't crash immediately)
if ! service_is_active "node" "$NODE_INSTANCE"; then
	echo "ERROR: Node service crashed after starting"
	show_service_logs "node" "$NODE_INSTANCE" 50
	exit 1
fi

if ! service_is_active "baker" "$BAKER_INSTANCE"; then
	echo "ERROR: Baker service crashed after starting"
	show_service_logs "baker" "$BAKER_INSTANCE" 50
	exit 1
fi

# Stop services to avoid long sync
echo "Stopping services..."
systemctl stop "octez-baker@${BAKER_INSTANCE}.service" 2>/dev/null || true
systemctl stop "octez-node@${NODE_INSTANCE}.service" 2>/dev/null || true

# Clean up imported instances (remove dependent first)
echo "Removing imported instances..."
om instance "$BAKER_INSTANCE" remove 2>&1 || echo "WARNING: Failed to remove $BAKER_INSTANCE"
om instance "$NODE_INSTANCE" remove 2>&1 || echo "WARNING: Failed to remove $NODE_INSTANCE"

echo "Cascade import test passed - services imported and started successfully"
