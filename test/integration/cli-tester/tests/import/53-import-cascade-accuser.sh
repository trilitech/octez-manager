#!/bin/bash
# Test: Import accuser with cascade (imports node dependency)
# This test validates issue #512 fix: accusers should link to imported nodes
# instead of trying to query the stopped node's RPC endpoint
set -euo pipefail
source /tests/lib.sh

test_init "Cascade import - accuser with node dependency"

NODE_INSTANCE="cascade-accuser-node"
ACCUSER_INSTANCE="cascade-accuser"
NODE_DATA="/var/lib/octez-external/$NODE_INSTANCE"
ACCUSER_DATA="/var/lib/octez-external/$ACCUSER_INSTANCE"
NODE_RPC="127.0.0.1:$(alloc_port)"

register_instance "$NODE_INSTANCE"
register_instance "$ACCUSER_INSTANCE"
register_external_service "node" "$NODE_INSTANCE"
register_external_service "accuser" "$ACCUSER_INSTANCE"
register_external_service "node" "$ACCUSER_INSTANCE"
register_external_service "accuser" "$NODE_INSTANCE"
register_data_dir "$NODE_DATA"
register_data_dir "$ACCUSER_DATA"

# Create external node service
echo "Creating external node service..."
mkdir -p "$NODE_DATA"
inject_identity "$NODE_INSTANCE" "$NODE_DATA"
chown -R tezos:tezos "$NODE_DATA"
create_external_service "node" "$NODE_INSTANCE" "$NODE_DATA" "$NODE_RPC" "shadownet"
systemctl enable "octez-node@${NODE_INSTANCE}.service"
systemctl start "octez-node@${NODE_INSTANCE}.service"

# Wait for node to be ready
wait_for_node_ready "$NODE_RPC" 30

# Create external accuser service that depends on node
echo "Creating external accuser service that depends on node..."
create_external_service "accuser" "$ACCUSER_INSTANCE" "$ACCUSER_DATA" "" "shadownet" "http://$NODE_RPC" "$ACCUSER_DATA" "$NODE_INSTANCE" "$NODE_DATA"

# Import accuser with cascade
# Prior to fix #512, this would fail because:
# 1. Cascade import stops the node (Takeover strategy)
# 2. Accuser import tries to query node RPC to resolve network
# 3. RPC call fails with "Blank input data"
# After fix #512, accuser links to imported node and uses its network field
echo "Importing accuser with cascade (should also import node)..."
om import "octez-accuser@${ACCUSER_INSTANCE}" --cascade --network shadownet 2>&1 || {
	# Stop services to avoid long sync
	systemctl stop "octez-node@${NODE_INSTANCE}.service" 2>/dev/null || true
	systemctl stop "octez-accuser@${ACCUSER_INSTANCE}.service" 2>/dev/null || true
	echo "ERROR: Import command failed"
	echo "This indicates the accuser could not link to the imported node"
	om list 2>&1
	exit 1
}

# Verify both node and accuser are now managed
if ! service_is_managed "$NODE_INSTANCE"; then
	echo "ERROR: Node should be imported as part of cascade"
	om list 2>&1
	exit 1
fi

if ! service_is_managed "$ACCUSER_INSTANCE"; then
	echo "ERROR: Accuser should be imported"
	om list 2>&1
	exit 1
fi

# Verify the accuser service has depends_on pointing to the node
echo "Verifying accuser depends_on node..."
ACCUSER_JSON=$(om show "$ACCUSER_INSTANCE" --json 2>/dev/null || echo "{}")
DEPENDS_ON=$(echo "$ACCUSER_JSON" | jq -r '.depends_on // empty')

if [ -z "$DEPENDS_ON" ]; then
	echo "ERROR: Accuser should have depends_on set to node instance"
	echo "This indicates the fix for #512 did not work correctly"
	om show "$ACCUSER_INSTANCE"
	exit 1
fi

if [ "$DEPENDS_ON" != "$NODE_INSTANCE" ]; then
	echo "ERROR: Accuser depends_on='$DEPENDS_ON', expected '$NODE_INSTANCE'"
	om show "$ACCUSER_INSTANCE"
	exit 1
fi

echo "Accuser correctly linked to node (depends_on='$DEPENDS_ON')"

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

# Start the accuser
echo "Starting accuser..."
om instance "$ACCUSER_INSTANCE" start

# Wait for accuser to become active
if ! wait_for_service_active "accuser" "$ACCUSER_INSTANCE" 30; then
	echo "ERROR: Accuser service did not start after import"
	show_service_logs "accuser" "$ACCUSER_INSTANCE" 50
	exit 1
fi

echo "Accuser is running"

# Give services a moment to stabilize
sleep 5

# Verify services are still active (didn't crash immediately)
if ! service_is_active "node" "$NODE_INSTANCE"; then
	echo "ERROR: Node service crashed after starting"
	show_service_logs "node" "$NODE_INSTANCE" 50
	exit 1
fi

if ! service_is_active "accuser" "$ACCUSER_INSTANCE"; then
	echo "ERROR: Accuser service crashed after starting"
	show_service_logs "accuser" "$ACCUSER_INSTANCE" 50
	exit 1
fi

# Stop services to avoid long sync
systemctl stop "octez-accuser@${ACCUSER_INSTANCE}.service" 2>/dev/null || true
systemctl stop "octez-node@${NODE_INSTANCE}.service" 2>/dev/null || true

echo "Cascade import accuser test passed - services imported and started successfully"
