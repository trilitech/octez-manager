#!/bin/bash
# Test: Import node with cascade (imports dependent baker and accuser)
# This tests the reverse direction: node → dependents
set -euo pipefail
source /tests/lib.sh

test_init "Cascade import - node with dependents (baker + accuser)"

NODE_INSTANCE="cascade-full-node"
BAKER_INSTANCE="cascade-full-baker"
ACCUSER_INSTANCE="cascade-full-accuser"
NODE_DATA="/var/lib/octez-external/$NODE_INSTANCE"
BAKER_DATA="/var/lib/octez-external/$BAKER_INSTANCE"
ACCUSER_DATA="/var/lib/octez-external/$ACCUSER_INSTANCE"
NODE_RPC="127.0.0.1:$(alloc_port)"

# Register for cleanup
register_instance "$NODE_INSTANCE"
register_instance "$BAKER_INSTANCE"
register_instance "$ACCUSER_INSTANCE"
register_external_service "node" "$NODE_INSTANCE"
register_external_service "baker" "$BAKER_INSTANCE"
register_external_service "accuser" "$ACCUSER_INSTANCE"
register_data_dir "$NODE_DATA"
register_data_dir "$BAKER_DATA"
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

# Create external baker service that depends on node
echo "Creating external baker service that depends on node..."
create_external_service "baker" "$BAKER_INSTANCE" "$BAKER_DATA" "" "shadownet" "http://$NODE_RPC" "$BAKER_DATA" "$NODE_INSTANCE" "$NODE_DATA"

# Create external accuser service that depends on node
echo "Creating external accuser service that depends on node..."
create_external_service "accuser" "$ACCUSER_INSTANCE" "$ACCUSER_DATA" "" "shadownet" "http://$NODE_RPC" "$ACCUSER_DATA" "$NODE_INSTANCE" "$NODE_DATA"

# Import node with cascade (should import node + baker + accuser)
echo "Importing node with cascade (should also import baker and accuser dependents)..."
om import "octez-node@${NODE_INSTANCE}" --cascade --network shadownet 2>&1 || {
	echo "Import command failed"
	om list 2>&1
	exit 1
}

# Verify all three services are now managed
if ! service_is_managed "$NODE_INSTANCE"; then
	echo "ERROR: Node should be imported"
	om list 2>&1
	exit 1
fi

if ! service_is_managed "$BAKER_INSTANCE"; then
	echo "ERROR: Baker should be imported as dependent"
	om list 2>&1
	exit 1
fi

if ! service_is_managed "$ACCUSER_INSTANCE"; then
	echo "ERROR: Accuser should be imported as dependent"
	om list 2>&1
	exit 1
fi

echo "All services imported successfully, now verifying they start correctly..."

# Start the node
echo "Starting node..."
om instance "$NODE_INSTANCE" start

# Wait for node to become active
if ! wait_for_service_active "node" "$NODE_INSTANCE" 30; then
	echo "ERROR: Node service did not start after import"
	show_service_logs "node" "$NODE_INSTANCE" 50
	exit 1
fi

# Wait for node RPC
if ! wait_for_node_ready "$NODE_RPC" 60; then
	echo "ERROR: Node RPC not ready"
	show_service_logs "node" "$NODE_INSTANCE" 50
	exit 1
fi

echo "Node is running"

# Start the baker
echo "Starting baker..."
om instance "$BAKER_INSTANCE" start

if ! wait_for_service_active "baker" "$BAKER_INSTANCE" 30; then
	echo "ERROR: Baker service did not start"
	show_service_logs "baker" "$BAKER_INSTANCE" 50
	exit 1
fi

echo "Baker is running"

# Start the accuser
echo "Starting accuser..."
om instance "$ACCUSER_INSTANCE" start

if ! wait_for_service_active "accuser" "$ACCUSER_INSTANCE" 30; then
	echo "ERROR: Accuser service did not start"
	show_service_logs "accuser" "$ACCUSER_INSTANCE" 50
	exit 1
fi

echo "Accuser is running"

# Stability check
sleep 5

if ! service_is_active "node" "$NODE_INSTANCE"; then
	echo "ERROR: Node crashed"
	show_service_logs "node" "$NODE_INSTANCE" 50
	exit 1
fi

if ! service_is_active "baker" "$BAKER_INSTANCE"; then
	echo "ERROR: Baker crashed"
	show_service_logs "baker" "$BAKER_INSTANCE" 50
	exit 1
fi

if ! service_is_active "accuser" "$ACCUSER_INSTANCE"; then
	echo "ERROR: Accuser crashed"
	show_service_logs "accuser" "$ACCUSER_INSTANCE" 50
	exit 1
fi

# Stop services
systemctl stop "octez-accuser@${ACCUSER_INSTANCE}.service" 2>/dev/null || true
systemctl stop "octez-baker@${BAKER_INSTANCE}.service" 2>/dev/null || true
systemctl stop "octez-node@${NODE_INSTANCE}.service" 2>/dev/null || true

echo "Cascade import node with dependents test passed - all services imported and started successfully"
