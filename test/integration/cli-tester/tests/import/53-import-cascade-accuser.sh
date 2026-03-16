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

# Create external accuser data dir
mkdir -p "$ACCUSER_DATA"
chown -R tezos:tezos "$ACCUSER_DATA"

# Create external accuser service that depends on node
# Pass node_instance as $8 so the external unit gets the correct Requires= directive
echo "Creating external accuser service that depends on node..."
create_external_service "accuser" "$ACCUSER_INSTANCE" "$ACCUSER_DATA" "" "shadownet" "http://$NODE_RPC" "" "$NODE_INSTANCE"
systemctl daemon-reload

# Import accuser with cascade using clone strategy.
# Clone keeps the original node running so the managed accuser can connect to
# the node RPC immediately, avoiding the race between node startup and accuser.
# Prior to fix #512, this would fail because:
# 1. Accuser import tries to query node RPC to resolve network
# 2. With clone, the external node stays running so RPC is always reachable
# After fix #512, accuser links to imported node via depends_on field
echo "Importing accuser with cascade (should also import node)..."
om import "octez-accuser@${ACCUSER_INSTANCE}" --cascade --strategy clone --network shadownet 2>&1 || {
	echo "ERROR: Import command failed"
	echo "This indicates the accuser could not be imported in cascade"
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

# Verify the accuser service has depends_on pointing to the node.
# Read from the service registry JSON directly (om show has no --json flag).
ACCUSER_JSON="/etc/octez_manager/services/${ACCUSER_INSTANCE}.json"
if [ ! -f "$ACCUSER_JSON" ]; then
	echo "ERROR: Accuser service registry file not found: $ACCUSER_JSON"
	exit 1
fi

DEPENDS_ON=$(jq -r '.depends_on // empty' "$ACCUSER_JSON")

if [ -z "$DEPENDS_ON" ]; then
	echo "ERROR: Accuser should have depends_on set to node instance"
	echo "This indicates the fix for #512 did not work correctly"
	jq . "$ACCUSER_JSON"
	exit 1
fi

if [ "$DEPENDS_ON" != "$NODE_INSTANCE" ]; then
	echo "ERROR: Accuser depends_on='$DEPENDS_ON', expected '$NODE_INSTANCE'"
	jq . "$ACCUSER_JSON"
	exit 1
fi

echo "Accuser correctly linked to node (depends_on='$DEPENDS_ON')"
echo "Cascade import accuser test passed"
