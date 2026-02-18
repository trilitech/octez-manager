#!/bin/bash
# Test: Purging one baker preserves shared node data for other bakers
set -euo pipefail
source /tests/lib.sh

test_init "Multiple bakers sharing node - purge one preserves data"

NODE_INSTANCE="test-multi-baker-node"
BAKER1_INSTANCE="test-baker1"
BAKER2_INSTANCE="test-baker2"
RPC_PORT=$(alloc_port)
NET_PORT=$(alloc_port)
NODE_RPC="127.0.0.1:$RPC_PORT"
NODE_NET="0.0.0.0:$NET_PORT"

register_instance "$BAKER1_INSTANCE"
register_instance "$BAKER2_INSTANCE"
register_instance "$NODE_INSTANCE"

# Install a node first
echo "Installing node..."
om install-node \
	--instance "$NODE_INSTANCE" \
	--network shadownet \
	--rpc-addr "$NODE_RPC" \
	--net-addr "$NODE_NET" \
	--service-user tezos \
	--no-enable 2>&1

# Verify node data directory exists
NODE_DATA_DIR="/var/lib/octez/$NODE_INSTANCE"
if [ ! -d "$NODE_DATA_DIR" ]; then
	echo "ERROR: Node data directory not created: $NODE_DATA_DIR"
	exit 1
fi
echo "Node data directory exists: $NODE_DATA_DIR"

# Install first baker
echo "Installing baker1..."
om install-baker \
	--instance "$BAKER1_INSTANCE" \
	--node-instance "$NODE_INSTANCE" \
	--liquidity-baking-vote pass \
	--service-user tezos \
	--no-enable 2>&1

# Install second baker on the same node
echo "Installing baker2..."
om install-baker \
	--instance "$BAKER2_INSTANCE" \
	--node-instance "$NODE_INSTANCE" \
	--liquidity-baking-vote pass \
	--service-user tezos \
	--no-enable 2>&1

# Verify both bakers are installed
if ! instance_exists "$BAKER1_INSTANCE"; then
	echo "ERROR: Baker1 instance not in registry"
	exit 1
fi
if ! instance_exists "$BAKER2_INSTANCE"; then
	echo "ERROR: Baker2 instance not in registry"
	exit 1
fi
echo "Both bakers registered"

# Now purge baker1 - this should NOT delete the node's data directory
# because baker2 still uses it
echo "Purging baker1..."
om instance "$BAKER1_INSTANCE" purge 2>&1

# Verify baker1 is removed
if instance_exists "$BAKER1_INSTANCE"; then
	echo "ERROR: Baker1 still in registry after purge"
	exit 1
fi
echo "Baker1 purged successfully"

# CRITICAL CHECK: Node data directory should still exist (baker2 still uses it)
if [ ! -d "$NODE_DATA_DIR" ]; then
	echo "ERROR: Node data directory was deleted when purging baker1!"
	echo "Baker2 still depends on this data - this is the bug we're fixing"
	exit 1
fi
echo "✓ Node data directory preserved (baker2 still uses it)"

# Verify node and baker2 still exist
if ! instance_exists "$NODE_INSTANCE"; then
	echo "ERROR: Node instance removed from registry"
	exit 1
fi
if ! instance_exists "$BAKER2_INSTANCE"; then
	echo "ERROR: Baker2 removed from registry"
	exit 1
fi
echo "✓ Node and baker2 still in registry"

# Now purge baker2 - this should STILL NOT delete the node's data
# because the node instance still exists
echo "Purging baker2..."
om instance "$BAKER2_INSTANCE" purge 2>&1

# CRITICAL CHECK: Node data directory should still exist (node instance still exists)
if [ ! -d "$NODE_DATA_DIR" ]; then
	echo "ERROR: Node data directory was deleted when purging baker2!"
	echo "Node instance still exists - this is the bug we're fixing"
	exit 1
fi
echo "✓ Node data directory preserved (node instance still exists)"

# Verify node still exists
if ! instance_exists "$NODE_INSTANCE"; then
	echo "ERROR: Node instance removed from registry"
	exit 1
fi
echo "✓ Node instance still in registry"

echo "Test passed: Multiple bakers sharing node data directory works correctly"
