#!/bin/bash
# Test: Purging baker preserves node's blockchain data
set -euo pipefail
source /tests/lib.sh

test_init "Purging baker preserves node blockchain data"

NODE_INSTANCE="test-purge-node"
BAKER_INSTANCE="test-purge-baker"
RPC_PORT=$(alloc_port)
NET_PORT=$(alloc_port)
NODE_RPC="127.0.0.1:$RPC_PORT"
NODE_NET="0.0.0.0:$NET_PORT"

register_instance "$BAKER_INSTANCE"
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

# Install baker pointing to the node
echo "Installing baker..."
om install-baker \
	--instance "$BAKER_INSTANCE" \
	--node-instance "$NODE_INSTANCE" \
	--liquidity-baking-vote pass \
	--service-user tezos \
	--no-enable 2>&1

# Verify baker is installed
if ! instance_exists "$BAKER_INSTANCE"; then
	echo "ERROR: Baker instance not in registry"
	exit 1
fi
echo "Baker instance registered"

# Now purge the baker - this should NOT delete the node's data directory
echo "Purging baker..."
om instance "$BAKER_INSTANCE" purge 2>&1

# Verify baker is removed
if instance_exists "$BAKER_INSTANCE"; then
	echo "ERROR: Baker still in registry after purge"
	exit 1
fi
echo "Baker purged successfully"

# CRITICAL CHECK: Node data directory should still exist
if [ ! -d "$NODE_DATA_DIR" ]; then
	echo "ERROR: Node data directory was deleted when purging baker!"
	echo "This is the bug we're fixing - baker purge should not delete node's blockchain data"
	exit 1
fi
echo "✓ Node data directory preserved after baker purge"

# Verify node instance still exists in registry
if ! instance_exists "$NODE_INSTANCE"; then
	echo "ERROR: Node instance removed from registry"
	exit 1
fi
echo "✓ Node instance still in registry"

echo "Test passed: Baker purge preserves node blockchain data"
