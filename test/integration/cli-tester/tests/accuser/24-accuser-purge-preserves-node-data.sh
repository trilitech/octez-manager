#!/bin/bash
# Test: Purging accuser preserves node's blockchain data
set -euo pipefail
source /tests/lib.sh

test_init "Purging accuser preserves node blockchain data"

NODE_INSTANCE="test-accuser-purge-node"
ACCUSER_INSTANCE="test-accuser-purge"
RPC_PORT=$(alloc_port)
NET_PORT=$(alloc_port)
NODE_RPC="127.0.0.1:$RPC_PORT"
NODE_NET="0.0.0.0:$NET_PORT"

register_instance "$ACCUSER_INSTANCE"
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

# Install accuser pointing to the node
echo "Installing accuser..."
om install-accuser \
	--instance "$ACCUSER_INSTANCE" \
	--node-instance "$NODE_INSTANCE" \
	--service-user tezos \
	--no-enable 2>&1

# Verify accuser is installed
if ! instance_exists "$ACCUSER_INSTANCE"; then
	echo "ERROR: Accuser instance not in registry"
	exit 1
fi
echo "Accuser instance registered"

# Now purge the accuser - this should NOT delete the node's data directory
echo "Purging accuser..."
om instance "$ACCUSER_INSTANCE" purge 2>&1

# Verify accuser is removed
if instance_exists "$ACCUSER_INSTANCE"; then
	echo "ERROR: Accuser still in registry after purge"
	exit 1
fi
echo "Accuser purged successfully"

# CRITICAL CHECK: Node data directory should still exist
if [ ! -d "$NODE_DATA_DIR" ]; then
	echo "ERROR: Node data directory was deleted when purging accuser!"
	echo "This is the bug we're fixing - accuser purge should not delete node's blockchain data"
	exit 1
fi
echo "✓ Node data directory preserved after accuser purge"

# Verify node instance still exists in registry
if ! instance_exists "$NODE_INSTANCE"; then
	echo "ERROR: Node instance removed from registry"
	exit 1
fi
echo "✓ Node instance still in registry"

echo "Test passed: Accuser purge preserves node blockchain data"
