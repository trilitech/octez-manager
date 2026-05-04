#!/bin/bash
# Test: DAL node installation with custom RPC and net addresses
set -euo pipefail
source /tests/lib.sh

test_init "DAL node installation with custom addresses"

NODE_INSTANCE="test-dal-custom-node"
DAL_INSTANCE="test-dal-custom"
RPC_PORT=$(alloc_port)
NET_PORT=$(alloc_port)
CUSTOM_DAL_RPC_PORT=$(alloc_port)
CUSTOM_DAL_NET_PORT=$(alloc_port)
NODE_RPC="127.0.0.1:$RPC_PORT"
NODE_NET="0.0.0.0:$NET_PORT"
# Non-default addresses for DAL
CUSTOM_DAL_RPC="0.0.0.0:$CUSTOM_DAL_RPC_PORT"
CUSTOM_DAL_NET="127.0.0.1:$CUSTOM_DAL_NET_PORT"

register_instance "$DAL_INSTANCE"
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

# Install DAL node with custom addresses
echo "Installing DAL node with custom addresses..."
om install-dal-node \
	--instance "$DAL_INSTANCE" \
	--node-instance "$NODE_INSTANCE" \
	--rpc-addr "$CUSTOM_DAL_RPC" \
	--net-addr "$CUSTOM_DAL_NET" \
	--service-user tezos \
	--no-enable 2>&1

# Verify env file contains custom addresses
ENV_FILE="/etc/octez/instances/$DAL_INSTANCE/node.env"

if ! grep -q "OCTEZ_DAL_RPC_ADDR=$CUSTOM_DAL_RPC" "$ENV_FILE"; then
	echo "ERROR: Custom RPC address not in env file"
	cat "$ENV_FILE"
	exit 1
fi
echo "Custom RPC address configured: $CUSTOM_DAL_RPC"

if ! grep -q "OCTEZ_DAL_NET_ADDR=$CUSTOM_DAL_NET" "$ENV_FILE"; then
	echo "ERROR: Custom net address not in env file"
	cat "$ENV_FILE"
	exit 1
fi
echo "Custom net address configured: $CUSTOM_DAL_NET"

echo "DAL custom addresses test passed"
