#!/bin/bash
# Test: Basic octez-index installation with local node reference
set -euo pipefail
source /tests/lib.sh

test_init "Basic octez-index installation with local node"

NODE_INSTANCE="test-index-node"
INDEX_INSTANCE="test-index-basic"
RPC_PORT=$(alloc_port)
NET_PORT=$(alloc_port)
INDEX_RPC_PORT=$(alloc_port)
NODE_RPC="127.0.0.1:$RPC_PORT"
NODE_NET="0.0.0.0:$NET_PORT"
INDEX_RPC="127.0.0.1:$INDEX_RPC_PORT"

register_instance "$INDEX_INSTANCE"
register_instance "$NODE_INSTANCE"

# Install a node (index requires a node endpoint)
echo "Installing node '$NODE_INSTANCE'..."
om install-node \
	--instance "$NODE_INSTANCE" \
	--network shadownet \
	--rpc-addr "$NODE_RPC" \
	--net-addr "$NODE_NET" \
	--service-user tezos \
	--no-enable

# Install octez-index pointing to the local node
echo "Installing index '$INDEX_INSTANCE'..."
om install-index \
	--instance "$INDEX_INSTANCE" \
	--node-instance "$NODE_INSTANCE" \
	--rpc-addr "$INDEX_RPC" \
	--watched-address tz1burnburnburnburnburnburnburjAYjjX \
	--service-user tezos \
	--no-enable

# Verify index instance is registered
if ! instance_exists "$INDEX_INSTANCE"; then
	echo "ERROR: Index instance not in registry"
	exit 1
fi
echo "Index instance registered"

# Verify env file exists and contains expected values
if [ "$(id -u)" -eq 0 ]; then
	ENV_FILE="/etc/octez/instances/$INDEX_INSTANCE/node.env"
else
	ENV_FILE="$HOME/.config/octez/instances/$INDEX_INSTANCE/node.env"
fi
if [ ! -f "$ENV_FILE" ]; then
	echo "ERROR: Env file not found: $ENV_FILE"
	exit 1
fi
echo "Env file exists: $ENV_FILE"

if ! grep -q "OCTEZ_INDEXER_DIR=" "$ENV_FILE"; then
	echo "ERROR: OCTEZ_INDEXER_DIR not in env file"
	exit 1
fi
echo "OCTEZ_INDEXER_DIR configured"

if ! grep -q "OCTEZ_NODE_ENDPOINT=http://$NODE_RPC" "$ENV_FILE"; then
	echo "ERROR: Node endpoint not correctly configured (expected http://$NODE_RPC)"
	cat "$ENV_FILE"
	exit 1
fi
echo "OCTEZ_NODE_ENDPOINT configured correctly"

if ! grep -q "OCTEZ_INDEX_RPC_ADDR=$INDEX_RPC" "$ENV_FILE"; then
	echo "ERROR: Index RPC address not in env file (expected $INDEX_RPC)"
	cat "$ENV_FILE"
	exit 1
fi
echo "OCTEZ_INDEX_RPC_ADDR configured correctly"

# Verify systemd service exists
if ! service_exists "index" "$INDEX_INSTANCE"; then
	echo "ERROR: Index systemd service not found"
	exit 1
fi
echo "Systemd service exists"

# Verify dependency on node is set
if ! systemctl cat "octez-index@$INDEX_INSTANCE.service" 2>/dev/null | grep -q "BindsTo=octez-node@$NODE_INSTANCE.service"; then
	echo "WARN: BindsTo dependency on node not found (may be in drop-in)"
fi

echo "octez-index basic install test passed"
