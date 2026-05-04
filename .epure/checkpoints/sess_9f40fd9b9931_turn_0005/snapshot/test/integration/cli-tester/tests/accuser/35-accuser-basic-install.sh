#!/bin/bash
# Test: Basic accuser installation with local node reference
set -euo pipefail
source /tests/lib.sh

test_init "Basic accuser installation"

NODE_INSTANCE="test-accuser-basic-node"
ACCUSER_INSTANCE="test-accuser-basic"
RPC_PORT=$(alloc_port)
NET_PORT=$(alloc_port)
NODE_RPC="127.0.0.1:$RPC_PORT"
NODE_NET="0.0.0.0:$NET_PORT"

register_instance "$ACCUSER_INSTANCE"
register_instance "$NODE_INSTANCE"

# Install a node first (accuser needs a node reference)
echo "Installing node..."
om install-node \
	--instance "$NODE_INSTANCE" \
	--network shadownet \
	--rpc-addr "$NODE_RPC" \
	--net-addr "$NODE_NET" \
	--service-user tezos \
	--no-enable 2>&1

# Install accuser with node reference
echo "Installing accuser..."
om install-accuser \
	--instance "$ACCUSER_INSTANCE" \
	--node-instance "$NODE_INSTANCE" \
	--service-user tezos \
	--no-enable 2>&1

# Verify instance is in registry
if ! instance_exists "$ACCUSER_INSTANCE"; then
	echo "ERROR: Accuser instance not in registry"
	exit 1
fi
echo "Accuser instance registered"

# Verify env file exists
ENV_FILE="/etc/octez/instances/$ACCUSER_INSTANCE/node.env"
if [ ! -f "$ENV_FILE" ]; then
	echo "ERROR: Env file not found at $ENV_FILE"
	exit 1
fi
echo "Env file exists"

# Verify node endpoint is configured
if ! grep -q "OCTEZ_NODE_ENDPOINT=http://$NODE_RPC" "$ENV_FILE"; then
	echo "ERROR: Node endpoint not in env file"
	cat "$ENV_FILE"
	exit 1
fi
echo "Node endpoint configured"

# Verify systemd service exists
if ! service_exists "accuser" "$ACCUSER_INSTANCE"; then
	echo "ERROR: Systemd service not found"
	exit 1
fi
echo "Systemd service exists"

echo "Accuser basic install test passed"
