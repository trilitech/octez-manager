#!/bin/bash
# Test: Basic baker installation with local node reference
set -euo pipefail
source /tests/lib.sh

test_init "Basic baker installation with local node"

NODE_INSTANCE="test-baker-node"
BAKER_INSTANCE="test-baker-basic"
RPC_PORT=$(alloc_port)
NET_PORT=$(alloc_port)
NODE_RPC="127.0.0.1:$RPC_PORT"
NODE_NET="0.0.0.0:$NET_PORT"

register_instance "$BAKER_INSTANCE"
register_instance "$NODE_INSTANCE"

# First install a node (baker requires a node)
echo "Installing node..."
om install-node \
	--instance "$NODE_INSTANCE" \
	--network shadownet \
	--rpc-addr "$NODE_RPC" \
	--net-addr "$NODE_NET" \
	--service-user tezos \
	--no-enable 2>&1

# Install baker pointing to the local node
echo "Installing baker..."
om install-baker \
	--instance "$BAKER_INSTANCE" \
	--node-instance "$NODE_INSTANCE" \
	--liquidity-baking-vote pass \
	--service-user tezos \
	--no-enable 2>&1

# Verify baker instance exists
if ! instance_exists "$BAKER_INSTANCE"; then
	echo "ERROR: Baker instance not in registry"
	exit 1
fi
echo "Baker instance registered"

# Verify env file exists
ENV_FILE="/etc/octez/instances/$BAKER_INSTANCE/node.env"
if [ ! -f "$ENV_FILE" ]; then
	echo "ERROR: Env file not found: $ENV_FILE"
	exit 1
fi
echo "Env file exists"

# Verify env file contains expected values
if ! grep -q "OCTEZ_NODE_ENDPOINT=http://$NODE_RPC" "$ENV_FILE"; then
	echo "ERROR: Node endpoint not in env file"
	cat "$ENV_FILE"
	exit 1
fi
echo "Node endpoint configured correctly"

if ! grep -q "OCTEZ_BAKER_LB_VOTE=pass" "$ENV_FILE"; then
	echo "ERROR: Liquidity baking vote not in env file"
	cat "$ENV_FILE"
	exit 1
fi
echo "Liquidity baking vote configured correctly"

if ! grep -q "OCTEZ_BAKER_NODE_MODE=local" "$ENV_FILE"; then
	echo "ERROR: Node mode not set to local"
	cat "$ENV_FILE"
	exit 1
fi
echo "Node mode configured correctly"

# Verify systemd service file exists
if ! service_exists "baker" "$BAKER_INSTANCE"; then
	echo "ERROR: Baker systemd service not found"
	exit 1
fi
echo "Systemd service exists"

echo "Baker basic install test passed"
