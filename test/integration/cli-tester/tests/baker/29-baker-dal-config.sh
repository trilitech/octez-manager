#!/bin/bash
# Test: Baker installation with DAL configuration
set -euo pipefail
source /tests/lib.sh

test_init "Baker DAL configuration"

NODE_INSTANCE="test-baker-dal-node"
BAKER_DISABLED="test-baker-dal-disabled"
BAKER_ENDPOINT="test-baker-dal-endpoint"
RPC_PORT=$(alloc_port)
NET_PORT=$(alloc_port)
DAL_PORT=$(alloc_port)
NODE_RPC="127.0.0.1:$RPC_PORT"
NODE_NET="0.0.0.0:$NET_PORT"
DAL_ENDPOINT="http://127.0.0.1:$DAL_PORT"

register_instance "$BAKER_DISABLED"
register_instance "$BAKER_ENDPOINT"
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

# Test DAL disabled (--dal-endpoint none)
echo "Installing baker with DAL disabled..."
om install-baker \
	--instance "$BAKER_DISABLED" \
	--node-instance "$NODE_INSTANCE" \
	--liquidity-baking-vote pass \
	--dal-endpoint none \
	--service-user tezos \
	--no-enable 2>&1

ENV_FILE="/etc/octez/instances/$BAKER_DISABLED/node.env"
if ! grep -q "OCTEZ_DAL_CONFIG=disabled" "$ENV_FILE"; then
	echo "ERROR: DAL not marked as disabled"
	cat "$ENV_FILE"
	exit 1
fi
echo "DAL disabled configured correctly"

# Test DAL with custom endpoint
echo "Installing baker with DAL endpoint..."
om install-baker \
	--instance "$BAKER_ENDPOINT" \
	--node-instance "$NODE_INSTANCE" \
	--liquidity-baking-vote pass \
	--dal-endpoint "$DAL_ENDPOINT" \
	--service-user tezos \
	--no-enable 2>&1

ENV_FILE="/etc/octez/instances/$BAKER_ENDPOINT/node.env"
if ! grep -q "OCTEZ_DAL_CONFIG=$DAL_ENDPOINT" "$ENV_FILE"; then
	echo "ERROR: DAL endpoint not configured"
	cat "$ENV_FILE"
	exit 1
fi
echo "DAL endpoint configured correctly"

echo "Baker DAL config test passed"
