#!/bin/bash
# Test: Baker installation with delegate keys
set -euo pipefail
source /tests/lib.sh

test_init "Baker installation with delegates"

NODE_INSTANCE="test-baker-del-node"
BAKER_INSTANCE="test-baker-delegates"
RPC_PORT=$(alloc_port)
NET_PORT=$(alloc_port)
NODE_RPC="127.0.0.1:$RPC_PORT"
NODE_NET="0.0.0.0:$NET_PORT"
# Test delegate addresses (these are example addresses, not real keys)
DELEGATE1="tz1VSUr8wwNhLAzempoch5d6hLRiTh8Cjcjb"
DELEGATE2="tz1aSkwEot3L2kmUvcoxzjMomb9mvBNuzFK6"

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

# Install baker with multiple delegates
echo "Installing baker with delegates..."
om install-baker \
	--instance "$BAKER_INSTANCE" \
	--node-instance "$NODE_INSTANCE" \
	--delegate "$DELEGATE1" \
	--delegate "$DELEGATE2" \
	--liquidity-baking-vote pass \
	--service-user tezos \
	--no-enable 2>&1

# Verify baker instance exists
if ! instance_exists "$BAKER_INSTANCE"; then
	echo "ERROR: Baker instance not in registry"
	exit 1
fi
echo "Baker instance registered"

# Verify env file contains delegates
ENV_FILE="/etc/octez/instances/$BAKER_INSTANCE/node.env"

# Check delegates are in the args (space-separated for command line)
if ! grep -q "OCTEZ_BAKER_DELEGATES_ARGS=.*$DELEGATE1" "$ENV_FILE"; then
	echo "ERROR: First delegate not in args"
	cat "$ENV_FILE"
	exit 1
fi
echo "First delegate in args"

if ! grep -q "OCTEZ_BAKER_DELEGATES_ARGS=.*$DELEGATE2" "$ENV_FILE"; then
	echo "ERROR: Second delegate not in args"
	cat "$ENV_FILE"
	exit 1
fi
echo "Second delegate in args"

# Check delegates CSV (comma-separated for storage)
if ! grep -q "OCTEZ_BAKER_DELEGATES_CSV=.*$DELEGATE1" "$ENV_FILE"; then
	echo "ERROR: First delegate not in CSV"
	cat "$ENV_FILE"
	exit 1
fi
echo "Delegates CSV configured"

echo "Baker with delegates test passed"
