#!/bin/bash
# Test: Baker show-service command
set -euo pipefail
source /tests/lib.sh

test_init "Baker show-service command"

NODE_INSTANCE="test-baker-show-node"
BAKER_INSTANCE="test-baker-show"
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

# Install baker
echo "Installing baker..."
om install-baker \
	--instance "$BAKER_INSTANCE" \
	--node-instance "$NODE_INSTANCE" \
	--liquidity-baking-vote pass \
	--service-user tezos \
	--no-enable 2>&1

# Test show-service command
echo "Testing show-service command..."
OUTPUT=$(om instance "$BAKER_INSTANCE" show-service 2>&1)

# Verify output contains expected service name
if ! echo "$OUTPUT" | grep -q "octez-baker@${BAKER_INSTANCE}"; then
	echo "ERROR: show-service output doesn't contain service name"
	echo "$OUTPUT"
	exit 1
fi
echo "Service name in output"

# Verify output contains loaded status
if ! echo "$OUTPUT" | grep -qi "loaded"; then
	echo "ERROR: show-service output doesn't show loaded status"
	echo "$OUTPUT"
	exit 1
fi
echo "Loaded status in output"

# Test show command
echo "Testing show command..."
SHOW_OUTPUT=$(om instance "$BAKER_INSTANCE" show 2>&1)

# Verify show output contains instance info
if ! echo "$SHOW_OUTPUT" | grep -q "$BAKER_INSTANCE"; then
	echo "ERROR: show output doesn't contain instance name"
	echo "$SHOW_OUTPUT"
	exit 1
fi
echo "Instance name in show output"

# Verify show output contains role
if ! echo "$SHOW_OUTPUT" | grep -qi "baker"; then
	echo "ERROR: show output doesn't contain role"
	echo "$SHOW_OUTPUT"
	exit 1
fi
echo "Role in show output"

echo "Baker show-service test passed"
