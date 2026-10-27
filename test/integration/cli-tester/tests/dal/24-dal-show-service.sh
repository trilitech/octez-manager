#!/bin/bash
# Test: DAL node show-service command
set -euo pipefail
source /tests/lib.sh

test_init "DAL node show-service command"

NODE_INSTANCE="test-dal-show-node"
DAL_INSTANCE="test-dal-show"
RPC_PORT=$(alloc_port)
NET_PORT=$(alloc_port)
DAL_RPC_PORT=$(alloc_port)
DAL_NET_PORT=$(alloc_port)
NODE_RPC="127.0.0.1:$RPC_PORT"
NODE_NET="0.0.0.0:$NET_PORT"
DAL_RPC="127.0.0.1:$DAL_RPC_PORT"
DAL_NET="0.0.0.0:$DAL_NET_PORT"

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

# Install DAL node
echo "Installing DAL node..."
om install-dal-node \
	--instance "$DAL_INSTANCE" \
	--node-instance "$NODE_INSTANCE" \
	--rpc-addr "$DAL_RPC" \
	--net-addr "$DAL_NET" \
	--service-user tezos \
	--no-enable 2>&1

# Test show-service command
echo "Testing show-service command..."
OUTPUT=$(assert_command_output 0 "" "capture output" -- om instance "$DAL_INSTANCE" show-service)

# Verify output contains expected service name
if ! [[ "$OUTPUT" == *"octez-dal-node@${DAL_INSTANCE}"* ]]; then
	echo "ERROR: show-service output doesn't contain service name"
	echo "$OUTPUT"
	exit 1
fi
echo "Service name in output"

# Verify output contains loaded status
if ! grep -qi "loaded" <<<"$OUTPUT"; then
	echo "ERROR: show-service output doesn't show loaded status"
	echo "$OUTPUT"
	exit 1
fi
echo "Loaded status in output"

# Test show command (different from show-service)
echo "Testing show command..."
SHOW_OUTPUT=$(assert_command_output 0 "" "capture show_output" -- om instance "$DAL_INSTANCE" show)

# Verify show output contains instance info
if ! [[ "$SHOW_OUTPUT" == *"$DAL_INSTANCE"* ]]; then
	echo "ERROR: show output doesn't contain instance name"
	echo "$SHOW_OUTPUT"
	exit 1
fi
echo "Instance name in show output"

# Verify show output contains role
if ! grep -qi "dal" <<<"$SHOW_OUTPUT"; then
	echo "ERROR: show output doesn't contain role"
	echo "$SHOW_OUTPUT"
	exit 1
fi
echo "Role in show output"

echo "DAL show-service test passed"
