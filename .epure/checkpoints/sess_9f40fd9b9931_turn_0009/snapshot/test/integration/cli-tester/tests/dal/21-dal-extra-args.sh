#!/bin/bash
# Test: DAL node installation with extra arguments
set -euo pipefail
source /tests/lib.sh

test_init "DAL node installation with extra arguments"

NODE_INSTANCE="test-dal-extra-node"
DAL_INSTANCE="test-dal-extra"
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

# Install DAL node with extra args
echo "Installing DAL node with extra args..."
om install-dal-node \
	--instance "$DAL_INSTANCE" \
	--node-instance "$NODE_INSTANCE" \
	--rpc-addr "$DAL_RPC" \
	--net-addr "$DAL_NET" \
	--service-user tezos \
	--extra-arg="--metrics-addr=0.0.0.0:9933" \
	--no-enable 2>&1

# Verify env file contains extra args
ENV_FILE="/etc/octez/instances/$DAL_INSTANCE/node.env"

if ! grep -q "OCTEZ_SERVICE_ARGS=.*--metrics-addr=0.0.0.0:9933" "$ENV_FILE"; then
	echo "ERROR: Extra args not in env file"
	cat "$ENV_FILE"
	exit 1
fi
echo "Extra args configured correctly"

# Cleanup and test with multiple extra args
cleanup_instance "$DAL_INSTANCE"

echo "Installing DAL node with multiple extra args..."
om install-dal-node \
	--instance "$DAL_INSTANCE" \
	--node-instance "$NODE_INSTANCE" \
	--rpc-addr "$DAL_RPC" \
	--net-addr "$DAL_NET" \
	--service-user tezos \
	--extra-arg="--metrics-addr=0.0.0.0:9933" \
	--extra-arg="--log-output=stdout" \
	--no-enable 2>&1

# Verify both extra args are present
if ! grep -q "OCTEZ_SERVICE_ARGS=.*--metrics-addr=0.0.0.0:9933" "$ENV_FILE"; then
	echo "ERROR: First extra arg not in env file"
	cat "$ENV_FILE"
	exit 1
fi

if ! grep -q "OCTEZ_SERVICE_ARGS=.*--log-output=stdout" "$ENV_FILE"; then
	echo "ERROR: Second extra arg not in env file"
	cat "$ENV_FILE"
	exit 1
fi
echo "Multiple extra args configured correctly"

echo "DAL extra args test passed"
