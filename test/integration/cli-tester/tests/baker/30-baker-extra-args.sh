#!/bin/bash
# Test: Baker installation with extra arguments
set -euo pipefail
source /tests/lib.sh

NODE_INSTANCE="test-baker-extra-node"
BAKER_INSTANCE="test-baker-extra"
NODE_RPC="127.0.0.1:18764"
NODE_NET="0.0.0.0:19784"

echo "Test: Baker installation with extra arguments"

cleanup_instance "$BAKER_INSTANCE" || true
cleanup_instance "$NODE_INSTANCE" || true

# Install a node first
echo "Installing node..."
om install-node \
    --instance "$NODE_INSTANCE" \
    --network shadownet \
    --rpc-addr "$NODE_RPC" \
    --net-addr "$NODE_NET" \
    --service-user tezos \
    --no-enable 2>&1

# Install baker with extra args
echo "Installing baker with extra args..."
om install-baker \
    --instance "$BAKER_INSTANCE" \
    --node-instance "$NODE_INSTANCE" \
    --liquidity-baking-vote pass \
    --extra-arg="--keep-alive" \
    --service-user tezos \
    --no-enable 2>&1

# Verify env file contains extra args
ENV_FILE="/etc/octez/instances/$BAKER_INSTANCE/node.env"

if ! grep -q "OCTEZ_BAKER_EXTRA_ARGS=.*--keep-alive" "$ENV_FILE"; then
    echo "ERROR: Extra args not in env file"
    cat "$ENV_FILE"
    exit 1
fi
echo "Extra args configured correctly"

# Test with multiple extra args
cleanup_instance "$BAKER_INSTANCE"

echo "Installing baker with multiple extra args..."
om install-baker \
    --instance "$BAKER_INSTANCE" \
    --node-instance "$NODE_INSTANCE" \
    --liquidity-baking-vote pass \
    --extra-arg="--keep-alive" \
    --extra-arg="--operations-pool=file:/tmp/ops.json" \
    --service-user tezos \
    --no-enable 2>&1

if ! grep -q "OCTEZ_BAKER_EXTRA_ARGS=.*--keep-alive" "$ENV_FILE"; then
    echo "ERROR: First extra arg not in env file"
    cat "$ENV_FILE"
    exit 1
fi

if ! grep -q "OCTEZ_BAKER_EXTRA_ARGS=.*--operations-pool" "$ENV_FILE"; then
    echo "ERROR: Second extra arg not in env file"
    cat "$ENV_FILE"
    exit 1
fi
echo "Multiple extra args configured correctly"

# Cleanup
cleanup_instance "$BAKER_INSTANCE"
cleanup_instance "$NODE_INSTANCE"

echo "Baker extra args test passed"
