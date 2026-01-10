#!/bin/bash
# Test: Baker installation with DAL configuration
set -euo pipefail
source /tests/lib.sh

NODE_INSTANCE="test-baker-dal-node"
BAKER_DISABLED="test-baker-dal-disabled"
BAKER_ENDPOINT="test-baker-dal-endpoint"
NODE_RPC="127.0.0.1:18763"
NODE_NET="0.0.0.0:19783"
DAL_ENDPOINT="http://127.0.0.1:10732"

echo "Test: Baker DAL configuration"

cleanup_instance "$BAKER_DISABLED" || true
cleanup_instance "$BAKER_ENDPOINT" || true
cleanup_instance "$NODE_INSTANCE" || true

# Install a node first
echo "Installing node..."
om install-node \
    --instance "$NODE_INSTANCE" \
    --network tallinnnet \
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

# Cleanup
cleanup_instance "$BAKER_DISABLED"
cleanup_instance "$BAKER_ENDPOINT"
cleanup_instance "$NODE_INSTANCE"

echo "Baker DAL config test passed"
