#!/bin/bash
# Test: Baker installation with different liquidity baking vote options
set -euo pipefail
source /tests/lib.sh

NODE_INSTANCE="test-baker-lb-node"
NODE_RPC="127.0.0.1:18762"
NODE_NET="0.0.0.0:19782"

echo "Test: Baker liquidity baking vote options"

cleanup_instance "test-baker-lb-on" || true
cleanup_instance "test-baker-lb-off" || true
cleanup_instance "test-baker-lb-pass" || true
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

# Test 'on' vote
echo "Installing baker with LB vote 'on'..."
om install-baker \
    --instance "test-baker-lb-on" \
    --node-instance "$NODE_INSTANCE" \
    --liquidity-baking-vote on \
    --service-user tezos \
    --no-enable 2>&1

ENV_FILE="/etc/octez/instances/test-baker-lb-on/node.env"
if ! grep -q "OCTEZ_BAKER_LB_VOTE=on" "$ENV_FILE"; then
    echo "ERROR: LB vote 'on' not configured"
    cat "$ENV_FILE"
    exit 1
fi
echo "LB vote 'on' configured correctly"

# Test 'off' vote
echo "Installing baker with LB vote 'off'..."
om install-baker \
    --instance "test-baker-lb-off" \
    --node-instance "$NODE_INSTANCE" \
    --liquidity-baking-vote off \
    --service-user tezos \
    --no-enable 2>&1

ENV_FILE="/etc/octez/instances/test-baker-lb-off/node.env"
if ! grep -q "OCTEZ_BAKER_LB_VOTE=off" "$ENV_FILE"; then
    echo "ERROR: LB vote 'off' not configured"
    cat "$ENV_FILE"
    exit 1
fi
echo "LB vote 'off' configured correctly"

# Test 'pass' vote
echo "Installing baker with LB vote 'pass'..."
om install-baker \
    --instance "test-baker-lb-pass" \
    --node-instance "$NODE_INSTANCE" \
    --liquidity-baking-vote pass \
    --service-user tezos \
    --no-enable 2>&1

ENV_FILE="/etc/octez/instances/test-baker-lb-pass/node.env"
if ! grep -q "OCTEZ_BAKER_LB_VOTE=pass" "$ENV_FILE"; then
    echo "ERROR: LB vote 'pass' not configured"
    cat "$ENV_FILE"
    exit 1
fi
echo "LB vote 'pass' configured correctly"

# Cleanup
cleanup_instance "test-baker-lb-on"
cleanup_instance "test-baker-lb-off"
cleanup_instance "test-baker-lb-pass"
cleanup_instance "$NODE_INSTANCE"

echo "Baker liquidity vote test passed"
