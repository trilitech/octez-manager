#!/bin/bash
# Test: Accuser show-service command
set -euo pipefail
source /tests/lib.sh

NODE_INSTANCE="test-accuser-show-node"
ACCUSER_INSTANCE="test-accuser-show"
NODE_RPC="127.0.0.1:18773"
NODE_NET="0.0.0.0:19793"

echo "Test: Accuser show-service command"

cleanup_instance "$ACCUSER_INSTANCE" || true
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

# Install accuser
echo "Installing accuser..."
om install-accuser \
    --instance "$ACCUSER_INSTANCE" \
    --node-instance "$NODE_INSTANCE" \
    --service-user tezos \
    --no-enable 2>&1

# Test show-service command
echo "Testing show-service command..."
OUTPUT=$(om instance "$ACCUSER_INSTANCE" show-service 2>&1)

# Verify output contains expected service name
if ! echo "$OUTPUT" | grep -q "octez-accuser@${ACCUSER_INSTANCE}"; then
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
SHOW_OUTPUT=$(om instance "$ACCUSER_INSTANCE" show 2>&1)

# Verify show output contains instance info
if ! echo "$SHOW_OUTPUT" | grep -q "$ACCUSER_INSTANCE"; then
    echo "ERROR: show output doesn't contain instance name"
    echo "$SHOW_OUTPUT"
    exit 1
fi
echo "Instance name in show output"

# Verify show output contains role
if ! echo "$SHOW_OUTPUT" | grep -qi "accuser"; then
    echo "ERROR: show output doesn't contain role"
    echo "$SHOW_OUTPUT"
    exit 1
fi
echo "Role in show output"

# Cleanup
cleanup_instance "$ACCUSER_INSTANCE"
cleanup_instance "$NODE_INSTANCE"

echo "Accuser show-service test passed"
