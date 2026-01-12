#!/bin/bash
# Test: Starting baker fails gracefully when node is not running
set -euo pipefail
source /tests/lib.sh

NODE_INSTANCE="test-baker-nonode-node"
BAKER_INSTANCE="test-baker-nonode"
NODE_RPC="127.0.0.1:18782"
NODE_NET="0.0.0.0:19802"

echo "Test: Baker requires node to be running"

cleanup_instance "$BAKER_INSTANCE" || true
cleanup_instance "$NODE_INSTANCE" || true

# Install node but DON'T start it
echo "Installing node (not starting)..."
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
    --dal-endpoint none \
    --service-user tezos \
    --no-enable 2>&1

# Try to start baker - should fail with clear error
echo "Attempting to start baker (should fail)..."
if om instance "$BAKER_INSTANCE" start 2>&1; then
    echo "ERROR: Baker start should have failed when node is not running"
    exit 1
fi
echo "Baker start correctly failed"

# Verify the error message mentions the dependency
OUTPUT=$(om instance "$BAKER_INSTANCE" start 2>&1 || true)
if ! echo "$OUTPUT" | grep -qi "not running\|dependency"; then
    echo "ERROR: Error message should mention dependency not running"
    echo "Got: $OUTPUT"
    exit 1
fi
echo "Error message correctly indicates dependency issue"

# Verify baker is not running
if service_is_active "baker" "$BAKER_INSTANCE"; then
    echo "ERROR: Baker should not be running"
    exit 1
fi
echo "Baker correctly not running"

# Now start the node
echo "Starting node..."
om instance "$NODE_INSTANCE" start

if ! wait_for_service_active "node" "$NODE_INSTANCE" 30; then
    echo "ERROR: Node did not start"
    exit 1
fi

if ! wait_for_node_ready "$NODE_RPC" 180; then
    echo "ERROR: Node RPC not ready"
    show_service_logs "node" "$NODE_INSTANCE" 50
    exit 1
fi
echo "Node is ready"

# Now baker start should succeed
echo "Starting baker (should succeed now)..."
om instance "$BAKER_INSTANCE" start

if ! wait_for_service_active "baker" "$BAKER_INSTANCE" 30; then
    echo "ERROR: Baker should start when node is running"
    show_service_logs "baker" "$BAKER_INSTANCE" 50
    exit 1
fi
echo "Baker started successfully after node was running"

# Cleanup
om instance "$BAKER_INSTANCE" stop
wait_for_service_stopped "baker" "$BAKER_INSTANCE" 30
om instance "$NODE_INSTANCE" stop
wait_for_service_stopped "node" "$NODE_INSTANCE" 30

cleanup_instance "$BAKER_INSTANCE"
cleanup_instance "$NODE_INSTANCE"

echo "Baker requires node running test passed"
