#!/bin/bash
# Test: Baker start/stop lifecycle with running node
set -euo pipefail
source /tests/lib.sh

NODE_INSTANCE="test-baker-lifecycle-node"
BAKER_INSTANCE="test-baker-lifecycle"
NODE_RPC="127.0.0.1:18765"
NODE_NET="0.0.0.0:19785"

echo "Test: Baker start/stop lifecycle"

cleanup_instance "$BAKER_INSTANCE" || true
cleanup_instance "$NODE_INSTANCE" || true

# Install node with snapshot
echo "Installing node with snapshot..."
om install-node \
    --instance "$NODE_INSTANCE" \
    --network shadownet \
    --snapshot \
    --snapshot-no-check \
    --snapshot-uri "$SANDBOX_URL/snapshot.rolling" \
    --rpc-addr "$NODE_RPC" \
    --net-addr "$NODE_NET" \
    --service-user tezos \
    --no-enable 2>&1

# Inject pre-generated identity to skip PoW
inject_identity "$NODE_INSTANCE"

# Start the node
echo "Starting node..."
om instance "$NODE_INSTANCE" start

# Wait for node to be ready
if ! wait_for_service_active "node" "$NODE_INSTANCE" 30; then
    echo "ERROR: Node service did not start"
    show_service_logs "node" "$NODE_INSTANCE" 50
    exit 1
fi

# Wait for node RPC
if ! wait_for_node_ready "$NODE_RPC" 60; then
    echo "ERROR: Node RPC not ready"
    show_service_logs "node" "$NODE_INSTANCE" 50
    exit 1
fi
echo "Node is ready"

# Install baker
echo "Installing baker..."
om install-baker \
    --instance "$BAKER_INSTANCE" \
    --node-instance "$NODE_INSTANCE" \
    --liquidity-baking-vote pass \
    --dal-endpoint none \
    --service-user tezos \
    --no-enable 2>&1

# Start baker
echo "Starting baker..."
om instance "$BAKER_INSTANCE" start

# Wait for baker service to be active
if ! wait_for_service_active "baker" "$BAKER_INSTANCE" 30; then
    echo "ERROR: Baker service did not start"
    show_service_logs "baker" "$BAKER_INSTANCE" 50
    exit 1
fi
echo "Baker service is active"

# Give it a moment to initialize
sleep 5

# Check baker is still running (didn't crash immediately)
if ! service_is_active "baker" "$BAKER_INSTANCE"; then
    echo "ERROR: Baker service crashed after startup"
    show_service_logs "baker" "$BAKER_INSTANCE" 50
    exit 1
fi
echo "Baker service stable"

# Stop baker
echo "Stopping baker..."
om instance "$BAKER_INSTANCE" stop

if ! wait_for_service_stopped "baker" "$BAKER_INSTANCE" 30; then
    echo "ERROR: Baker service did not stop"
    exit 1
fi
echo "Baker service stopped"

# Stop node
echo "Stopping node..."
om instance "$NODE_INSTANCE" stop
wait_for_service_stopped "node" "$NODE_INSTANCE" 30

# Cleanup
cleanup_instance "$BAKER_INSTANCE"
cleanup_instance "$NODE_INSTANCE"

echo "Baker start/stop lifecycle test passed"
