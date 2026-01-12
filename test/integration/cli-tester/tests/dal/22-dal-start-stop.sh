#!/bin/bash
# Test: DAL node start/stop lifecycle with running node
set -euo pipefail
source /tests/lib.sh

NODE_INSTANCE="test-dal-lifecycle-node"
DAL_INSTANCE="test-dal-lifecycle"
NODE_RPC="127.0.0.1:18750"
NODE_NET="0.0.0.0:19770"
DAL_RPC="127.0.0.1:10750"
DAL_NET="0.0.0.0:11750"

echo "Test: DAL node start/stop lifecycle"

cleanup_instance "$DAL_INSTANCE" || true
cleanup_instance "$NODE_INSTANCE" || true

# Install node with snapshot
echo "Installing node with snapshot..."
om install-node \
    --instance "$NODE_INSTANCE" \
    --network shadownet \
    --snapshot \
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

# Identity generation can take time - wait for RPC
if ! wait_for_node_ready "$NODE_RPC" 60; then
    echo "ERROR: Node RPC not ready"
    show_service_logs "node" "$NODE_INSTANCE" 50
    exit 1
fi
echo "Node is ready"

# Install DAL node
echo "Installing DAL node..."
om install-dal-node \
    --instance "$DAL_INSTANCE" \
    --node-instance "$NODE_INSTANCE" \
    --rpc-addr "$DAL_RPC" \
    --net-addr "$DAL_NET" \
    --service-user tezos \
    --no-enable 2>&1

# Start DAL node
echo "Starting DAL node..."
om instance "$DAL_INSTANCE" start

# Wait for DAL service to be active
if ! wait_for_service_active "dal-node" "$DAL_INSTANCE" 30; then
    echo "ERROR: DAL service did not start"
    show_service_logs "dal-node" "$DAL_INSTANCE" 50
    exit 1
fi
echo "DAL service is active"

# Give it a moment to initialize
sleep 5

# Check DAL is still running (didn't crash immediately)
if ! service_is_active "dal-node" "$DAL_INSTANCE"; then
    echo "ERROR: DAL service crashed after startup"
    show_service_logs "dal-node" "$DAL_INSTANCE" 50
    exit 1
fi
echo "DAL service stable"

# Stop DAL node
echo "Stopping DAL node..."
om instance "$DAL_INSTANCE" stop

if ! wait_for_service_stopped "dal-node" "$DAL_INSTANCE" 30; then
    echo "ERROR: DAL service did not stop"
    exit 1
fi
echo "DAL service stopped"

# Stop node
echo "Stopping node..."
om instance "$NODE_INSTANCE" stop
wait_for_service_stopped "node" "$NODE_INSTANCE" 30

# Cleanup
cleanup_instance "$DAL_INSTANCE"
cleanup_instance "$NODE_INSTANCE"

echo "DAL start/stop lifecycle test passed"
