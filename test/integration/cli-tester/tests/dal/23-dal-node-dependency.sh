#!/bin/bash
# Test: DAL node stops when parent node stops (BindsTo dependency)
set -euo pipefail
source /tests/lib.sh

NODE_INSTANCE="test-dal-dep-node"
DAL_INSTANCE="test-dal-dep"
NODE_RPC="127.0.0.1:18751"
NODE_NET="0.0.0.0:19771"
DAL_RPC="127.0.0.1:10751"
DAL_NET="0.0.0.0:11751"

echo "Test: DAL node dependency on parent node"

cleanup_instance "$DAL_INSTANCE" || true
cleanup_instance "$NODE_INSTANCE" || true

# Install node with snapshot
echo "Installing node..."
om install-node \
    --instance "$NODE_INSTANCE" \
    --network shadownet \
    --snapshot \
    --snapshot-uri "$SANDBOX_URL/snapshot.rolling" \
    --rpc-addr "$NODE_RPC" \
    --net-addr "$NODE_NET" \
    --service-user tezos \
    --no-enable 2>&1

# Start the node
echo "Starting node..."
om instance "$NODE_INSTANCE" start

if ! wait_for_service_active "node" "$NODE_INSTANCE" 30; then
    echo "ERROR: Node service did not start"
    exit 1
fi

# Wait for node RPC
if ! wait_for_node_ready "$NODE_RPC" 180; then
    echo "ERROR: Node RPC not ready"
    show_service_logs "node" "$NODE_INSTANCE" 50
    exit 1
fi

# Install DAL node with local node reference
echo "Installing DAL node..."
om install-dal-node \
    --instance "$DAL_INSTANCE" \
    --node-instance "$NODE_INSTANCE" \
    --rpc-addr "$DAL_RPC" \
    --net-addr "$DAL_NET" \
    --service-user tezos \
    --no-enable 2>&1

# Verify systemd dependency is configured
DROPIN="/etc/systemd/system/octez-dal-node@${DAL_INSTANCE}.service.d/override.conf"
if [ ! -f "$DROPIN" ]; then
    echo "ERROR: Drop-in override not found"
    exit 1
fi

if ! grep -q "BindsTo=octez-node@${NODE_INSTANCE}.service" "$DROPIN"; then
    echo "ERROR: BindsTo dependency not configured"
    cat "$DROPIN"
    exit 1
fi
echo "BindsTo dependency configured correctly"

if ! grep -q "After=octez-node@${NODE_INSTANCE}.service" "$DROPIN"; then
    echo "ERROR: After dependency not configured"
    cat "$DROPIN"
    exit 1
fi
echo "After dependency configured correctly"

# Start DAL node
echo "Starting DAL node..."
om instance "$DAL_INSTANCE" start

if ! wait_for_service_active "dal-node" "$DAL_INSTANCE" 30; then
    echo "ERROR: DAL service did not start"
    show_service_logs "dal-node" "$DAL_INSTANCE" 50
    exit 1
fi
echo "DAL service is active"

# Now stop the node - DAL should stop automatically due to BindsTo
echo "Stopping node (DAL should stop automatically)..."
om instance "$NODE_INSTANCE" stop

if ! wait_for_service_stopped "node" "$NODE_INSTANCE" 30; then
    echo "ERROR: Node did not stop"
    exit 1
fi
echo "Node stopped"

# DAL should have stopped automatically
sleep 2
if service_is_active "dal-node" "$DAL_INSTANCE"; then
    echo "ERROR: DAL is still running after node stopped"
    echo "BindsTo dependency not working correctly"
    exit 1
fi
echo "DAL stopped automatically (BindsTo working)"

# Cleanup
cleanup_instance "$DAL_INSTANCE"
cleanup_instance "$NODE_INSTANCE"

echo "DAL node dependency test passed"
