#!/bin/bash
# Test: Baker stops when parent node stops (BindsTo dependency)
set -euo pipefail
source /tests/lib.sh

NODE_INSTANCE="test-baker-dep-node"
BAKER_INSTANCE="test-baker-dep"
NODE_RPC="127.0.0.1:18766"
NODE_NET="0.0.0.0:19786"

echo "Test: Baker node dependency (BindsTo)"

cleanup_instance "$BAKER_INSTANCE" || true
cleanup_instance "$NODE_INSTANCE" || true

# Install node with snapshot
echo "Installing node..."
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

if ! wait_for_service_active "node" "$NODE_INSTANCE" 30; then
    echo "ERROR: Node service did not start"
    exit 1
fi

# Wait for node RPC
if ! wait_for_node_ready "$NODE_RPC" 60; then
    echo "ERROR: Node RPC not ready"
    show_service_logs "node" "$NODE_INSTANCE" 50
    exit 1
fi

# Install baker with local node reference
echo "Installing baker..."
om install-baker \
    --instance "$BAKER_INSTANCE" \
    --node-instance "$NODE_INSTANCE" \
    --liquidity-baking-vote pass \
    --dal-endpoint none \
    --service-user tezos \
    --no-enable 2>&1

# Verify systemd dependency is configured
DROPIN="/etc/systemd/system/octez-baker@${BAKER_INSTANCE}.service.d/override.conf"
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

# Start baker
echo "Starting baker..."
om instance "$BAKER_INSTANCE" start

if ! wait_for_service_active "baker" "$BAKER_INSTANCE" 30; then
    echo "ERROR: Baker service did not start"
    show_service_logs "baker" "$BAKER_INSTANCE" 50
    exit 1
fi
echo "Baker service is active"

# Now stop the node - baker should stop automatically due to BindsTo
echo "Stopping node (baker should stop automatically)..."
om instance "$NODE_INSTANCE" stop

if ! wait_for_service_stopped "node" "$NODE_INSTANCE" 30; then
    echo "ERROR: Node did not stop"
    exit 1
fi
echo "Node stopped"

# Baker should have stopped automatically
sleep 2
if service_is_active "baker" "$BAKER_INSTANCE"; then
    echo "ERROR: Baker is still running after node stopped"
    echo "BindsTo dependency not working correctly"
    exit 1
fi
echo "Baker stopped automatically (BindsTo working)"

# Cleanup
cleanup_instance "$BAKER_INSTANCE"
cleanup_instance "$NODE_INSTANCE"

echo "Baker node dependency test passed"
