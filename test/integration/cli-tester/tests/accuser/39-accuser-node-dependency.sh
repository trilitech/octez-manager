#!/bin/bash
# Test: Accuser stops when parent node stops (BindsTo dependency)
set -euo pipefail
source /tests/lib.sh

NODE_INSTANCE="test-accuser-dep-node"
ACCUSER_INSTANCE="test-accuser-dep"
NODE_RPC="127.0.0.1:18774"
NODE_NET="0.0.0.0:19794"

echo "Test: Accuser node dependency (BindsTo)"

cleanup_instance "$ACCUSER_INSTANCE" || true
cleanup_instance "$NODE_INSTANCE" || true

# Install node with snapshot
echo "Installing node..."
om install-node \
    --instance "$NODE_INSTANCE" \
    --network tallinnnet \
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

# Install accuser with local node reference
echo "Installing accuser..."
om install-accuser \
    --instance "$ACCUSER_INSTANCE" \
    --node-instance "$NODE_INSTANCE" \
    --service-user tezos \
    --no-enable 2>&1

# Verify systemd dependency is configured
DROPIN="/etc/systemd/system/octez-accuser@${ACCUSER_INSTANCE}.service.d/override.conf"
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

# Start accuser
echo "Starting accuser..."
om instance "$ACCUSER_INSTANCE" start

if ! wait_for_service_active "accuser" "$ACCUSER_INSTANCE" 30; then
    echo "ERROR: Accuser service did not start"
    show_service_logs "accuser" "$ACCUSER_INSTANCE" 50
    exit 1
fi
echo "Accuser service is active"

# Now stop the node - accuser should stop automatically due to BindsTo
echo "Stopping node (accuser should stop automatically)..."
om instance "$NODE_INSTANCE" stop

if ! wait_for_service_stopped "node" "$NODE_INSTANCE" 30; then
    echo "ERROR: Node did not stop"
    exit 1
fi
echo "Node stopped"

# Accuser should have stopped automatically
sleep 2
if service_is_active "accuser" "$ACCUSER_INSTANCE"; then
    echo "ERROR: Accuser is still running after node stopped"
    echo "BindsTo dependency not working correctly"
    exit 1
fi
echo "Accuser stopped automatically (BindsTo working)"

# Cleanup
cleanup_instance "$ACCUSER_INSTANCE"
cleanup_instance "$NODE_INSTANCE"

echo "Accuser node dependency test passed"
