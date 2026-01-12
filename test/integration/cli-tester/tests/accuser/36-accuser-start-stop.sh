#!/bin/bash
# Test: Accuser start/stop lifecycle with running node
# Also discovers where accuser writes logs
set -euo pipefail
source /tests/lib.sh

NODE_INSTANCE="test-accuser-lifecycle-node"
ACCUSER_INSTANCE="test-accuser-lifecycle"
NODE_RPC="127.0.0.1:18771"
NODE_NET="0.0.0.0:19791"

echo "Test: Accuser start/stop lifecycle"

cleanup_instance "$ACCUSER_INSTANCE" || true
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
if ! wait_for_node_ready "$NODE_RPC" 180; then
    echo "ERROR: Node RPC not ready"
    show_service_logs "node" "$NODE_INSTANCE" 50
    exit 1
fi
echo "Node is ready"

# Install accuser
echo "Installing accuser..."
om install-accuser \
    --instance "$ACCUSER_INSTANCE" \
    --node-instance "$NODE_INSTANCE" \
    --service-user tezos \
    --no-enable 2>&1

# Get the base dir from env file
ENV_FILE="/etc/octez/instances/$ACCUSER_INSTANCE/node.env"
BASE_DIR=$(grep "OCTEZ_CLIENT_BASE_DIR=" "$ENV_FILE" | cut -d'=' -f2)
echo "Accuser base dir: $BASE_DIR"

# Start accuser
echo "Starting accuser..."
om instance "$ACCUSER_INSTANCE" start

# Wait for accuser service to be active
if ! wait_for_service_active "accuser" "$ACCUSER_INSTANCE" 30; then
    echo "ERROR: Accuser service did not start"
    show_service_logs "accuser" "$ACCUSER_INSTANCE" 50
    exit 1
fi
echo "Accuser service is active"

# Give it a moment to initialize and create logs
sleep 10

# Check accuser is still running (didn't crash immediately)
if ! service_is_active "accuser" "$ACCUSER_INSTANCE"; then
    echo "ERROR: Accuser service crashed after startup"
    show_service_logs "accuser" "$ACCUSER_INSTANCE" 50
    exit 1
fi
echo "Accuser service stable"

# Discover log directories - print what exists
echo "=== Log directory discovery ==="
echo "Checking $BASE_DIR/logs/ ..."
if [ -d "$BASE_DIR/logs" ]; then
    echo "Contents of $BASE_DIR/logs/:"
    ls -la "$BASE_DIR/logs/" || true

    # Check for octez-accuser
    if [ -d "$BASE_DIR/logs/octez-accuser" ]; then
        echo "FOUND: $BASE_DIR/logs/octez-accuser/"
        ls -la "$BASE_DIR/logs/octez-accuser/" || true
    fi

    # Check for octez-baker
    if [ -d "$BASE_DIR/logs/octez-baker" ]; then
        echo "FOUND: $BASE_DIR/logs/octez-baker/"
        ls -la "$BASE_DIR/logs/octez-baker/" || true
    fi
else
    echo "No logs directory at $BASE_DIR/logs/"
fi

# Also check base_dir directly
echo "Contents of $BASE_DIR:"
ls -la "$BASE_DIR/" || true

echo "=== End log discovery ==="

# Stop accuser
echo "Stopping accuser..."
om instance "$ACCUSER_INSTANCE" stop

if ! wait_for_service_stopped "accuser" "$ACCUSER_INSTANCE" 30; then
    echo "ERROR: Accuser service did not stop"
    exit 1
fi
echo "Accuser service stopped"

# Stop node
echo "Stopping node..."
om instance "$NODE_INSTANCE" stop
wait_for_service_stopped "node" "$NODE_INSTANCE" 30

# Cleanup
cleanup_instance "$ACCUSER_INSTANCE"
cleanup_instance "$NODE_INSTANCE"

echo "Accuser start/stop lifecycle test passed"
