#!/bin/bash
# Test: Remove node (keep data), reinstall with same data-dir, verify resumes
set -euo pipefail
source /tests/lib.sh

INSTANCE="test-preserve-resume"
RPC_ADDR="127.0.0.1:18733"
NET_ADDR="0.0.0.0:19751"
DATA_DIR="/var/lib/octez/$INSTANCE"

echo "Test: Remove node, reinstall with preserved data, verify resume"

cleanup_instance "$INSTANCE" || true
rm -rf "$DATA_DIR"

# Install node with snapshot
echo "Installing node with snapshot..."
om install-node \
    --instance "$INSTANCE" \
    --network shadownet \
    --snapshot \
    --snapshot-uri "$SANDBOX_URL/snapshot.rolling" \
    --rpc-addr "$RPC_ADDR" \
    --net-addr "$NET_ADDR" \
    --service-user tezos \
    --no-enable 2>&1

# Inject pre-generated identity to skip PoW
inject_identity "$INSTANCE"

# Start the node
echo "Starting node (first time)..."
om instance "$INSTANCE" start

# Wait for service and RPC
if ! wait_for_service_active "node" "$INSTANCE" 30; then
    echo "ERROR: Service did not start"
    show_service_logs "node" "$INSTANCE" 50
    exit 1
fi

if ! wait_for_node_ready "$RPC_ADDR" 60; then
    echo "ERROR: Node RPC not ready"
    show_service_logs "node" "$INSTANCE" 50
    exit 1
fi

# Get the level
LEVEL_BEFORE=$(get_node_level "$RPC_ADDR")
if [ -z "$LEVEL_BEFORE" ]; then
    echo "ERROR: Could not get level"
    exit 1
fi
echo "Level before remove: $LEVEL_BEFORE"

# Stop the node
echo "Stopping node..."
om instance "$INSTANCE" stop
wait_for_service_stopped "node" "$INSTANCE" 30

# Remove instance (WITHOUT purge - keep data)
echo "Removing instance (keeping data)..."
om instance "$INSTANCE" remove

# Verify instance is gone from registry
if instance_exists "$INSTANCE"; then
    echo "ERROR: Instance still in registry after remove"
    exit 1
fi
echo "Instance removed from registry"

# Verify data directory still exists
if [ ! -d "$DATA_DIR" ]; then
    echo "ERROR: Data directory was deleted"
    exit 1
fi
echo "Data directory preserved: $DATA_DIR"

# Reinstall with same data directory and --preserve-data
echo "Reinstalling with preserved data..."
om install-node \
    --instance "$INSTANCE" \
    --data-dir "$DATA_DIR" \
    --rpc-addr "$RPC_ADDR" \
    --net-addr "$NET_ADDR" \
    --service-user tezos \
    --preserve-data \
    --no-enable 2>&1

# Start the node again
echo "Starting node (second time)..."
om instance "$INSTANCE" start

# Wait for service and RPC
if ! wait_for_service_active "node" "$INSTANCE" 30; then
    echo "ERROR: Service did not start on second run"
    show_service_logs "node" "$INSTANCE" 50
    exit 1
fi

# Second run should be fast since identity was preserved
if ! wait_for_node_ready "$RPC_ADDR" 60; then
    echo "ERROR: Node RPC not ready on second run"
    show_service_logs "node" "$INSTANCE" 50
    exit 1
fi

# Get the level after restart
LEVEL_AFTER=$(get_node_level "$RPC_ADDR")
if [ -z "$LEVEL_AFTER" ]; then
    echo "ERROR: Could not get level after restart"
    exit 1
fi
echo "Level after restart: $LEVEL_AFTER"

# Verify level is at least as high as before (data was preserved)
if [ "$LEVEL_AFTER" -lt "$LEVEL_BEFORE" ]; then
    echo "ERROR: Level decreased after restart ($LEVEL_BEFORE -> $LEVEL_AFTER)"
    echo "Data may not have been preserved correctly"
    exit 1
fi
echo "Level preserved: $LEVEL_BEFORE -> $LEVEL_AFTER"

# Stop and cleanup
echo "Stopping node..."
om instance "$INSTANCE" stop
wait_for_service_stopped "node" "$INSTANCE" 30

cleanup_instance "$INSTANCE"

echo "Preserve data resume test passed"
