#!/bin/bash
# Test: --tmp-dir option for snapshot download
set -euo pipefail
source /tests/lib.sh

INSTANCE="test-tmp-dir"
RPC_ADDR="127.0.0.1:18795"
NET_ADDR="0.0.0.0:19815"
TMP_DIR="/var/tmp/octez-test-tmpdir"

echo "Test: --tmp-dir option for snapshot download"

cleanup_instance "$INSTANCE" || true
rm -rf "$TMP_DIR"

# Create custom tmp directory
mkdir -p "$TMP_DIR"
chown tezos:tezos "$TMP_DIR"

# Install node with --tmp-dir option
echo "Installing node with --tmp-dir..."
om install-node \
    --instance "$INSTANCE" \
    --network tallinnnet \
    --snapshot \
    --snapshot-uri "$SANDBOX_URL/snapshot.rolling" \
    --tmp-dir "$TMP_DIR" \
    --rpc-addr "$RPC_ADDR" \
    --net-addr "$NET_ADDR" \
    --service-user tezos \
    --no-enable 2>&1

# Verify instance was created
if ! instance_exists "$INSTANCE"; then
    echo "ERROR: Instance not created"
    exit 1
fi
echo "Instance created successfully with --tmp-dir"

# Verify the tmp directory was used (no leftover files should remain)
# The snapshot file should have been cleaned up after import
LEFTOVER=$(ls -A "$TMP_DIR" 2>/dev/null | wc -l)
if [ "$LEFTOVER" -gt 0 ]; then
    echo "WARNING: Leftover files in tmp dir (expected cleanup):"
    ls -la "$TMP_DIR"
fi

# Start the node to verify it works
echo "Starting node..."
om instance "$INSTANCE" start

if ! wait_for_service_active "node" "$INSTANCE" 30; then
    echo "ERROR: Node service did not start"
    show_service_logs "node" "$INSTANCE" 50
    exit 1
fi

# Wait for RPC to be ready
if ! wait_for_node_ready "$RPC_ADDR" 240; then
    echo "ERROR: Node RPC not ready"
    show_service_logs "node" "$INSTANCE" 50
    exit 1
fi
echo "Node is ready"

# Verify node is working
LEVEL=$(get_node_level "$RPC_ADDR")
if [ -z "$LEVEL" ]; then
    echo "ERROR: Could not get node level"
    exit 1
fi
echo "Node is at level $LEVEL"

# Stop and cleanup
echo "Stopping node..."
om instance "$INSTANCE" stop
wait_for_service_stopped "node" "$INSTANCE" 30

cleanup_instance "$INSTANCE"
rm -rf "$TMP_DIR"

echo "tmp-dir option test passed"
