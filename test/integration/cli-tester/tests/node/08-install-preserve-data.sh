#!/bin/bash
# Test: Install node with --preserve-data keeps existing data
set -euo pipefail
source /tests/lib.sh

INSTANCE="test-preserve"
DATA_DIR="/var/lib/octez/$INSTANCE"
MARKER_FILE="$DATA_DIR/test-marker.txt"

echo "Test: Install node with --preserve-data"

cleanup_instance "$INSTANCE" || true

# First install
echo "First install..."
om install-node \
    --instance "$INSTANCE" \
    --network tallinnnet \
    --rpc-addr "127.0.0.1:8737" --net-addr "0.0.0.0:9737" \
    --service-user tezos \
    --no-enable 2>&1

# Create a marker file to verify data preservation
echo "test-marker-content" > "$MARKER_FILE"
chown tezos:tezos "$MARKER_FILE"

# Reinstall with --preserve-data (don't specify --network, it's in existing config)
echo "Reinstall with --preserve-data..."
om install-node \
    --instance "$INSTANCE" \
    --data-dir "$DATA_DIR" \
    --rpc-addr "127.0.0.1:8737" --net-addr "0.0.0.0:9737" \
    --service-user tezos \
    --preserve-data \
    --no-enable 2>&1

# Verify marker file still exists
if [ ! -f "$MARKER_FILE" ]; then
    echo "ERROR: Marker file was deleted - data not preserved"
    exit 1
fi

if [ "$(cat "$MARKER_FILE")" != "test-marker-content" ]; then
    echo "ERROR: Marker file content changed"
    exit 1
fi
echo "Data preserved successfully"

# Cleanup
cleanup_instance "$INSTANCE"

echo "Preserve data test passed"
