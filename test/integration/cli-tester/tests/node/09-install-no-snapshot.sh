#!/bin/bash
# Test: Install node without snapshot import
set -euo pipefail
source /tests/lib.sh

INSTANCE="test-no-snapshot"
DATA_DIR="/var/lib/octez/$INSTANCE"

echo "Test: Install node without snapshot import"

cleanup_instance "$INSTANCE" || true

# Install without --snapshot flag
om install-node \
    --instance "$INSTANCE" \
    --network tallinnnet \
    --rpc-addr "127.0.0.1:8738" --net-addr "0.0.0.0:9738" \
    --service-user tezos \
    --no-enable 2>&1

# Verify instance created
if ! instance_exists "$INSTANCE"; then
    echo "ERROR: Instance not created"
    exit 1
fi
echo "Instance created without snapshot"

# Verify data dir exists but has no context (no snapshot imported)
if [ ! -d "$DATA_DIR" ]; then
    echo "ERROR: Data directory not created"
    exit 1
fi

# Context directory should not exist or be empty (no snapshot)
if [ -d "$DATA_DIR/context" ] && [ "$(ls -A "$DATA_DIR/context" 2>/dev/null)" ]; then
    echo "ERROR: Context directory has content - snapshot may have been imported"
    exit 1
fi
echo "No snapshot imported (context dir empty/missing as expected)"

# Cleanup
cleanup_instance "$INSTANCE"

echo "No snapshot test passed"
