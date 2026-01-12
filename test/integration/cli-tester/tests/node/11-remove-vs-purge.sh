#!/bin/bash
# Test: Remove keeps data, purge deletes it
set -euo pipefail
source /tests/lib.sh

INSTANCE="test-remove-purge"
DATA_DIR="/var/lib/octez/$INSTANCE"

echo "Test: Remove keeps data, purge deletes it"

cleanup_instance "$INSTANCE" || true
rm -rf "$DATA_DIR"

# Install node
om install-node \
    --instance "$INSTANCE" \
    --network shadownet \
    --rpc-addr "127.0.0.1:8740" --net-addr "0.0.0.0:9740" \
    --service-user tezos \
    --no-enable 2>&1

# Create marker in data dir
echo "marker" > "$DATA_DIR/marker.txt"

# Remove instance (should keep data)
echo "Removing instance..."
om instance "$INSTANCE" remove

# Verify removed from registry
if instance_exists "$INSTANCE"; then
    echo "ERROR: Instance still in registry after remove"
    exit 1
fi
echo "Instance removed from registry"

# Verify data still exists
if [ ! -f "$DATA_DIR/marker.txt" ]; then
    echo "ERROR: Data was deleted by remove (should be kept)"
    exit 1
fi
echo "Data preserved after remove"

# Reinstall to test purge (don't specify --network, it's in existing config)
om install-node \
    --instance "$INSTANCE" \
    --data-dir "$DATA_DIR" \
    --rpc-addr "127.0.0.1:8740" --net-addr "0.0.0.0:9740" \
    --service-user tezos \
    --preserve-data \
    --no-enable 2>&1

# Purge instance (should delete data)
echo "Purging instance..."
om instance "$INSTANCE" purge

# Verify data deleted
if [ -d "$DATA_DIR" ]; then
    echo "ERROR: Data still exists after purge"
    exit 1
fi
echo "Data deleted after purge"

echo "Remove vs purge test passed"
