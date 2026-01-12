#!/bin/bash
# Test: Multiple instances can coexist independently
set -euo pipefail
source /tests/lib.sh

INSTANCE1="test-multi-1"
INSTANCE2="test-multi-2"

echo "Test: Multiple instances can coexist"

cleanup_instance "$INSTANCE1" || true
cleanup_instance "$INSTANCE2" || true

# Install first instance
echo "Installing first instance..."
om install-node \
    --instance "$INSTANCE1" \
    --network shadownet \
    --rpc-addr "127.0.0.1:8744" --net-addr "0.0.0.0:9744" \
    --service-user tezos \
    --no-enable 2>&1

# Install second instance
echo "Installing second instance..."
om install-node \
    --instance "$INSTANCE2" \
    --network shadownet \
    --rpc-addr "127.0.0.1:8745" --net-addr "0.0.0.0:9745" \
    --service-user tezos \
    --no-enable 2>&1

# Verify both in registry
if ! instance_exists "$INSTANCE1"; then
    echo "ERROR: First instance not in registry"
    exit 1
fi
if ! instance_exists "$INSTANCE2"; then
    echo "ERROR: Second instance not in registry"
    exit 1
fi
echo "Both instances registered"

# Verify separate data directories
if [ ! -d "/var/lib/octez/$INSTANCE1" ]; then
    echo "ERROR: First instance data dir missing"
    exit 1
fi
if [ ! -d "/var/lib/octez/$INSTANCE2" ]; then
    echo "ERROR: Second instance data dir missing"
    exit 1
fi
echo "Separate data directories exist"

# Verify separate env files with different ports
if ! grep -q "8744" "/etc/octez/instances/$INSTANCE1/node.env"; then
    echo "ERROR: First instance wrong port"
    exit 1
fi
if ! grep -q "8745" "/etc/octez/instances/$INSTANCE2/node.env"; then
    echo "ERROR: Second instance wrong port"
    exit 1
fi
echo "Different RPC ports configured"

# Remove first, verify second still exists
echo "Removing first instance..."
om instance "$INSTANCE1" purge

if instance_exists "$INSTANCE1"; then
    echo "ERROR: First instance not removed"
    exit 1
fi
if ! instance_exists "$INSTANCE2"; then
    echo "ERROR: Second instance affected by first removal"
    exit 1
fi
echo "Instances are independent"

# Cleanup
cleanup_instance "$INSTANCE2"

echo "Multiple instances test passed"
