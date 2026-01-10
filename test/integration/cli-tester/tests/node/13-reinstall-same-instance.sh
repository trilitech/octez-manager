#!/bin/bash
# Test: Reinstall existing instance updates configuration
set -euo pipefail
source /tests/lib.sh

INSTANCE="test-reinstall"
DATA_DIR="/var/lib/octez/$INSTANCE"

echo "Test: Reinstall existing instance updates configuration"

cleanup_instance "$INSTANCE" || true

# First install with one RPC address
echo "First install..."
om install-node \
    --instance "$INSTANCE" \
    --network tallinnnet \
    --rpc-addr "127.0.0.1:8742" --net-addr "0.0.0.0:9742" \
    --service-user tezos \
    --no-enable 2>&1

# Verify first config
ENV_FILE="/etc/octez/instances/$INSTANCE/node.env"
if ! grep -q "8742" "$ENV_FILE"; then
    echo "ERROR: First install RPC not configured"
    exit 1
fi
echo "First install: RPC 8742"

# Reinstall with different RPC address (don't specify --network, it's in existing config)
echo "Reinstall with new RPC..."
om install-node \
    --instance "$INSTANCE" \
    --data-dir "$DATA_DIR" \
    --rpc-addr "127.0.0.1:8743" --net-addr "0.0.0.0:9743" \
    --service-user tezos \
    --preserve-data \
    --no-enable 2>&1

# Verify updated config
if ! grep -q "8743" "$ENV_FILE"; then
    echo "ERROR: Reinstall didn't update RPC"
    cat "$ENV_FILE"
    exit 1
fi
echo "Reinstall: RPC updated to 8743"

# Verify old RPC not present
if grep -q "8742" "$ENV_FILE"; then
    echo "ERROR: Old RPC still in config"
    exit 1
fi
echo "Old RPC removed"

# Cleanup
cleanup_instance "$INSTANCE"

echo "Reinstall test passed"
