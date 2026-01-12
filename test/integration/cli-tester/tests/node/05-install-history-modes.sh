#!/bin/bash
# Test: Install nodes with different history modes (rolling and full)
set -euo pipefail
source /tests/lib.sh

echo "Test: Install nodes with different history modes"

# Test rolling mode
INSTANCE_ROLLING="test-rolling"
cleanup_instance "$INSTANCE_ROLLING" || true

echo "Installing node with rolling history mode..."
om install-node \
    --instance "$INSTANCE_ROLLING" \
    --network shadownet \
    --history-mode rolling \
    --rpc-addr "127.0.0.1:8734" \
    --net-addr "0.0.0.0:9734" \
    --service-user tezos \
    --no-enable 2>&1

# Verify rolling mode in env
ENV_ROLLING="/etc/octez/instances/$INSTANCE_ROLLING/node.env"
if ! grep -q "OCTEZ_HISTORY_MODE=rolling" "$ENV_ROLLING"; then
    echo "ERROR: Rolling mode not set in env file"
    cat "$ENV_ROLLING"
    exit 1
fi
echo "Rolling mode correctly configured"

# Test full mode
INSTANCE_FULL="test-full"
cleanup_instance "$INSTANCE_FULL" || true

echo "Installing node with full history mode..."
om install-node \
    --instance "$INSTANCE_FULL" \
    --network shadownet \
    --history-mode full \
    --rpc-addr "127.0.0.1:8735" \
    --net-addr "0.0.0.0:9735" \
    --service-user tezos \
    --no-enable 2>&1

# Verify full mode in env
ENV_FULL="/etc/octez/instances/$INSTANCE_FULL/node.env"
if ! grep -q "OCTEZ_HISTORY_MODE=full" "$ENV_FULL"; then
    echo "ERROR: Full mode not set in env file"
    cat "$ENV_FULL"
    exit 1
fi
echo "Full mode correctly configured"

# Cleanup
cleanup_instance "$INSTANCE_ROLLING"
cleanup_instance "$INSTANCE_FULL"

echo "History modes test passed"
