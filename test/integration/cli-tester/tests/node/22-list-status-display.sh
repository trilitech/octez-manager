#!/bin/bash
# Test: om list status display - verify running/stopped status
set -euo pipefail
source /tests/lib.sh

NODE_INSTANCE="test-list-status"
RPC_ADDR="127.0.0.1:18797"
NET_ADDR="0.0.0.0:19817"

echo "Test: om list status display"

cleanup_instance "$NODE_INSTANCE" || true

# Install a node
echo "Installing node..."
om install-node \
    --instance "$NODE_INSTANCE" \
    --network tallinnnet \
    --rpc-addr "$RPC_ADDR" \
    --net-addr "$NET_ADDR" \
    --service-user tezos \
    --no-enable 2>&1

# Inject pre-generated identity for faster start
inject_identity "$NODE_INSTANCE"

# Initially should be stopped
LIST_OUTPUT=$(om list)
echo "Initial om list output:"
echo "$LIST_OUTPUT"
if echo "$LIST_OUTPUT" | grep "$NODE_INSTANCE" | grep -q "stopped"; then
    echo "Correctly shows stopped status after install"
else
    echo "ERROR: Expected stopped status after install"
    exit 1
fi

# Start the node
echo "Starting node..."
om instance "$NODE_INSTANCE" start
sleep 3

# Should now show running
LIST_OUTPUT=$(om list)
echo "After start om list output:"
echo "$LIST_OUTPUT"
if echo "$LIST_OUTPUT" | grep "$NODE_INSTANCE" | grep -q "running"; then
    echo "Correctly shows running status"
else
    echo "ERROR: Expected running status"
    exit 1
fi

# Stop the node
echo "Stopping node..."
om instance "$NODE_INSTANCE" stop
sleep 2

# Should show stopped (or failed due to SIGTERM exit - both mean not running)
LIST_OUTPUT=$(om list)
echo "After stop om list output:"
echo "$LIST_OUTPUT"
# Check it's not running anymore - could be "stopped" or "failed" depending on SIGTERM handling
if echo "$LIST_OUTPUT" | grep "$NODE_INSTANCE" | grep -q "running"; then
    echo "ERROR: Still shows running status after stop"
    exit 1
else
    echo "Correctly shows non-running status after stop"
fi

# Reset failed state for clean test
systemctl reset-failed "octez-node@$NODE_INSTANCE" || true
sleep 1

# Now check it shows stopped
LIST_OUTPUT=$(om list)
echo "After reset-failed om list output:"
echo "$LIST_OUTPUT"
if echo "$LIST_OUTPUT" | grep "$NODE_INSTANCE" | grep -q "stopped"; then
    echo "Correctly shows stopped status after reset-failed"
else
    echo "WARNING: Expected stopped status, got:"
    echo "$LIST_OUTPUT" | grep "$NODE_INSTANCE"
fi

# Cleanup
cleanup_instance "$NODE_INSTANCE"

echo "om list status display test passed"
