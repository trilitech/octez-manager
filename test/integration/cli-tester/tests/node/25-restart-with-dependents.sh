#!/bin/bash
# Test: Restart node with health checks for dependents
# Verifies that restarting a node properly stops dependents,
# waits for RPC, and restarts dependents in order.
set -euo pipefail
source /tests/lib.sh

NODE_INSTANCE="test-restart-node"
BAKER_INSTANCE="test-restart-baker"
NODE_RPC="127.0.0.1:18798"
NODE_NET="0.0.0.0:19798"

echo "Test: Restart node with health checks"

# Cleanup any previous test instances
cleanup_instance "$BAKER_INSTANCE" || true
cleanup_instance "$NODE_INSTANCE" || true

# Install a node
echo "Installing node..."
om install-node \
    --instance "$NODE_INSTANCE" \
    --network shadownet \
    --history-mode rolling \
    --rpc-addr "$NODE_RPC" \
    --net-addr "$NODE_NET" \
    --service-user tezos \
    --no-enable 2>&1

# Start the node
echo "Starting node..."
om instance "$NODE_INSTANCE" start 2>&1
wait_for_service_active "node" "$NODE_INSTANCE"
echo "Node started"

# Wait for node RPC to be ready
echo "Waiting for node RPC..."
wait_for_rpc "$NODE_RPC"
echo "Node RPC ready"

# Install a baker that depends on the node
echo "Installing baker dependent on node..."
om install-baker \
    --instance "$BAKER_INSTANCE" \
    --node-instance "$NODE_INSTANCE" \
    --liquidity-baking-vote pass \
    --service-user tezos \
    --no-enable 2>&1

# Start the baker
echo "Starting baker..."
om instance "$BAKER_INSTANCE" start 2>&1
wait_for_service_active "baker" "$BAKER_INSTANCE"
echo "Baker started"

# Verify both services are running
echo "Verifying services are running..."
if ! systemctl is-active --quiet "octez-node@$NODE_INSTANCE"; then
    echo "ERROR: Node should be running"
    exit 1
fi
if ! systemctl is-active --quiet "octez-baker@$BAKER_INSTANCE"; then
    echo "ERROR: Baker should be running"
    exit 1
fi
echo "Both services running"

# Restart the node with health checks
echo "Restarting node (with health checks)..."
RESTART_OUTPUT=$(om instance "$NODE_INSTANCE" restart 2>&1)
echo "$RESTART_OUTPUT"

# Verify restart output mentions stopping and starting dependents
if ! echo "$RESTART_OUTPUT" | grep -q "Stopping dependent"; then
    echo "ERROR: Restart should mention stopping dependents"
    exit 1
fi
echo "Restart output mentions stopping dependents"

if ! echo "$RESTART_OUTPUT" | grep -q "Waiting for RPC"; then
    echo "ERROR: Restart should wait for RPC"
    exit 1
fi
echo "Restart output mentions waiting for RPC"

if ! echo "$RESTART_OUTPUT" | grep -q "Starting dependent"; then
    echo "ERROR: Restart should mention starting dependents"
    exit 1
fi
echo "Restart output mentions starting dependents"

# Verify both services are still running after restart
sleep 3
echo "Verifying services after restart..."
if ! systemctl is-active --quiet "octez-node@$NODE_INSTANCE"; then
    echo "ERROR: Node should be running after restart"
    systemctl status "octez-node@$NODE_INSTANCE" || true
    exit 1
fi
echo "Node running after restart"

if ! systemctl is-active --quiet "octez-baker@$BAKER_INSTANCE"; then
    echo "ERROR: Baker should be running after restart"
    systemctl status "octez-baker@$BAKER_INSTANCE" || true
    exit 1
fi
echo "Baker running after restart"

# Test --no-wait flag
echo "Testing --no-wait flag..."
NO_WAIT_OUTPUT=$(om instance "$NODE_INSTANCE" restart --no-wait 2>&1)
echo "$NO_WAIT_OUTPUT"

if echo "$NO_WAIT_OUTPUT" | grep -q "Waiting for RPC"; then
    echo "ERROR: --no-wait should skip RPC wait"
    exit 1
fi
echo "--no-wait correctly skips RPC wait"

# Cleanup
echo "Cleaning up..."
om instance "$BAKER_INSTANCE" stop 2>&1 || true
om instance "$NODE_INSTANCE" stop 2>&1 || true
cleanup_instance "$BAKER_INSTANCE"
cleanup_instance "$NODE_INSTANCE"

echo "Restart with dependents test passed"
