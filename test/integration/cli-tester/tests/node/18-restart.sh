#!/bin/bash
# Test: Node restart command
set -euo pipefail
source /tests/lib.sh

INSTANCE="test-restart"
RPC_ADDR="127.0.0.1:18733"
NET_ADDR="0.0.0.0:19751"

echo "Test: Node restart command"

cleanup_instance "$INSTANCE" || true

# Install node with snapshot
echo "Installing node with snapshot..."
om install-node \
    --instance "$INSTANCE" \
    --network tallinnnet \
    --snapshot \
    --snapshot-uri "$SANDBOX_URL/snapshot.rolling" \
    --rpc-addr "$RPC_ADDR" \
    --net-addr "$NET_ADDR" \
    --service-user tezos \
    --no-enable 2>&1

# Start the node
echo "Starting node..."
om instance "$INSTANCE" start

# Wait for service to be active
if ! wait_for_service_active "node" "$INSTANCE" 30; then
    echo "ERROR: Service did not start"
    show_service_logs "node" "$INSTANCE" 50
    exit 1
fi

# Wait for node RPC to be ready
if ! wait_for_node_ready "$RPC_ADDR" 180; then
    echo "ERROR: Node RPC not ready"
    show_service_logs "node" "$INSTANCE" 50
    exit 1
fi
echo "Node is ready"

# Get PID before restart
PID_BEFORE=$(systemctl show "octez-node@$INSTANCE" --property=MainPID --value)
echo "PID before restart: $PID_BEFORE"

# Restart the node
echo "Restarting node..."
om instance "$INSTANCE" restart

# Wait for service to be active again
if ! wait_for_service_active "node" "$INSTANCE" 30; then
    echo "ERROR: Service did not restart"
    show_service_logs "node" "$INSTANCE" 50
    exit 1
fi

# Wait for node RPC to be ready again
if ! wait_for_node_ready "$RPC_ADDR" 180; then
    echo "ERROR: Node RPC not ready after restart"
    show_service_logs "node" "$INSTANCE" 50
    exit 1
fi
echo "Node is ready after restart"

# Get PID after restart - should be different
PID_AFTER=$(systemctl show "octez-node@$INSTANCE" --property=MainPID --value)
echo "PID after restart: $PID_AFTER"

if [ "$PID_BEFORE" = "$PID_AFTER" ]; then
    echo "ERROR: PID did not change after restart (service may not have restarted)"
    exit 1
fi
echo "PID changed, confirming restart occurred"

# Verify we can still query the node
LEVEL=$(get_node_level "$RPC_ADDR")
if [ -z "$LEVEL" ]; then
    echo "ERROR: Could not get level after restart"
    exit 1
fi
echo "Node responding at level $LEVEL"

# Stop the node
echo "Stopping node..."
om instance "$INSTANCE" stop

if ! wait_for_service_stopped "node" "$INSTANCE" 30; then
    echo "ERROR: Service did not stop"
    exit 1
fi

# Cleanup
cleanup_instance "$INSTANCE"

echo "Node restart test passed"
