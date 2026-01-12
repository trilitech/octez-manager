#!/bin/bash
# Test: Baker running with DAL node
set -euo pipefail
source /tests/lib.sh

NODE_INSTANCE="test-baker-dal-runtime-node"
DAL_INSTANCE="test-baker-dal-runtime-dal"
BAKER_INSTANCE="test-baker-dal-runtime"
NODE_RPC="127.0.0.1:18781"
NODE_NET="0.0.0.0:19801"
DAL_RPC="127.0.0.1:10781"
DAL_NET="0.0.0.0:11781"

echo "Test: Baker running with DAL node"

cleanup_instance "$BAKER_INSTANCE" || true
cleanup_instance "$DAL_INSTANCE" || true
cleanup_instance "$NODE_INSTANCE" || true

# Install node with snapshot
echo "Installing node..."
om install-node \
    --instance "$NODE_INSTANCE" \
    --network shadownet \
    --snapshot \
    --snapshot-uri "$SANDBOX_URL/snapshot.rolling" \
    --rpc-addr "$NODE_RPC" \
    --net-addr "$NODE_NET" \
    --service-user tezos \
    --no-enable 2>&1

# Start node
echo "Starting node..."
om instance "$NODE_INSTANCE" start

if ! wait_for_service_active "node" "$NODE_INSTANCE" 30; then
    echo "ERROR: Node service did not start"
    show_service_logs "node" "$NODE_INSTANCE" 50
    exit 1
fi

if ! wait_for_node_ready "$NODE_RPC" 180; then
    echo "ERROR: Node RPC not ready"
    show_service_logs "node" "$NODE_INSTANCE" 50
    exit 1
fi
echo "Node is ready"

# Install and start DAL node
echo "Installing DAL node..."
om install-dal-node \
    --instance "$DAL_INSTANCE" \
    --node-instance "$NODE_INSTANCE" \
    --rpc-addr "$DAL_RPC" \
    --net-addr "$DAL_NET" \
    --service-user tezos \
    --no-enable 2>&1

echo "Starting DAL node..."
om instance "$DAL_INSTANCE" start

if ! wait_for_service_active "dal-node" "$DAL_INSTANCE" 30; then
    echo "ERROR: DAL service did not start"
    show_service_logs "dal-node" "$DAL_INSTANCE" 50
    exit 1
fi
echo "DAL node is active"

# Install baker pointing to DAL node
echo "Installing baker with DAL endpoint..."
om install-baker \
    --instance "$BAKER_INSTANCE" \
    --node-instance "$NODE_INSTANCE" \
    --liquidity-baking-vote pass \
    --dal-endpoint "http://$DAL_RPC" \
    --service-user tezos \
    --no-enable 2>&1

# Verify DAL endpoint configured
ENV_FILE="/etc/octez/instances/$BAKER_INSTANCE/node.env"
if ! grep -q "OCTEZ_DAL_CONFIG=http://$DAL_RPC" "$ENV_FILE"; then
    echo "ERROR: DAL endpoint not configured"
    cat "$ENV_FILE"
    exit 1
fi
echo "DAL endpoint configured correctly"

# Start baker
echo "Starting baker..."
om instance "$BAKER_INSTANCE" start

if ! wait_for_service_active "baker" "$BAKER_INSTANCE" 30; then
    echo "ERROR: Baker service did not start"
    show_service_logs "baker" "$BAKER_INSTANCE" 50
    exit 1
fi
echo "Baker service is active"

# Check all services are stable
sleep 5

if ! service_is_active "node" "$NODE_INSTANCE"; then
    echo "ERROR: Node crashed"
    exit 1
fi

if ! service_is_active "dal-node" "$DAL_INSTANCE"; then
    echo "ERROR: DAL node crashed"
    show_service_logs "dal-node" "$DAL_INSTANCE" 50
    exit 1
fi

if ! service_is_active "baker" "$BAKER_INSTANCE"; then
    echo "ERROR: Baker crashed"
    show_service_logs "baker" "$BAKER_INSTANCE" 50
    exit 1
fi
echo "All services stable (node + DAL + baker)"

# Stop services in order
echo "Stopping baker..."
om instance "$BAKER_INSTANCE" stop
wait_for_service_stopped "baker" "$BAKER_INSTANCE" 30

echo "Stopping DAL node..."
om instance "$DAL_INSTANCE" stop
wait_for_service_stopped "dal-node" "$DAL_INSTANCE" 30

echo "Stopping node..."
om instance "$NODE_INSTANCE" stop
wait_for_service_stopped "node" "$NODE_INSTANCE" 30

# Cleanup
cleanup_instance "$BAKER_INSTANCE"
cleanup_instance "$DAL_INSTANCE"
cleanup_instance "$NODE_INSTANCE"

echo "Baker with DAL node test passed"
