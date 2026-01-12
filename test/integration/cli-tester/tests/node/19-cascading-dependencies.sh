#!/bin/bash
# Test: Cascading dependencies - all dependents stop when node stops
# Tests that baker, accuser, and DAL all stop when the parent node stops
set -euo pipefail
source /tests/lib.sh

NODE_INSTANCE="test-cascade-node"
DAL_INSTANCE="test-cascade-dal"
BAKER_INSTANCE="test-cascade-baker"
ACCUSER_INSTANCE="test-cascade-accuser"
NODE_RPC="127.0.0.1:18790"
NODE_NET="0.0.0.0:19810"
DAL_RPC="127.0.0.1:10790"
DAL_NET="0.0.0.0:11790"

echo "Test: Cascading dependencies (node → baker, accuser, DAL)"

# Cleanup any existing instances
cleanup_instance "$ACCUSER_INSTANCE" || true
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

# Install DAL node
echo "Installing DAL node..."
om install-dal-node \
    --instance "$DAL_INSTANCE" \
    --node-instance "$NODE_INSTANCE" \
    --rpc-addr "$DAL_RPC" \
    --net-addr "$DAL_NET" \
    --service-user tezos \
    --no-enable 2>&1

# Install baker
echo "Installing baker..."
om install-baker \
    --instance "$BAKER_INSTANCE" \
    --node-instance "$NODE_INSTANCE" \
    --liquidity-baking-vote pass \
    --dal-endpoint none \
    --service-user tezos \
    --no-enable 2>&1

# Install accuser
echo "Installing accuser..."
om install-accuser \
    --instance "$ACCUSER_INSTANCE" \
    --node-instance "$NODE_INSTANCE" \
    --service-user tezos \
    --no-enable 2>&1

# Start all dependent services
echo "Starting DAL node..."
om instance "$DAL_INSTANCE" start
if ! wait_for_service_active "dal-node" "$DAL_INSTANCE" 30; then
    echo "ERROR: DAL service did not start"
    show_service_logs "dal-node" "$DAL_INSTANCE" 50
    exit 1
fi

echo "Starting baker..."
om instance "$BAKER_INSTANCE" start
if ! wait_for_service_active "baker" "$BAKER_INSTANCE" 30; then
    echo "ERROR: Baker service did not start"
    show_service_logs "baker" "$BAKER_INSTANCE" 50
    exit 1
fi

echo "Starting accuser..."
om instance "$ACCUSER_INSTANCE" start
if ! wait_for_service_active "accuser" "$ACCUSER_INSTANCE" 30; then
    echo "ERROR: Accuser service did not start"
    show_service_logs "accuser" "$ACCUSER_INSTANCE" 50
    exit 1
fi

# Verify all services are running
echo "All services started:"
echo "  - Node: $(service_is_active node $NODE_INSTANCE && echo 'active' || echo 'inactive')"
echo "  - DAL: $(service_is_active dal-node $DAL_INSTANCE && echo 'active' || echo 'inactive')"
echo "  - Baker: $(service_is_active baker $BAKER_INSTANCE && echo 'active' || echo 'inactive')"
echo "  - Accuser: $(service_is_active accuser $ACCUSER_INSTANCE && echo 'active' || echo 'inactive')"

# Give services time to stabilize
sleep 5

# Now stop the node - all dependents should stop automatically
echo ""
echo "Stopping node (all dependents should cascade stop)..."
om instance "$NODE_INSTANCE" stop

if ! wait_for_service_stopped "node" "$NODE_INSTANCE" 30; then
    echo "ERROR: Node did not stop"
    exit 1
fi
echo "Node stopped"

# Wait a moment for cascading stops
sleep 3

# Check all dependents stopped
FAILED=0

if service_is_active "dal-node" "$DAL_INSTANCE"; then
    echo "ERROR: DAL node still running after node stopped"
    FAILED=1
else
    echo "DAL node stopped (cascade working)"
fi

if service_is_active "baker" "$BAKER_INSTANCE"; then
    echo "ERROR: Baker still running after node stopped"
    FAILED=1
else
    echo "Baker stopped (cascade working)"
fi

if service_is_active "accuser" "$ACCUSER_INSTANCE"; then
    echo "ERROR: Accuser still running after node stopped"
    FAILED=1
else
    echo "Accuser stopped (cascade working)"
fi

if [ "$FAILED" -eq 1 ]; then
    echo ""
    echo "Cascade dependency test FAILED"
    exit 1
fi

# Cleanup
cleanup_instance "$ACCUSER_INSTANCE"
cleanup_instance "$BAKER_INSTANCE"
cleanup_instance "$DAL_INSTANCE"
cleanup_instance "$NODE_INSTANCE"

echo ""
echo "Cascading dependencies test passed"
