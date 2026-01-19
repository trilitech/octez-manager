#!/bin/bash
# Test: Baker installation with remote node endpoint (not --node-instance)
set -euo pipefail
source /tests/lib.sh

NODE_INSTANCE="test-baker-remote-node"
BAKER_INSTANCE="test-baker-remote"
NODE_RPC="127.0.0.1:18780"
NODE_NET="0.0.0.0:19800"

echo "Test: Baker installation with remote node endpoint"

cleanup_instance "$BAKER_INSTANCE" || true
cleanup_instance "$NODE_INSTANCE" || true

# Install and start a node (we'll reference it by endpoint, not instance)
echo "Installing node..."
om install-node \
    --instance "$NODE_INSTANCE" \
    --network shadownet \
    --snapshot \
    --snapshot-no-check \
    --snapshot-uri "$SANDBOX_URL/snapshot.rolling" \
    --rpc-addr "$NODE_RPC" \
    --net-addr "$NODE_NET" \
    --service-user tezos \
    --no-enable 2>&1

# Inject pre-generated identity to skip PoW
inject_identity "$NODE_INSTANCE"

# Start the node
echo "Starting node..."
om instance "$NODE_INSTANCE" start

# Wait for node to be ready
if ! wait_for_service_active "node" "$NODE_INSTANCE" 30; then
    echo "ERROR: Node service did not start"
    show_service_logs "node" "$NODE_INSTANCE" 50
    exit 1
fi

if ! wait_for_node_ready "$NODE_RPC" 60; then
    echo "ERROR: Node RPC not ready"
    show_service_logs "node" "$NODE_INSTANCE" 50
    exit 1
fi
echo "Node is ready"

# Install baker with endpoint URL via --node-instance (not an instance name)
echo "Installing baker with remote endpoint..."
om install-baker \
    --instance "$BAKER_INSTANCE" \
    --node-instance "http://$NODE_RPC" \
    --liquidity-baking-vote pass \
    --dal-endpoint none \
    --service-user tezos \
    --no-enable 2>&1

# Verify env file has remote mode
ENV_FILE="/etc/octez/instances/$BAKER_INSTANCE/node.env"
if ! grep -q "OCTEZ_BAKER_NODE_MODE=remote" "$ENV_FILE"; then
    echo "ERROR: Node mode not set to remote"
    cat "$ENV_FILE"
    exit 1
fi
echo "Node mode is remote"

if ! grep -q "OCTEZ_NODE_ENDPOINT=http://$NODE_RPC" "$ENV_FILE"; then
    echo "ERROR: Node endpoint not configured"
    cat "$ENV_FILE"
    exit 1
fi
echo "Node endpoint configured correctly"

# Verify no BindsTo dependency (remote nodes shouldn't have it)
DROPIN_DIR="/etc/systemd/system/octez-baker@$BAKER_INSTANCE.service.d"
if [ -f "$DROPIN_DIR/override.conf" ] && grep -q "BindsTo=octez-node@" "$DROPIN_DIR/override.conf"; then
    echo "ERROR: Remote endpoint baker should not have BindsTo dependency"
    cat "$DROPIN_DIR/override.conf"
    exit 1
fi
echo "No BindsTo dependency (correct for remote endpoint)"

# Start baker
echo "Starting baker..."
om instance "$BAKER_INSTANCE" start

if ! wait_for_service_active "baker" "$BAKER_INSTANCE" 30; then
    echo "ERROR: Baker service did not start"
    show_service_logs "baker" "$BAKER_INSTANCE" 50
    exit 1
fi
echo "Baker service is active"

# Check baker is stable
sleep 5
if ! service_is_active "baker" "$BAKER_INSTANCE"; then
    echo "ERROR: Baker service crashed"
    show_service_logs "baker" "$BAKER_INSTANCE" 50
    exit 1
fi
echo "Baker service stable with remote endpoint"

# Stop baker
echo "Stopping baker..."
om instance "$BAKER_INSTANCE" stop
wait_for_service_stopped "baker" "$BAKER_INSTANCE" 30

# Stop node
echo "Stopping node..."
om instance "$NODE_INSTANCE" stop
wait_for_service_stopped "node" "$NODE_INSTANCE" 30

# Cleanup
cleanup_instance "$BAKER_INSTANCE"
cleanup_instance "$NODE_INSTANCE"

echo "Baker remote endpoint test passed"
