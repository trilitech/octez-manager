#!/bin/bash
# Test: DAL node installation verifies systemd dependency is created
# When using a local node instance, DAL should have BindsTo dependency
set -euo pipefail
source /tests/lib.sh

NODE_INSTANCE="test-dal-dep-check-node"
DAL_INSTANCE="test-dal-dep-check"
NODE_RPC="127.0.0.1:18741"
NODE_NET="0.0.0.0:19761"
DAL_RPC="127.0.0.1:10741"
DAL_NET="0.0.0.0:11741"

echo "Test: DAL node systemd dependency verification"

cleanup_instance "$DAL_INSTANCE" || true
cleanup_instance "$NODE_INSTANCE" || true

# Install a node first
echo "Installing node..."
om install-node \
    --instance "$NODE_INSTANCE" \
    --network shadownet \
    --rpc-addr "$NODE_RPC" \
    --net-addr "$NODE_NET" \
    --service-user tezos \
    --no-enable 2>&1

# Install DAL node using local instance reference
echo "Installing DAL node with local instance reference..."
om install-dal-node \
    --instance "$DAL_INSTANCE" \
    --node-instance "$NODE_INSTANCE" \
    --rpc-addr "$DAL_RPC" \
    --net-addr "$DAL_NET" \
    --service-user tezos \
    --no-enable 2>&1

# Verify DAL instance exists
if ! instance_exists "$DAL_INSTANCE"; then
    echo "ERROR: DAL instance not in registry"
    exit 1
fi
echo "DAL instance registered"

# Verify env file contains the node endpoint
ENV_FILE="/etc/octez/instances/$DAL_INSTANCE/node.env"
if ! grep -q "OCTEZ_NODE_ENDPOINT=http://$NODE_RPC" "$ENV_FILE"; then
    echo "ERROR: Node endpoint not in env file"
    cat "$ENV_FILE"
    exit 1
fi
echo "Node endpoint configured correctly"

# Verify systemd dependency IS configured (local instance should have BindsTo)
DROPIN_DIR="/etc/systemd/system/octez-dal-node@${DAL_INSTANCE}.service.d"
if [ ! -f "$DROPIN_DIR/override.conf" ]; then
    echo "ERROR: Drop-in override not found"
    exit 1
fi

if ! grep -q "BindsTo=octez-node@${NODE_INSTANCE}.service" "$DROPIN_DIR/override.conf"; then
    echo "ERROR: BindsTo dependency not configured for local instance"
    cat "$DROPIN_DIR/override.conf"
    exit 1
fi
echo "BindsTo dependency configured correctly"

if ! grep -q "After=octez-node@${NODE_INSTANCE}.service" "$DROPIN_DIR/override.conf"; then
    echo "ERROR: After dependency not configured"
    cat "$DROPIN_DIR/override.conf"
    exit 1
fi
echo "After dependency configured correctly"

# Verify systemd service exists
if ! service_exists "dal-node" "$DAL_INSTANCE"; then
    echo "ERROR: DAL systemd service not found"
    exit 1
fi
echo "Systemd service exists"

# Cleanup
cleanup_instance "$DAL_INSTANCE"
cleanup_instance "$NODE_INSTANCE"

echo "DAL systemd dependency test passed"
