#!/bin/bash
# Test: DAL node installation with RPC endpoint (not local instance reference)
set -euo pipefail
source /tests/lib.sh

NODE_INSTANCE="test-dal-remote-node"
DAL_INSTANCE="test-dal-remote"
NODE_RPC="127.0.0.1:18741"
NODE_NET="0.0.0.0:19761"
DAL_RPC="127.0.0.1:10741"
DAL_NET="0.0.0.0:11741"

echo "Test: DAL node installation with RPC endpoint"

cleanup_instance "$DAL_INSTANCE" || true
cleanup_instance "$NODE_INSTANCE" || true

# Install a node first (needed to have a valid RPC endpoint)
echo "Installing node..."
om install-node \
    --instance "$NODE_INSTANCE" \
    --network tallinnnet \
    --rpc-addr "$NODE_RPC" \
    --net-addr "$NODE_NET" \
    --service-user tezos \
    --no-enable 2>&1

# Install DAL node using RPC endpoint instead of instance reference
# This tests the "remote endpoint" code path
echo "Installing DAL node with RPC endpoint..."
om install-dal-node \
    --instance "$DAL_INSTANCE" \
    --node-instance "http://$NODE_RPC" \
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

# Verify env file contains the RPC endpoint (not instance reference)
ENV_FILE="/etc/octez/instances/$DAL_INSTANCE/node.env"
if ! grep -q "OCTEZ_NODE_ENDPOINT=http://$NODE_RPC" "$ENV_FILE"; then
    echo "ERROR: RPC endpoint not in env file"
    cat "$ENV_FILE"
    exit 1
fi
echo "RPC endpoint configured correctly"

# Verify systemd service exists
if ! service_exists "dal-node" "$DAL_INSTANCE"; then
    echo "ERROR: DAL systemd service not found"
    exit 1
fi
echo "Systemd service exists"

# Verify no systemd dependency on local node (using RPC endpoint, not instance)
# Note: When using an RPC endpoint directly, there should be no BindsTo dependency
DROPIN_DIR="/etc/systemd/system/octez-dal-node@${DAL_INSTANCE}.service.d"
if [ -f "$DROPIN_DIR/override.conf" ]; then
    if grep -q "BindsTo=octez-node@" "$DROPIN_DIR/override.conf"; then
        echo "ERROR: Should not have BindsTo dependency for RPC endpoint"
        cat "$DROPIN_DIR/override.conf"
        exit 1
    fi
fi
echo "No local node dependency (correct for RPC endpoint)"

# Cleanup
cleanup_instance "$DAL_INSTANCE"
cleanup_instance "$NODE_INSTANCE"

echo "DAL RPC endpoint test passed"
