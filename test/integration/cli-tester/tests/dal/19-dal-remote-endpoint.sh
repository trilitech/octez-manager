#!/bin/bash
# Test: DAL node installation with remote RPC endpoint
set -euo pipefail
source /tests/lib.sh

DAL_INSTANCE="test-dal-remote"
REMOTE_ENDPOINT="http://remote-node.example.com:8732"
DAL_RPC="127.0.0.1:10741"
DAL_NET="0.0.0.0:11741"

echo "Test: DAL node installation with remote endpoint"

cleanup_instance "$DAL_INSTANCE" || true

# Install DAL node with remote endpoint (no local node needed)
echo "Installing DAL node with remote endpoint..."
om install-dal-node \
    --instance "$DAL_INSTANCE" \
    --node-instance "$REMOTE_ENDPOINT" \
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

# Verify env file contains remote endpoint
ENV_FILE="/etc/octez/instances/$DAL_INSTANCE/node.env"
if ! grep -q "OCTEZ_NODE_ENDPOINT=$REMOTE_ENDPOINT" "$ENV_FILE"; then
    echo "ERROR: Remote endpoint not in env file"
    cat "$ENV_FILE"
    exit 1
fi
echo "Remote endpoint configured correctly"

# Verify systemd service exists
if ! service_exists "dal-node" "$DAL_INSTANCE"; then
    echo "ERROR: DAL systemd service not found"
    exit 1
fi
echo "Systemd service exists"

# Verify no systemd dependency on local node (since using remote)
DROPIN_DIR="/etc/systemd/system/octez-dal-node@${DAL_INSTANCE}.service.d"
if [ -f "$DROPIN_DIR/override.conf" ]; then
    if grep -q "BindsTo=octez-node@" "$DROPIN_DIR/override.conf"; then
        echo "ERROR: Should not have BindsTo dependency for remote endpoint"
        cat "$DROPIN_DIR/override.conf"
        exit 1
    fi
fi
echo "No local node dependency (correct for remote)"

# Cleanup
cleanup_instance "$DAL_INSTANCE"

echo "DAL remote endpoint test passed"
