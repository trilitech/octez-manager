#!/bin/bash
# Test: Basic accuser installation with local node reference
set -euo pipefail
source /tests/lib.sh

NODE_INSTANCE="test-accuser-basic-node"
ACCUSER_INSTANCE="test-accuser-basic"
NODE_RPC="127.0.0.1:18770"
NODE_NET="0.0.0.0:19790"

echo "Test: Basic accuser installation"

cleanup_instance "$ACCUSER_INSTANCE" || true
cleanup_instance "$NODE_INSTANCE" || true

# Install a node first (accuser needs a node reference)
echo "Installing node..."
om install-node \
    --instance "$NODE_INSTANCE" \
    --network shadownet \
    --rpc-addr "$NODE_RPC" \
    --net-addr "$NODE_NET" \
    --service-user tezos \
    --no-enable 2>&1

# Install accuser with node reference
echo "Installing accuser..."
om install-accuser \
    --instance "$ACCUSER_INSTANCE" \
    --node-instance "$NODE_INSTANCE" \
    --service-user tezos \
    --no-enable 2>&1

# Verify instance is in registry
if ! instance_exists "$ACCUSER_INSTANCE"; then
    echo "ERROR: Accuser instance not in registry"
    exit 1
fi
echo "Accuser instance registered"

# Verify env file exists
ENV_FILE="/etc/octez/instances/$ACCUSER_INSTANCE/node.env"
if [ ! -f "$ENV_FILE" ]; then
    echo "ERROR: Env file not found at $ENV_FILE"
    exit 1
fi
echo "Env file exists"

# Verify node endpoint is configured
if ! grep -q "OCTEZ_NODE_ENDPOINT=http://$NODE_RPC" "$ENV_FILE"; then
    echo "ERROR: Node endpoint not in env file"
    cat "$ENV_FILE"
    exit 1
fi
echo "Node endpoint configured"

# Verify systemd service exists
if ! service_exists "accuser" "$ACCUSER_INSTANCE"; then
    echo "ERROR: Systemd service not found"
    exit 1
fi
echo "Systemd service exists"

# Cleanup
cleanup_instance "$ACCUSER_INSTANCE"
cleanup_instance "$NODE_INSTANCE"

echo "Accuser basic install test passed"
