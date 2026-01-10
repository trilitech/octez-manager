#!/bin/bash
# Test: Accuser installation with extra arguments
set -euo pipefail
source /tests/lib.sh

NODE_INSTANCE="test-accuser-extra-node"
ACCUSER_INSTANCE="test-accuser-extra"
NODE_RPC="127.0.0.1:18772"
NODE_NET="0.0.0.0:19792"

echo "Test: Accuser installation with extra arguments"

cleanup_instance "$ACCUSER_INSTANCE" || true
cleanup_instance "$NODE_INSTANCE" || true

# Install a node first
echo "Installing node..."
om install-node \
    --instance "$NODE_INSTANCE" \
    --network tallinnnet \
    --rpc-addr "$NODE_RPC" \
    --net-addr "$NODE_NET" \
    --service-user tezos \
    --no-enable 2>&1

# Install accuser with extra args
echo "Installing accuser with extra args..."
om install-accuser \
    --instance "$ACCUSER_INSTANCE" \
    --node-instance "$NODE_INSTANCE" \
    --extra-arg="--preserved-levels=10" \
    --service-user tezos \
    --no-enable 2>&1

# Verify env file contains extra args
ENV_FILE="/etc/octez/instances/$ACCUSER_INSTANCE/node.env"

if ! grep -q "OCTEZ_SERVICE_ARGS=.*--preserved-levels" "$ENV_FILE"; then
    echo "ERROR: Extra args not in env file"
    cat "$ENV_FILE"
    exit 1
fi
echo "Extra args configured correctly"

# Cleanup
cleanup_instance "$ACCUSER_INSTANCE"
cleanup_instance "$NODE_INSTANCE"

echo "Accuser extra args test passed"
