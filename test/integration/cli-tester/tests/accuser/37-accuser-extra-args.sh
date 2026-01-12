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
    --network shadownet \
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

# Verify accuser was installed successfully
if ! om list | grep -q "$ACCUSER_INSTANCE"; then
    echo "ERROR: Accuser not found in list"
    exit 1
fi
echo "Accuser installed successfully with extra args"

# Cleanup
cleanup_instance "$ACCUSER_INSTANCE"
cleanup_instance "$NODE_INSTANCE"

echo "Accuser extra args test passed"
