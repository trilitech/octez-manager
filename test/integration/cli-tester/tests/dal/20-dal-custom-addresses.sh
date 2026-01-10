#!/bin/bash
# Test: DAL node installation with custom RPC and net addresses
set -euo pipefail
source /tests/lib.sh

DAL_INSTANCE="test-dal-custom"
REMOTE_ENDPOINT="http://remote-node.example.com:8732"
# Non-default addresses
CUSTOM_RPC="0.0.0.0:10799"
CUSTOM_NET="192.168.1.100:11799"

echo "Test: DAL node installation with custom addresses"

cleanup_instance "$DAL_INSTANCE" || true

# Install DAL node with custom addresses
echo "Installing DAL node with custom addresses..."
om install-dal-node \
    --instance "$DAL_INSTANCE" \
    --node-instance "$REMOTE_ENDPOINT" \
    --rpc-addr "$CUSTOM_RPC" \
    --net-addr "$CUSTOM_NET" \
    --service-user tezos \
    --no-enable 2>&1

# Verify env file contains custom addresses
ENV_FILE="/etc/octez/instances/$DAL_INSTANCE/node.env"

if ! grep -q "OCTEZ_DAL_RPC_ADDR=$CUSTOM_RPC" "$ENV_FILE"; then
    echo "ERROR: Custom RPC address not in env file"
    cat "$ENV_FILE"
    exit 1
fi
echo "Custom RPC address configured: $CUSTOM_RPC"

if ! grep -q "OCTEZ_DAL_NET_ADDR=$CUSTOM_NET" "$ENV_FILE"; then
    echo "ERROR: Custom net address not in env file"
    cat "$ENV_FILE"
    exit 1
fi
echo "Custom net address configured: $CUSTOM_NET"

# Cleanup
cleanup_instance "$DAL_INSTANCE"

echo "DAL custom addresses test passed"
