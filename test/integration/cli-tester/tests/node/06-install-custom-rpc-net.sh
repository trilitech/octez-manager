#!/bin/bash
# Test: Install node with custom RPC and network addresses
set -euo pipefail
source /tests/lib.sh

INSTANCE="test-custom-addrs"
CUSTOM_RPC="0.0.0.0:18732"
CUSTOM_NET="0.0.0.0:19732"

echo "Test: Install node with custom RPC and network addresses"

cleanup_instance "$INSTANCE" || true

om install-node \
    --instance "$INSTANCE" \
    --network shadownet \
    --rpc-addr "$CUSTOM_RPC" \
    --net-addr "$CUSTOM_NET" \
    --service-user tezos \
    --no-enable 2>&1

# Verify RPC address in env (stored in OCTEZ_NODE_ARGS)
ENV_FILE="/etc/octez/instances/$INSTANCE/node.env"
if ! grep -q "18732" "$ENV_FILE"; then
    echo "ERROR: Custom RPC port not in env file"
    cat "$ENV_FILE"
    exit 1
fi
echo "Custom RPC address configured: $CUSTOM_RPC"

# Verify net address in env
if ! grep -q "19732" "$ENV_FILE"; then
    echo "ERROR: Custom net port not in env file"
    cat "$ENV_FILE"
    exit 1
fi
echo "Custom net address configured: $CUSTOM_NET"

# Cleanup
cleanup_instance "$INSTANCE"

echo "Custom RPC/net addresses test passed"
