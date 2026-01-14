#!/bin/bash
# Test: DAL node installation with custom data directory
# Verifies that the --data-dir option is correctly applied
set -euo pipefail
source /tests/lib.sh

NODE_INSTANCE="test-dal-datadir-node"
DAL_INSTANCE="test-dal-datadir"
NODE_RPC="127.0.0.1:18750"
NODE_NET="0.0.0.0:19770"
DAL_RPC="127.0.0.1:10750"
DAL_NET="0.0.0.0:11750"
CUSTOM_DATA_DIR="/var/lib/octez/custom-dal-data"

echo "Test: DAL node installation with custom data directory"

cleanup_instance "$DAL_INSTANCE" || true
cleanup_instance "$NODE_INSTANCE" || true
rm -rf "$CUSTOM_DATA_DIR" || true

# First install a node (DAL requires a node)
echo "Installing node..."
om install-node \
    --instance "$NODE_INSTANCE" \
    --network shadownet \
    --rpc-addr "$NODE_RPC" \
    --net-addr "$NODE_NET" \
    --service-user tezos \
    --no-enable 2>&1

# Install DAL node with custom data directory
echo "Installing DAL node with custom data-dir..."
om install-dal-node \
    --instance "$DAL_INSTANCE" \
    --node-instance "$NODE_INSTANCE" \
    --data-dir "$CUSTOM_DATA_DIR" \
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

# Verify env file exists
ENV_FILE="/etc/octez/instances/$DAL_INSTANCE/node.env"
if [ ! -f "$ENV_FILE" ]; then
    echo "ERROR: Env file not found: $ENV_FILE"
    exit 1
fi
echo "Env file exists"

# Verify env file contains the custom data directory
if ! grep -q "OCTEZ_DAL_DATA_DIR=$CUSTOM_DATA_DIR" "$ENV_FILE"; then
    echo "ERROR: Custom data directory not in env file"
    echo "Expected: OCTEZ_DAL_DATA_DIR=$CUSTOM_DATA_DIR"
    echo "Actual env file contents:"
    cat "$ENV_FILE"
    exit 1
fi
echo "Custom data directory configured correctly"

# Verify the registry has the correct data_dir
REGISTRY_DATA_DIR=$(om list --json 2>/dev/null | jq -r ".[] | select(.instance == \"$DAL_INSTANCE\") | .data_dir" 2>/dev/null || echo "")
if [ "$REGISTRY_DATA_DIR" != "$CUSTOM_DATA_DIR" ]; then
    echo "ERROR: Registry data_dir doesn't match"
    echo "Expected: $CUSTOM_DATA_DIR"
    echo "Got: $REGISTRY_DATA_DIR"
    om list --json 2>&1 | jq . || true
    exit 1
fi
echo "Registry data_dir matches"

# Cleanup
cleanup_instance "$DAL_INSTANCE"
cleanup_instance "$NODE_INSTANCE"
rm -rf "$CUSTOM_DATA_DIR" || true

echo "DAL custom data-dir test passed"
