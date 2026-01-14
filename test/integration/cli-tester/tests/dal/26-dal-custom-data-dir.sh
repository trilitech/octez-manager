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

# Start the node first (DAL node depends on it)
echo "Starting node..."
om instance "$NODE_INSTANCE" start 2>&1
wait_for_service_active "node" "$NODE_INSTANCE"
echo "Node started"

# Start the DAL node
echo "Starting DAL node..."
om instance "$DAL_INSTANCE" start 2>&1

# Wait for service to start and create data
echo "Waiting for DAL node to initialize..."
sleep 5

# Verify the custom data directory was created and populated
if [ ! -d "$CUSTOM_DATA_DIR" ]; then
    echo "ERROR: Custom data directory was not created"
    echo "Expected directory: $CUSTOM_DATA_DIR"
    ls -la /var/lib/octez/ || true
    exit 1
fi
echo "Custom data directory exists"

# Check that the directory contains data (config.json or other files)
if [ -z "$(ls -A "$CUSTOM_DATA_DIR" 2>/dev/null)" ]; then
    echo "ERROR: Custom data directory is empty"
    echo "DAL node did not write data to the custom directory"
    exit 1
fi
echo "Custom data directory contains data:"
ls -la "$CUSTOM_DATA_DIR"

# Stop services before cleanup
echo "Stopping services..."
om instance "$DAL_INSTANCE" stop 2>&1 || true
om instance "$NODE_INSTANCE" stop 2>&1 || true

# Cleanup
cleanup_instance "$DAL_INSTANCE"
cleanup_instance "$NODE_INSTANCE"
rm -rf "$CUSTOM_DATA_DIR" || true

echo "DAL custom data-dir test passed"
