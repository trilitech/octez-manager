#!/bin/bash
# Test: Basic DAL node installation with local node reference
set -euo pipefail
source /tests/lib.sh

NODE_INSTANCE="test-dal-node"
DAL_INSTANCE="test-dal-basic"
NODE_RPC="127.0.0.1:18740"
NODE_NET="0.0.0.0:19760"
DAL_RPC="127.0.0.1:10740"
DAL_NET="0.0.0.0:11740"

echo "Test: Basic DAL node installation with local node"

cleanup_instance "$DAL_INSTANCE" || true
cleanup_instance "$NODE_INSTANCE" || true

# First install a node (DAL requires a node)
echo "Installing node..."
om install-node \
    --instance "$NODE_INSTANCE" \
    --network tallinnnet \
    --rpc-addr "$NODE_RPC" \
    --net-addr "$NODE_NET" \
    --service-user tezos \
    --no-enable 2>&1

# Install DAL node pointing to the local node
echo "Installing DAL node..."
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

# Verify env file exists
ENV_FILE="/etc/octez/instances/$DAL_INSTANCE/node.env"
if [ ! -f "$ENV_FILE" ]; then
    echo "ERROR: Env file not found: $ENV_FILE"
    exit 1
fi
echo "Env file exists"

# Verify env file contains expected values
if ! grep -q "OCTEZ_DAL_RPC_ADDR=$DAL_RPC" "$ENV_FILE"; then
    echo "ERROR: DAL RPC address not in env file"
    cat "$ENV_FILE"
    exit 1
fi
echo "DAL RPC address configured correctly"

if ! grep -q "OCTEZ_DAL_NET_ADDR=$DAL_NET" "$ENV_FILE"; then
    echo "ERROR: DAL net address not in env file"
    cat "$ENV_FILE"
    exit 1
fi
echo "DAL net address configured correctly"

# Verify node endpoint is set (should point to the node's RPC)
if ! grep -q "OCTEZ_NODE_ENDPOINT=http://$NODE_RPC" "$ENV_FILE"; then
    echo "ERROR: Node endpoint not correctly configured"
    cat "$ENV_FILE"
    exit 1
fi
echo "Node endpoint configured correctly"

# Verify systemd service file exists
if ! service_exists "dal-node" "$DAL_INSTANCE"; then
    echo "ERROR: DAL systemd service not found"
    exit 1
fi
echo "Systemd service exists"

# Verify service shows in list
if ! om list 2>&1 | grep -q "$DAL_INSTANCE"; then
    echo "ERROR: DAL instance not in list output"
    om list
    exit 1
fi
echo "DAL instance in list"

# Cleanup
cleanup_instance "$DAL_INSTANCE"
cleanup_instance "$NODE_INSTANCE"

echo "DAL basic install test passed"
