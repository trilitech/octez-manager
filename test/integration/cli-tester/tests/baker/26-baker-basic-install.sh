#!/bin/bash
# Test: Basic baker installation with local node reference
set -euo pipefail
source /tests/lib.sh

NODE_INSTANCE="test-baker-node"
BAKER_INSTANCE="test-baker-basic"
NODE_RPC="127.0.0.1:18760"
NODE_NET="0.0.0.0:19780"

echo "Test: Basic baker installation with local node"

cleanup_instance "$BAKER_INSTANCE" || true
cleanup_instance "$NODE_INSTANCE" || true

# First install a node (baker requires a node)
echo "Installing node..."
om install-node \
    --instance "$NODE_INSTANCE" \
    --network tallinnnet \
    --rpc-addr "$NODE_RPC" \
    --net-addr "$NODE_NET" \
    --service-user tezos \
    --no-enable 2>&1

# Install baker pointing to the local node
echo "Installing baker..."
om install-baker \
    --instance "$BAKER_INSTANCE" \
    --node-instance "$NODE_INSTANCE" \
    --liquidity-baking-vote pass \
    --service-user tezos \
    --no-enable 2>&1

# Verify baker instance exists
if ! instance_exists "$BAKER_INSTANCE"; then
    echo "ERROR: Baker instance not in registry"
    exit 1
fi
echo "Baker instance registered"

# Verify env file exists
ENV_FILE="/etc/octez/instances/$BAKER_INSTANCE/node.env"
if [ ! -f "$ENV_FILE" ]; then
    echo "ERROR: Env file not found: $ENV_FILE"
    exit 1
fi
echo "Env file exists"

# Verify env file contains expected values
if ! grep -q "OCTEZ_NODE_ENDPOINT=http://$NODE_RPC" "$ENV_FILE"; then
    echo "ERROR: Node endpoint not in env file"
    cat "$ENV_FILE"
    exit 1
fi
echo "Node endpoint configured correctly"

if ! grep -q "OCTEZ_BAKER_LB_VOTE=pass" "$ENV_FILE"; then
    echo "ERROR: Liquidity baking vote not in env file"
    cat "$ENV_FILE"
    exit 1
fi
echo "Liquidity baking vote configured correctly"

if ! grep -q "OCTEZ_BAKER_NODE_MODE=local" "$ENV_FILE"; then
    echo "ERROR: Node mode not set to local"
    cat "$ENV_FILE"
    exit 1
fi
echo "Node mode configured correctly"

# Verify systemd service file exists
if ! service_exists "baker" "$BAKER_INSTANCE"; then
    echo "ERROR: Baker systemd service not found"
    exit 1
fi
echo "Systemd service exists"

# Verify service shows in list
if ! om list 2>&1 | grep -q "$BAKER_INSTANCE"; then
    echo "ERROR: Baker instance not in list output"
    om list
    exit 1
fi
echo "Baker instance in list"

# Cleanup
cleanup_instance "$BAKER_INSTANCE"
cleanup_instance "$NODE_INSTANCE"

echo "Baker basic install test passed"
