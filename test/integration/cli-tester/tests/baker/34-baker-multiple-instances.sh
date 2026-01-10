#!/bin/bash
# Test: Multiple baker instances
set -euo pipefail
source /tests/lib.sh

NODE_INSTANCE="test-baker-multi-node"
BAKER_INSTANCE_1="test-baker-multi-1"
BAKER_INSTANCE_2="test-baker-multi-2"
NODE_RPC="127.0.0.1:18768"
NODE_NET="0.0.0.0:19788"

echo "Test: Multiple baker instances"

cleanup_instance "$BAKER_INSTANCE_1" || true
cleanup_instance "$BAKER_INSTANCE_2" || true
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

# Install first baker
echo "Installing first baker..."
om install-baker \
    --instance "$BAKER_INSTANCE_1" \
    --node-instance "$NODE_INSTANCE" \
    --liquidity-baking-vote on \
    --service-user tezos \
    --no-enable 2>&1

# Install second baker with different config
echo "Installing second baker..."
om install-baker \
    --instance "$BAKER_INSTANCE_2" \
    --node-instance "$NODE_INSTANCE" \
    --liquidity-baking-vote off \
    --service-user tezos \
    --no-enable 2>&1

# Verify both instances exist
if ! instance_exists "$BAKER_INSTANCE_1"; then
    echo "ERROR: First baker instance not in registry"
    exit 1
fi
echo "First baker instance registered"

if ! instance_exists "$BAKER_INSTANCE_2"; then
    echo "ERROR: Second baker instance not in registry"
    exit 1
fi
echo "Second baker instance registered"

# Verify both appear in list
LIST_OUTPUT=$(om list 2>&1)

if ! echo "$LIST_OUTPUT" | grep -q "$BAKER_INSTANCE_1"; then
    echo "ERROR: First baker instance not in list"
    echo "$LIST_OUTPUT"
    exit 1
fi

if ! echo "$LIST_OUTPUT" | grep -q "$BAKER_INSTANCE_2"; then
    echo "ERROR: Second baker instance not in list"
    echo "$LIST_OUTPUT"
    exit 1
fi
echo "Both instances in list"

# Verify they have different env files with different configs
ENV_FILE_1="/etc/octez/instances/$BAKER_INSTANCE_1/node.env"
ENV_FILE_2="/etc/octez/instances/$BAKER_INSTANCE_2/node.env"

if [ ! -f "$ENV_FILE_1" ]; then
    echo "ERROR: First env file missing"
    exit 1
fi

if [ ! -f "$ENV_FILE_2" ]; then
    echo "ERROR: Second env file missing"
    exit 1
fi

# Verify different LB votes
if ! grep -q "OCTEZ_BAKER_LB_VOTE=on" "$ENV_FILE_1"; then
    echo "ERROR: First baker LB vote incorrect"
    exit 1
fi

if ! grep -q "OCTEZ_BAKER_LB_VOTE=off" "$ENV_FILE_2"; then
    echo "ERROR: Second baker LB vote incorrect"
    exit 1
fi
echo "Different configs set correctly"

# Verify both systemd services exist
if ! service_exists "baker" "$BAKER_INSTANCE_1"; then
    echo "ERROR: First baker systemd service not found"
    exit 1
fi

if ! service_exists "baker" "$BAKER_INSTANCE_2"; then
    echo "ERROR: Second baker systemd service not found"
    exit 1
fi
echo "Both systemd services exist"

# Cleanup
cleanup_instance "$BAKER_INSTANCE_1"
cleanup_instance "$BAKER_INSTANCE_2"
cleanup_instance "$NODE_INSTANCE"

echo "Baker multiple instances test passed"
