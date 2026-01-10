#!/bin/bash
# Test: Multiple DAL node instances
set -euo pipefail
source /tests/lib.sh

DAL_INSTANCE_1="test-dal-multi-1"
DAL_INSTANCE_2="test-dal-multi-2"
REMOTE_ENDPOINT="http://remote-node.example.com:8732"
DAL_RPC_1="127.0.0.1:10760"
DAL_NET_1="0.0.0.0:11760"
DAL_RPC_2="127.0.0.1:10761"
DAL_NET_2="0.0.0.0:11761"

echo "Test: Multiple DAL node instances"

cleanup_instance "$DAL_INSTANCE_1" || true
cleanup_instance "$DAL_INSTANCE_2" || true

# Install first DAL node
echo "Installing first DAL node..."
om install-dal-node \
    --instance "$DAL_INSTANCE_1" \
    --node-instance "$REMOTE_ENDPOINT" \
    --rpc-addr "$DAL_RPC_1" \
    --net-addr "$DAL_NET_1" \
    --service-user tezos \
    --no-enable 2>&1

# Install second DAL node
echo "Installing second DAL node..."
om install-dal-node \
    --instance "$DAL_INSTANCE_2" \
    --node-instance "$REMOTE_ENDPOINT" \
    --rpc-addr "$DAL_RPC_2" \
    --net-addr "$DAL_NET_2" \
    --service-user tezos \
    --no-enable 2>&1

# Verify both instances exist
if ! instance_exists "$DAL_INSTANCE_1"; then
    echo "ERROR: First DAL instance not in registry"
    exit 1
fi
echo "First DAL instance registered"

if ! instance_exists "$DAL_INSTANCE_2"; then
    echo "ERROR: Second DAL instance not in registry"
    exit 1
fi
echo "Second DAL instance registered"

# Verify both appear in list
LIST_OUTPUT=$(om list 2>&1)

if ! echo "$LIST_OUTPUT" | grep -q "$DAL_INSTANCE_1"; then
    echo "ERROR: First DAL instance not in list"
    echo "$LIST_OUTPUT"
    exit 1
fi

if ! echo "$LIST_OUTPUT" | grep -q "$DAL_INSTANCE_2"; then
    echo "ERROR: Second DAL instance not in list"
    echo "$LIST_OUTPUT"
    exit 1
fi
echo "Both instances in list"

# Verify they have different env files
ENV_FILE_1="/etc/octez/instances/$DAL_INSTANCE_1/node.env"
ENV_FILE_2="/etc/octez/instances/$DAL_INSTANCE_2/node.env"

if [ ! -f "$ENV_FILE_1" ]; then
    echo "ERROR: First env file missing"
    exit 1
fi

if [ ! -f "$ENV_FILE_2" ]; then
    echo "ERROR: Second env file missing"
    exit 1
fi

# Verify different ports
if ! grep -q "OCTEZ_DAL_RPC_ADDR=$DAL_RPC_1" "$ENV_FILE_1"; then
    echo "ERROR: First DAL RPC address incorrect"
    exit 1
fi

if ! grep -q "OCTEZ_DAL_RPC_ADDR=$DAL_RPC_2" "$ENV_FILE_2"; then
    echo "ERROR: Second DAL RPC address incorrect"
    exit 1
fi
echo "Different ports configured correctly"

# Verify both systemd services exist
if ! service_exists "dal-node" "$DAL_INSTANCE_1"; then
    echo "ERROR: First DAL systemd service not found"
    exit 1
fi

if ! service_exists "dal-node" "$DAL_INSTANCE_2"; then
    echo "ERROR: Second DAL systemd service not found"
    exit 1
fi
echo "Both systemd services exist"

# Cleanup
cleanup_instance "$DAL_INSTANCE_1"
cleanup_instance "$DAL_INSTANCE_2"

echo "DAL multiple instances test passed"
