#!/bin/bash
# Test: DAL node installation with extra arguments
set -euo pipefail
source /tests/lib.sh

DAL_INSTANCE="test-dal-extra"
REMOTE_ENDPOINT="http://remote-node.example.com:8732"
DAL_RPC="127.0.0.1:10742"
DAL_NET="0.0.0.0:11742"

echo "Test: DAL node installation with extra arguments"

cleanup_instance "$DAL_INSTANCE" || true

# Install DAL node with extra args
echo "Installing DAL node with extra args..."
om install-dal-node \
    --instance "$DAL_INSTANCE" \
    --node-instance "$REMOTE_ENDPOINT" \
    --rpc-addr "$DAL_RPC" \
    --net-addr "$DAL_NET" \
    --service-user tezos \
    --extra-arg="--metrics-addr=0.0.0.0:9933" \
    --no-enable 2>&1

# Verify env file contains extra args
ENV_FILE="/etc/octez/instances/$DAL_INSTANCE/node.env"

if ! grep -q "OCTEZ_SERVICE_ARGS=.*--metrics-addr=0.0.0.0:9933" "$ENV_FILE"; then
    echo "ERROR: Extra args not in env file"
    cat "$ENV_FILE"
    exit 1
fi
echo "Extra args configured correctly"

# Test with multiple extra args
cleanup_instance "$DAL_INSTANCE"

echo "Installing DAL node with multiple extra args..."
om install-dal-node \
    --instance "$DAL_INSTANCE" \
    --node-instance "$REMOTE_ENDPOINT" \
    --rpc-addr "$DAL_RPC" \
    --net-addr "$DAL_NET" \
    --service-user tezos \
    --extra-arg="--metrics-addr=0.0.0.0:9933" \
    --extra-arg="--log-output=stdout" \
    --no-enable 2>&1

# Verify both extra args are present
if ! grep -q "OCTEZ_SERVICE_ARGS=.*--metrics-addr=0.0.0.0:9933" "$ENV_FILE"; then
    echo "ERROR: First extra arg not in env file"
    cat "$ENV_FILE"
    exit 1
fi

if ! grep -q "OCTEZ_SERVICE_ARGS=.*--log-output=stdout" "$ENV_FILE"; then
    echo "ERROR: Second extra arg not in env file"
    cat "$ENV_FILE"
    exit 1
fi
echo "Multiple extra args configured correctly"

# Cleanup
cleanup_instance "$DAL_INSTANCE"

echo "DAL extra args test passed"
