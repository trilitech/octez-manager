#!/bin/bash
# Test: Install node with extra arguments
set -euo pipefail
source /tests/lib.sh

INSTANCE="test-extra-args"

echo "Test: Install node with extra arguments"

cleanup_instance "$INSTANCE" || true

om install-node \
    --instance "$INSTANCE" \
    --network shadownet \
    --rpc-addr "127.0.0.1:8736" \
    --net-addr "0.0.0.0:9736" \
    --service-user tezos \
    --extra-arg="--connections=50" \
    --extra-arg="--log-output=stdout" \
    --no-enable 2>&1

# Verify extra args in env file
ENV_FILE="/etc/octez/instances/$INSTANCE/node.env"
if ! grep -q "\-\-connections=50" "$ENV_FILE"; then
    echo "ERROR: --connections=50 not in env file"
    cat "$ENV_FILE"
    exit 1
fi
echo "Extra arg --connections=50 found"

if ! grep -q "\-\-log-output=stdout" "$ENV_FILE"; then
    echo "ERROR: --log-output=stdout not in env file"
    cat "$ENV_FILE"
    exit 1
fi
echo "Extra arg --log-output=stdout found"

# Cleanup
cleanup_instance "$INSTANCE"

echo "Extra arguments test passed"
