#!/bin/bash
# Test: Install node with custom data directory
set -euo pipefail
source /tests/lib.sh

INSTANCE="test-custom-datadir"
CUSTOM_DATA_DIR="/tmp/octez-custom-data"

echo "Test: Install node with custom data directory"

cleanup_instance "$INSTANCE" || true
rm -rf "$CUSTOM_DATA_DIR"

# Install with custom data-dir
om install-node \
    --instance "$INSTANCE" \
    --network shadownet \
    --data-dir "$CUSTOM_DATA_DIR" \
    --rpc-addr "127.0.0.1:8733" --net-addr "0.0.0.0:9733" \
    --service-user tezos \
    --no-enable 2>&1

# Verify custom data directory was created
if [ ! -d "$CUSTOM_DATA_DIR" ]; then
    echo "ERROR: Custom data directory not created: $CUSTOM_DATA_DIR"
    exit 1
fi
echo "Custom data directory created: $CUSTOM_DATA_DIR"

# Verify config.json is in custom location
if [ ! -f "$CUSTOM_DATA_DIR/config.json" ]; then
    echo "ERROR: config.json not in custom data directory"
    exit 1
fi
echo "config.json found in custom location"

# Verify env file references custom data dir
ENV_FILE="/etc/octez/instances/$INSTANCE/node.env"
if ! grep -q "$CUSTOM_DATA_DIR" "$ENV_FILE"; then
    echo "ERROR: Env file doesn't reference custom data dir"
    cat "$ENV_FILE"
    exit 1
fi
echo "Env file correctly references custom data dir"

# Cleanup
cleanup_instance "$INSTANCE"
rm -rf "$CUSTOM_DATA_DIR"

echo "Custom data directory test passed"
