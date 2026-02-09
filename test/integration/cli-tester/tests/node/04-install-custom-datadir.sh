#!/bin/bash
# Test: Install node with custom data directory
set -euo pipefail
source /tests/lib.sh

test_init "Install node with custom data directory"

INSTANCE="test-custom-datadir"
CUSTOM_DATA_DIR="/tmp/octez-custom-data"

# Register instance and data dir for auto cleanup (also does pre-cleanup)
register_instance "$INSTANCE"
register_data_dir "$CUSTOM_DATA_DIR"

RPC_PORT=$(alloc_port)
NET_PORT=$(alloc_port)

# Install with custom data-dir
om install-node \
	--instance "$INSTANCE" \
	--network shadownet \
	--data-dir "$CUSTOM_DATA_DIR" \
	--rpc-addr "127.0.0.1:$RPC_PORT" --net-addr "0.0.0.0:$NET_PORT" \
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

echo "Custom data directory test passed"
