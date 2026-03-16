#!/usr/bin/env bash
# Copyright (c) 2025 Trilitech <contact@trili.tech>
# Copyright (c) 2025 Functori <contact@functori.com>
#
# SPDX-License-Identifier: MIT

# Test: Import handles presence of both config.json and ExecStart CLI args
# Verifies: config.json is preserved as-is during import regardless of CLI args

set -euo pipefail
source /tests/lib.sh

test_init "Import node - config.json preserved alongside CLI args"

# Unique instance name for this test
INSTANCE="node-cfg-cli-$$"
PORT=$(alloc_port)
RPC_PORT=$(alloc_port)

# Register for cleanup
register_instance "$INSTANCE"

echo "Creating external node with config.json and CLI args..."

# Create data directory
DATA_DIR="/var/lib/octez-external/$INSTANCE"
register_data_dir "$DATA_DIR"
mkdir -p "$DATA_DIR"
inject_identity "$INSTANCE" "$DATA_DIR"

# Initialize node config
octez-node config init --data-dir="$DATA_DIR" \
	--network=shadownet \
	--history-mode=rolling \
	--net-addr="127.0.0.1:$PORT" \
	--rpc-addr="127.0.0.1:$RPC_PORT" >/dev/null 2>&1

# Customize config.json with CORS and private mode
CONFIG_FILE="$DATA_DIR/config.json"
jq '.rpc."cors-origin" = ["https://example.com"] |
    .p2p."private-mode" = true' \
	"$CONFIG_FILE" >"$CONFIG_FILE.tmp" && mv "$CONFIG_FILE.tmp" "$CONFIG_FILE"

echo "Config.json has CORS origin and private-mode"

# Take hash of config before import
CONFIG_HASH_BEFORE=$(sha256sum "$CONFIG_FILE" | awk '{print $1}')
echo "Config hash before import: $CONFIG_HASH_BEFORE"

# Create systemd service (the create_external_service adds CLI args to ExecStart)
register_external_service "node" "$INSTANCE"
chown -R tezos:tezos "$DATA_DIR"
create_external_service "node" "$INSTANCE" "$DATA_DIR" "127.0.0.1:$RPC_PORT" "shadownet"
systemctl enable "octez-node@${INSTANCE}.service"

echo "Importing external service..."
om import "octez-node@${INSTANCE}" --strategy clone --network shadownet 2>&1

echo "Verifying config.json preservation (despite CLI args in ExecStart)..."

# Check hash is unchanged - config.json should be preserved as-is
CONFIG_HASH_AFTER=$(sha256sum "$CONFIG_FILE" | awk '{print $1}')
if [ "$CONFIG_HASH_BEFORE" != "$CONFIG_HASH_AFTER" ]; then
	echo "ERROR: Config file was modified during import!"
	echo "Before: $CONFIG_HASH_BEFORE"
	echo "After:  $CONFIG_HASH_AFTER"
	exit 1
fi
echo "✓ Config hash unchanged (preserved as-is)"

# Verify CORS preserved
CORS_ORIGIN=$(jq -r '.rpc."cors-origin"[0]' "$CONFIG_FILE")
if [ "$CORS_ORIGIN" != "https://example.com" ]; then
	echo "ERROR: CORS origin not preserved: $CORS_ORIGIN"
	exit 1
fi
echo "✓ CORS origin preserved"

# Verify private-mode preserved
PRIVATE_MODE=$(jq -r '.p2p."private-mode"' "$CONFIG_FILE")
if [ "$PRIVATE_MODE" != "true" ]; then
	echo "ERROR: Private mode not preserved: $PRIVATE_MODE"
	exit 1
fi
echo "✓ Private mode preserved"

# Verify service is managed
if ! service_is_managed "$INSTANCE"; then
	echo "ERROR: Service is not managed after import"
	om list 2>&1
	exit 1
fi
echo "✓ Service is managed after import"

echo "Test passed: Config.json preserved alongside CLI args during import"
