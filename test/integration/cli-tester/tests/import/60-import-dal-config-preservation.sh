#!/usr/bin/env bash
# Copyright (c) 2025 Trilitech <contact@trili.tech>
# Copyright (c) 2025 Functori <contact@functori.com>
#
# SPDX-License-Identifier: MIT

# Test: DAL node config.json is preserved during import
# Verifies: Custom DAL settings are not modified during import

set -euo pipefail
source /tests/lib.sh

test_init "Import DAL node - config.json preserved"

# Unique instance names for this test
NODE_INSTANCE="dal-cfg-node-$$"
DAL_INSTANCE="dal-cfg-dal-$$"
NODE_PORT=$(alloc_port)
NODE_RPC_PORT=$(alloc_port)
DAL_RPC_PORT=$(alloc_port)

# Register for cleanup
register_instance "$NODE_INSTANCE"
register_instance "$DAL_INSTANCE"

echo "Creating external node (required for DAL service file)..."

# Create node data directory
NODE_DATA_DIR="/var/lib/octez-external/$NODE_INSTANCE"
register_data_dir "$NODE_DATA_DIR"
mkdir -p "$NODE_DATA_DIR"
inject_identity "$NODE_INSTANCE" "$NODE_DATA_DIR"

# Initialize node config
octez-node config init --data-dir="$NODE_DATA_DIR" \
	--network=shadownet \
	--history-mode=rolling \
	--net-addr="127.0.0.1:$NODE_PORT" \
	--rpc-addr="127.0.0.1:$NODE_RPC_PORT" >/dev/null 2>&1

# Create node external service
register_external_service "node" "$NODE_INSTANCE"
chown -R tezos:tezos "$NODE_DATA_DIR"
create_external_service "node" "$NODE_INSTANCE" "$NODE_DATA_DIR" "127.0.0.1:$NODE_RPC_PORT" "shadownet"
systemctl enable "octez-node@${NODE_INSTANCE}.service"

echo "Creating external DAL node service with custom config..."

# Create DAL data directory
DAL_DATA_DIR="/var/lib/octez-external/$DAL_INSTANCE"
register_data_dir "$DAL_DATA_DIR"
mkdir -p "$DAL_DATA_DIR"

# Initialize DAL config using octez-dal-node
octez-dal-node config init --data-dir="$DAL_DATA_DIR" \
	--endpoint="http://127.0.0.1:$NODE_RPC_PORT" \
	--rpc-addr="127.0.0.1:$DAL_RPC_PORT" >/dev/null 2>&1

# Customize DAL config with extra fields
CONFIG_FILE="$DAL_DATA_DIR/config.json"
jq '.public_addr = "dal.example.com:10732"' \
	"$CONFIG_FILE" >"$CONFIG_FILE.tmp" && mv "$CONFIG_FILE.tmp" "$CONFIG_FILE"

echo "Custom DAL config created"

# Take hash of config before import
CONFIG_HASH_BEFORE=$(sha256sum "$CONFIG_FILE" | awk '{print $1}')
echo "Config hash before import: $CONFIG_HASH_BEFORE"

# Create DAL external service
register_external_service "dal-node" "$DAL_INSTANCE"
chown -R tezos:tezos "$DAL_DATA_DIR"
create_external_service "dal-node" "$DAL_INSTANCE" "$DAL_DATA_DIR" "127.0.0.1:$NODE_RPC_PORT" "shadownet" \
	"http://127.0.0.1:$NODE_RPC_PORT" "127.0.0.1:$DAL_RPC_PORT" "$NODE_INSTANCE"
systemctl enable "octez-dal-node@${DAL_INSTANCE}.service"

echo "Importing external DAL service..."
om import "octez-dal-node@${DAL_INSTANCE}" --strategy clone --network shadownet 2>&1

echo "Verifying DAL config.json preservation..."

# Check hash is unchanged
CONFIG_HASH_AFTER=$(sha256sum "$CONFIG_FILE" | awk '{print $1}')
if [ "$CONFIG_HASH_BEFORE" != "$CONFIG_HASH_AFTER" ]; then
	echo "ERROR: DAL config file was modified during import!"
	echo "Before: $CONFIG_HASH_BEFORE"
	echo "After:  $CONFIG_HASH_AFTER"
	exit 1
fi
echo "✓ DAL config hash unchanged"

# Verify specific DAL settings
PUBLIC_ADDR=$(jq -r '.public_addr' "$CONFIG_FILE")
if [ "$PUBLIC_ADDR" != "dal.example.com:10732" ]; then
	echo "ERROR: Public address not preserved: $PUBLIC_ADDR"
	exit 1
fi
echo "✓ Public address preserved"

# Verify service is managed
if ! service_is_managed "$DAL_INSTANCE"; then
	echo "ERROR: DAL service is not managed after import"
	om list 2>&1
	exit 1
fi
echo "✓ DAL service is managed after import"

echo "Test passed: DAL config preserved during import"
