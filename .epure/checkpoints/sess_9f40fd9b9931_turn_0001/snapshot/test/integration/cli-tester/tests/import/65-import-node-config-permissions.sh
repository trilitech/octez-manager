#!/usr/bin/env bash
# Copyright (c) 2025 Trilitech <contact@trili.tech>
# Copyright (c) 2025 Functori <contact@functori.com>
#
# SPDX-License-Identifier: MIT

# Test: Import preserves file ownership for data directory
# Verifies: Data directory remains owned by tezos user after import

set -euo pipefail
source /tests/lib.sh

test_init "Import node - data directory ownership preserved"

# Unique instance name for this test
INSTANCE="node-perms-$$"
PORT=$(alloc_port)
RPC_PORT=$(alloc_port)

# Register for cleanup
register_instance "$INSTANCE"

echo "Creating external node with tezos ownership..."

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

CONFIG_FILE="$DATA_DIR/config.json"

# Take hash of config before import
CONFIG_HASH_BEFORE=$(sha256sum "$CONFIG_FILE" | awk '{print $1}')
echo "Config hash before import: $CONFIG_HASH_BEFORE"

# Create systemd service (create_external_service sets tezos:tezos ownership)
register_external_service "node" "$INSTANCE"
chown -R tezos:tezos "$DATA_DIR"
create_external_service "node" "$INSTANCE" "$DATA_DIR" "127.0.0.1:$RPC_PORT" "shadownet"
systemctl enable "octez-node@${INSTANCE}.service"

# Record ownership before import
DIR_OWNER_BEFORE=$(stat -c '%U:%G' "$DATA_DIR")
CONFIG_OWNER_BEFORE=$(stat -c '%U:%G' "$CONFIG_FILE")
echo "Directory owner before import: $DIR_OWNER_BEFORE"
echo "Config owner before import: $CONFIG_OWNER_BEFORE"

echo "Importing external service..."
om import "octez-node@${INSTANCE}" --strategy clone --network shadownet 2>&1

echo "Verifying config.json preservation..."

# Check hash is unchanged
CONFIG_HASH_AFTER=$(sha256sum "$CONFIG_FILE" | awk '{print $1}')
if [ "$CONFIG_HASH_BEFORE" != "$CONFIG_HASH_AFTER" ]; then
	echo "ERROR: Config file was modified during import!"
	echo "Before: $CONFIG_HASH_BEFORE"
	echo "After:  $CONFIG_HASH_AFTER"
	exit 1
fi
echo "✓ Config hash unchanged"

echo "Verifying ownership after import..."

# Check data directory is owned by tezos
DIR_OWNER_AFTER=$(stat -c '%U:%G' "$DATA_DIR")
if [ "$DIR_OWNER_AFTER" != "tezos:tezos" ]; then
	echo "ERROR: Directory ownership incorrect after import: $DIR_OWNER_AFTER (expected tezos:tezos)"
	exit 1
fi
echo "✓ Directory ownership correct: $DIR_OWNER_AFTER"

# Check config.json is owned by tezos
CONFIG_OWNER_AFTER=$(stat -c '%U:%G' "$CONFIG_FILE")
if [ "$CONFIG_OWNER_AFTER" != "tezos:tezos" ]; then
	echo "ERROR: config.json ownership incorrect after import: $CONFIG_OWNER_AFTER (expected tezos:tezos)"
	exit 1
fi
echo "✓ config.json ownership correct: $CONFIG_OWNER_AFTER"

# Verify service is managed
if ! service_is_managed "$INSTANCE"; then
	echo "ERROR: Service is not managed after import"
	om list 2>&1
	exit 1
fi
echo "✓ Service is managed after import"

echo "Test passed: Data directory ownership preserved during import"
