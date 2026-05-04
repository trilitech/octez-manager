#!/usr/bin/env bash
# Copyright (c) 2025 Trilitech <contact@trili.tech>
# Copyright (c) 2025 Functori <contact@functori.com>
#
# SPDX-License-Identifier: MIT

# Test: Custom shell/storage settings in config.json are preserved during import
# Verifies: Block validator limits, prevalidator limits, and synchronisation threshold

set -euo pipefail
source /tests/lib.sh

test_init "Import node - custom shell/storage settings preserved"

# Unique instance name for this test
INSTANCE="node-storage-cfg-$$"
PORT=$(alloc_port)
RPC_PORT=$(alloc_port)

# Register for cleanup
register_instance "$INSTANCE"

echo "Creating external Octez node service with custom storage settings..."

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

# Customize shell settings in config.json
CONFIG_FILE="$DATA_DIR/config.json"
jq '.shell = {
  "history_mode": "rolling",
  "block_validator_limits": {
    "protocol_timeout": 120,
    "worker_limits": {
      "backlog_size": 1000,
      "backlog_level": 50
    }
  },
  "prevalidator_limits": {
    "max_refused_operations": 5000,
    "operation_timeout": 10,
    "operations_batch_size": 100
  },
  "synchronisation_threshold": 4
}' "$CONFIG_FILE" >"$CONFIG_FILE.tmp" && mv "$CONFIG_FILE.tmp" "$CONFIG_FILE"

echo "Custom shell config created"

# Take hash of config before import
CONFIG_HASH_BEFORE=$(sha256sum "$CONFIG_FILE" | awk '{print $1}')
echo "Config hash before import: $CONFIG_HASH_BEFORE"

# Create systemd service
register_external_service "node" "$INSTANCE"
chown -R tezos:tezos "$DATA_DIR"
create_external_service "node" "$INSTANCE" "$DATA_DIR" "127.0.0.1:$RPC_PORT" "shadownet"
systemctl enable "octez-node@${INSTANCE}.service"

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

# Verify specific storage settings
PROTOCOL_TIMEOUT=$(jq -r '.shell.block_validator_limits.protocol_timeout' "$CONFIG_FILE")
if [ "$PROTOCOL_TIMEOUT" != "120" ]; then
	echo "ERROR: Protocol timeout not preserved: $PROTOCOL_TIMEOUT"
	exit 1
fi
echo "✓ Protocol timeout preserved"

BACKLOG_SIZE=$(jq -r '.shell.block_validator_limits.worker_limits.backlog_size' "$CONFIG_FILE")
if [ "$BACKLOG_SIZE" != "1000" ]; then
	echo "ERROR: Backlog size not preserved: $BACKLOG_SIZE"
	exit 1
fi
echo "✓ Backlog size preserved"

MAX_REFUSED=$(jq -r '.shell.prevalidator_limits.max_refused_operations' "$CONFIG_FILE")
if [ "$MAX_REFUSED" != "5000" ]; then
	echo "ERROR: Max refused operations not preserved: $MAX_REFUSED"
	exit 1
fi
echo "✓ Max refused operations preserved"

SYNC_THRESHOLD=$(jq -r '.shell.synchronisation_threshold' "$CONFIG_FILE")
if [ "$SYNC_THRESHOLD" != "4" ]; then
	echo "ERROR: Synchronisation threshold not preserved: $SYNC_THRESHOLD"
	exit 1
fi
echo "✓ Synchronisation threshold preserved"

# Verify service is managed
if ! service_is_managed "$INSTANCE"; then
	echo "ERROR: Service is not managed after import"
	om list 2>&1
	exit 1
fi
echo "✓ Service is managed after import"

echo "Test passed: Custom storage settings preserved during import"
