#!/usr/bin/env bash
# Copyright (c) 2025 Trilitech <contact@trili.tech>
# Copyright (c) 2025 Functori <contact@functori.com>
#
# SPDX-License-Identifier: MIT

# Test: Custom storage backend settings in config.json are preserved during import
# Verifies: context storage backend and storage-related configuration

set -euo pipefail

SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
source "$SCRIPT_DIR/../lib.sh"

# Unique instance name for this test
INSTANCE="node-storage-config-test-$$"
PORT=$(alloc_port)
RPC_PORT=$(alloc_port)

# Register for cleanup
register_instance "$INSTANCE"

log "Creating external Octez node service with custom storage settings..."

# Create data directory
DATA_DIR="/tmp/octez-external-storage-test-$$"
register_data_dir "$DATA_DIR"
mkdir -p "$DATA_DIR"

# Initialize node config and identity
octez-node config init --data-dir="$DATA_DIR" \
	--network=weeklynet \
	--history-mode=rolling \
	--net-addr="127.0.0.1:$PORT" \
	--rpc-addr="127.0.0.1:$RPC_PORT" >/dev/null 2>&1

octez-node identity generate --data-dir="$DATA_DIR" >/dev/null 2>&1

# Customize storage settings in config.json
# Note: context storage backend options may vary by Octez version
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

log "Custom storage config created with:"
log "  - Protocol timeout: 120s"
log "  - Backlog size: 1000"
log "  - Max refused operations: 5000"
log "  - Operations batch size: 100"
log "  - Synchronisation threshold: 4"

# Take hash of config before import
CONFIG_HASH_BEFORE=$(sha256sum "$CONFIG_FILE" | awk '{print $1}')
log "Config hash before import: $CONFIG_HASH_BEFORE"

# Create systemd service
SERVICE_NAME="octez-node-${INSTANCE}"
SERVICE_FILE="/etc/systemd/system/${SERVICE_NAME}.service"

sudo tee "$SERVICE_FILE" >/dev/null <<EOF
[Unit]
Description=Octez Node - ${INSTANCE}
After=network.target

[Service]
Type=simple
User=octez
ExecStart=/usr/bin/octez-node run --data-dir=${DATA_DIR} --network=weeklynet --history-mode=rolling
Restart=on-failure
StandardOutput=journal
StandardError=journal

[Install]
WantedBy=multi-user.target
EOF

# Set ownership
sudo chown -R octez:octez "$DATA_DIR"

# Reload systemd
sudo systemctl daemon-reload

log "Importing external service with takeover..."
expect_success octez-manager import detect
expect_success octez-manager import takeover "$SERVICE_NAME" "$INSTANCE"

log "Verifying config.json preservation..."

# Check hash is unchanged
CONFIG_HASH_AFTER=$(sha256sum "$CONFIG_FILE" | awk '{print $1}')
if [ "$CONFIG_HASH_BEFORE" != "$CONFIG_HASH_AFTER" ]; then
	error "Config file was modified during import!"
	error "Before: $CONFIG_HASH_BEFORE"
	error "After:  $CONFIG_HASH_AFTER"
	exit 1
fi

log "✓ Config hash unchanged"

# Verify specific storage settings
log "Verifying storage settings in config.json..."

# Check protocol timeout
PROTOCOL_TIMEOUT=$(jq -r '.shell.block_validator_limits.protocol_timeout' "$CONFIG_FILE")
if [ "$PROTOCOL_TIMEOUT" != "120" ]; then
	error "Protocol timeout not preserved: $PROTOCOL_TIMEOUT"
	exit 1
fi
log "✓ Protocol timeout preserved"

# Check backlog size
BACKLOG_SIZE=$(jq -r '.shell.block_validator_limits.worker_limits.backlog_size' "$CONFIG_FILE")
if [ "$BACKLOG_SIZE" != "1000" ]; then
	error "Backlog size not preserved: $BACKLOG_SIZE"
	exit 1
fi
log "✓ Backlog size preserved"

# Check backlog level
BACKLOG_LEVEL=$(jq -r '.shell.block_validator_limits.worker_limits.backlog_level' "$CONFIG_FILE")
if [ "$BACKLOG_LEVEL" != "50" ]; then
	error "Backlog level not preserved: $BACKLOG_LEVEL"
	exit 1
fi
log "✓ Backlog level preserved"

# Check max refused operations
MAX_REFUSED=$(jq -r '.shell.prevalidator_limits.max_refused_operations' "$CONFIG_FILE")
if [ "$MAX_REFUSED" != "5000" ]; then
	error "Max refused operations not preserved: $MAX_REFUSED"
	exit 1
fi
log "✓ Max refused operations preserved"

# Check operation timeout
OP_TIMEOUT=$(jq -r '.shell.prevalidator_limits.operation_timeout' "$CONFIG_FILE")
if [ "$OP_TIMEOUT" != "10" ]; then
	error "Operation timeout not preserved: $OP_TIMEOUT"
	exit 1
fi
log "✓ Operation timeout preserved"

# Check operations batch size
BATCH_SIZE=$(jq -r '.shell.prevalidator_limits.operations_batch_size' "$CONFIG_FILE")
if [ "$BATCH_SIZE" != "100" ]; then
	error "Operations batch size not preserved: $BATCH_SIZE"
	exit 1
fi
log "✓ Operations batch size preserved"

# Check synchronisation threshold
SYNC_THRESHOLD=$(jq -r '.shell.synchronisation_threshold' "$CONFIG_FILE")
if [ "$SYNC_THRESHOLD" != "4" ]; then
	error "Synchronisation threshold not preserved: $SYNC_THRESHOLD"
	exit 1
fi
log "✓ Synchronisation threshold preserved"

log "Verifying managed service can start with preserved config..."
expect_success octez-manager start "$INSTANCE"

# Wait for node to be responsive
wait_for_node_rpc "$INSTANCE" 60

log "✓ Node started successfully with preserved storage config"

expect_success octez-manager stop "$INSTANCE"

log "✓ Test passed: Custom storage settings preserved during import"
