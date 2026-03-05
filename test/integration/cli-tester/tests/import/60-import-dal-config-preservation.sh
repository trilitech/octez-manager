#!/usr/bin/env bash
# Copyright (c) 2025 Trilitech <contact@trili.tech>
# Copyright (c) 2025 Functori <contact@functori.com>
#
# SPDX-License-Identifier: MIT

# Test: DAL node config.json is preserved during import
# Verifies: Attester profiles, RPC settings, and DAL-specific configuration

set -euo pipefail

SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
source "$SCRIPT_DIR/../lib.sh"

# Unique instance names for this test
NODE_INSTANCE="node-dal-test-$$"
DAL_INSTANCE="dal-dal-test-$$"
NODE_PORT=$(alloc_port)
NODE_RPC_PORT=$(alloc_port)
DAL_RPC_PORT=$(alloc_port)

# Register for cleanup
register_instance "$NODE_INSTANCE"
register_instance "$DAL_INSTANCE"

log "Creating external node (required for DAL)..."

# Create node data directory
NODE_DATA_DIR="/tmp/octez-node-dal-test-$$"
register_datadir "$NODE_DATA_DIR"
mkdir -p "$NODE_DATA_DIR"

# Initialize and start node
octez-node config init --data-dir="$NODE_DATA_DIR" \
	--network=weeklynet \
	--history-mode=rolling \
	--net-addr="127.0.0.1:$NODE_PORT" \
	--rpc-addr="127.0.0.1:$NODE_RPC_PORT" >/dev/null 2>&1

octez-node identity generate --data-dir="$NODE_DATA_DIR" >/dev/null 2>&1

# Install node via octez-manager
expect_success octez-manager install node "$NODE_INSTANCE" \
	--network weeklynet \
	--history-mode rolling \
	--rpc-addr "127.0.0.1:$NODE_RPC_PORT" \
	--net-addr "127.0.0.1:$NODE_PORT" \
	--data-dir "$NODE_DATA_DIR"

expect_success octez-manager start "$NODE_INSTANCE"
wait_for_node_rpc "$NODE_INSTANCE" 60

log "Creating external DAL node service with custom config..."

# Create DAL data directory
DAL_DATA_DIR="/tmp/octez-dal-external-test-$$"
register_datadir "$DAL_DATA_DIR"
mkdir -p "$DAL_DATA_DIR"

# Initialize DAL config
octez-dal-node config init --data-dir="$DAL_DATA_DIR" \
	--endpoint="http://127.0.0.1:$NODE_RPC_PORT" \
	--rpc-addr="127.0.0.1:$DAL_RPC_PORT" >/dev/null 2>&1

# Customize DAL config
CONFIG_FILE="$DAL_DATA_DIR/config.json"
jq '.rpc = {
  "listen-addr": "127.0.0.1:'"$DAL_RPC_PORT"'",
  "max_active_connections": 200
} | .public_addr = "dal.example.com:10732" | .metrics_addr = "127.0.0.1:11733"' \
	"$CONFIG_FILE" >"$CONFIG_FILE.tmp" && mv "$CONFIG_FILE.tmp" "$CONFIG_FILE"

log "Custom DAL config created with:"
log "  - RPC addr: 127.0.0.1:$DAL_RPC_PORT"
log "  - Max active connections: 200"
log "  - Public addr: dal.example.com:10732"
log "  - Metrics addr: 127.0.0.1:11733"

# Take hash of config before import
CONFIG_HASH_BEFORE=$(sha256sum "$CONFIG_FILE" | awk '{print $1}')
log "Config hash before import: $CONFIG_HASH_BEFORE"

# Create systemd service
SERVICE_NAME="octez-dal-node-${DAL_INSTANCE}"
SERVICE_FILE="/etc/systemd/system/${SERVICE_NAME}.service"

sudo tee "$SERVICE_FILE" >/dev/null <<EOF
[Unit]
Description=Octez DAL Node - ${DAL_INSTANCE}
After=network.target

[Service]
Type=simple
User=octez
ExecStart=/usr/bin/octez-dal-node run --data-dir=${DAL_DATA_DIR} --endpoint=http://127.0.0.1:${NODE_RPC_PORT}
Restart=on-failure
StandardOutput=journal
StandardError=journal

[Install]
WantedBy=multi-user.target
EOF

# Set ownership
sudo chown -R octez:octez "$DAL_DATA_DIR"

# Reload systemd
sudo systemctl daemon-reload

log "Importing external DAL service with takeover..."
expect_success octez-manager import detect
expect_success octez-manager import takeover "$SERVICE_NAME" "$DAL_INSTANCE"

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

# Verify specific DAL settings
log "Verifying DAL settings in config.json..."

# Check RPC listen address
RPC_ADDR=$(jq -r '.rpc["listen-addr"]' "$CONFIG_FILE")
if [ "$RPC_ADDR" != "127.0.0.1:$DAL_RPC_PORT" ]; then
	error "RPC listen address not preserved: $RPC_ADDR"
	exit 1
fi
log "✓ RPC listen address preserved"

# Check max active connections
MAX_CONN=$(jq -r '.rpc.max_active_connections' "$CONFIG_FILE")
if [ "$MAX_CONN" != "200" ]; then
	error "Max active connections not preserved: $MAX_CONN"
	exit 1
fi
log "✓ Max active connections preserved"

# Check public address
PUBLIC_ADDR=$(jq -r '.public_addr' "$CONFIG_FILE")
if [ "$PUBLIC_ADDR" != "dal.example.com:10732" ]; then
	error "Public address not preserved: $PUBLIC_ADDR"
	exit 1
fi
log "✓ Public address preserved"

# Check metrics address
METRICS_ADDR=$(jq -r '.metrics_addr' "$CONFIG_FILE")
if [ "$METRICS_ADDR" != "127.0.0.1:11733" ]; then
	error "Metrics address not preserved: $METRICS_ADDR"
	exit 1
fi
log "✓ Metrics address preserved"

log "Verifying managed DAL service can start with preserved config..."
expect_success octez-manager start "$DAL_INSTANCE"

# Wait for DAL to be responsive
sleep 5
if ! octez-manager status "$DAL_INSTANCE" | grep -q "running"; then
	error "DAL node failed to start"
	exit 1
fi

log "✓ DAL node started successfully with preserved config"

expect_success octez-manager stop "$DAL_INSTANCE"
expect_success octez-manager stop "$NODE_INSTANCE"

log "✓ Test passed: DAL config preserved during import"
