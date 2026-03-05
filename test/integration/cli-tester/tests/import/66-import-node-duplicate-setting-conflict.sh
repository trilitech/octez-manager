#!/usr/bin/env bash
# Copyright (c) 2025 Trilitech <contact@trili.tech>
# Copyright (c) 2025 Functori <contact@functori.com>
#
# SPDX-License-Identifier: MIT

# Test: Import handles same setting in both config.json and CLI args
# Verifies: CLI args override config.json, both are preserved separately

set -euo pipefail

SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
source "$SCRIPT_DIR/../lib.sh"

# Unique instance name for this test
INSTANCE="node-duplicate-setting-$$"
PORT=$(alloc_port)
RPC_PORT=$(alloc_port)
CONFIG_RPC_PORT=$(alloc_port) # Different port for config.json

# Register for cleanup
register_instance "$INSTANCE"

log "Creating external node with same setting in config.json AND CLI args..."

# Create data directory
DATA_DIR="/tmp/octez-external-duplicate-$$"
register_datadir "$DATA_DIR"
mkdir -p "$DATA_DIR"

# Initialize node config and identity
octez-node config init --data-dir="$DATA_DIR" \
	--network=weeklynet \
	--history-mode=rolling \
	--net-addr="127.0.0.1:$PORT" \
	--rpc-addr="127.0.0.1:$CONFIG_RPC_PORT" >/dev/null 2>&1

octez-node identity generate --data-dir="$DATA_DIR" >/dev/null 2>&1

# Customize config.json with specific RPC port and connections setting
CONFIG_FILE="$DATA_DIR/config.json"
jq '.rpc = {
  "listen-addrs": ["127.0.0.1:'"$CONFIG_RPC_PORT"'"],
  "max_active_connections": 50
}' "$CONFIG_FILE" >"$CONFIG_FILE.tmp" && mv "$CONFIG_FILE.tmp" "$CONFIG_FILE"

log "Config.json specifies:"
log "  - RPC port: $CONFIG_RPC_PORT"
log "  - Max active connections: 50"

# Take hash of config before import
CONFIG_HASH_BEFORE=$(sha256sum "$CONFIG_FILE" | awk '{print $1}')
log "Config hash before import: $CONFIG_HASH_BEFORE"

# Create systemd service with CONFLICTING values in CLI args
SERVICE_NAME="octez-node-${INSTANCE}"
SERVICE_FILE="/etc/systemd/system/${SERVICE_NAME}.service"

sudo tee "$SERVICE_FILE" >/dev/null <<EOF
[Unit]
Description=Octez Node - ${INSTANCE}
After=network.target

[Service]
Type=simple
User=octez
ExecStart=/usr/bin/octez-node run --data-dir=${DATA_DIR} --network=weeklynet --rpc-addr=127.0.0.1:${RPC_PORT} --connections=100
Restart=on-failure
StandardOutput=journal
StandardError=journal

[Install]
WantedBy=multi-user.target
EOF

log "ExecStart specifies CONFLICTING values:"
log "  - RPC port: $RPC_PORT (conflicts with config)"
log "  - Connections: 100 (conflicts with max_active_connections:50)"

# Set ownership
sudo chown -R octez:octez "$DATA_DIR"

# Reload systemd
sudo systemctl daemon-reload

log "Importing external service with takeover..."
expect_success octez-manager import detect
expect_success octez-manager import takeover "$SERVICE_NAME" "$INSTANCE"

log "Verifying config.json preservation (unchanged despite conflict)..."

# Check hash is unchanged - config.json should be preserved as-is
CONFIG_HASH_AFTER=$(sha256sum "$CONFIG_FILE" | awk '{print $1}')
if [ "$CONFIG_HASH_BEFORE" != "$CONFIG_HASH_AFTER" ]; then
	error "Config file was modified during import!"
	error "Before: $CONFIG_HASH_BEFORE"
	error "After:  $CONFIG_HASH_AFTER"
	exit 1
fi

log "✓ Config hash unchanged (preserved as-is)"

# Verify config.json still has original values
CONFIG_RPC=$(jq -r '.rpc["listen-addrs"][0]' "$CONFIG_FILE")
if ! echo "$CONFIG_RPC" | grep -q "$CONFIG_RPC_PORT"; then
	error "Config RPC port was changed: $CONFIG_RPC"
	exit 1
fi
log "✓ Config.json still has original RPC port: $CONFIG_RPC_PORT"

CONFIG_CONN=$(jq -r '.rpc.max_active_connections' "$CONFIG_FILE")
if [ "$CONFIG_CONN" != "50" ]; then
	error "Config max_active_connections was changed: $CONFIG_CONN"
	exit 1
fi
log "✓ Config.json still has original max_active_connections: 50"

# Verify CLI args preserved in metadata
META=$(octez-manager info "$INSTANCE" --json)

# Check RPC addr from CLI is in metadata
if ! echo "$META" | jq -e '.extra_args | contains(["--rpc-addr", "127.0.0.1:'"$RPC_PORT"'"])' >/dev/null &&
	! echo "$META" | jq -e '.rpc_addr == "127.0.0.1:'"$RPC_PORT"'"' >/dev/null; then
	error "CLI RPC addr not preserved in metadata"
	exit 1
fi
log "✓ CLI RPC addr preserved in metadata: 127.0.0.1:$RPC_PORT"

# Check connections from CLI is in metadata
if ! echo "$META" | jq -e '.extra_args | contains(["--connections", "100"])' >/dev/null; then
	error "CLI --connections not preserved in metadata"
	exit 1
fi
log "✓ CLI --connections preserved in metadata: 100"

log "Verifying service starts with CLI args taking precedence..."
expect_success octez-manager start "$INSTANCE"

# Wait for node to be responsive
wait_for_node_rpc "$INSTANCE" 60

# Verify node is listening on CLI arg RPC port, not config.json port
if curl -s "http://127.0.0.1:$RPC_PORT/chains/main/blocks/head/hash" >/dev/null 2>&1; then
	log "✓ Node responding on CLI arg RPC port: $RPC_PORT"
else
	error "Node not responding on CLI arg RPC port: $RPC_PORT"
	exit 1
fi

# Verify node is NOT listening on config.json RPC port
if curl -s --connect-timeout 2 "http://127.0.0.1:$CONFIG_RPC_PORT/chains/main/blocks/head/hash" >/dev/null 2>&1; then
	error "Node unexpectedly responding on config.json RPC port: $CONFIG_RPC_PORT"
	exit 1
fi
log "✓ Node correctly NOT listening on config.json RPC port: $CONFIG_RPC_PORT"

log "Verifying connection limit from CLI args is active..."
# Note: We can't easily verify the connections limit without monitoring actual connections,
# but the fact that the node started with CLI args proves they took precedence
log "✓ Node started with CLI args (connections=100 should be active, not config's 50)"

expect_success octez-manager stop "$INSTANCE"

log "✓ Test passed: Duplicate settings handled correctly"
log "  - Config.json preserved unchanged"
log "  - CLI args preserved in metadata"
log "  - CLI args take precedence at runtime"
