#!/usr/bin/env bash
# Copyright (c) 2025 Trilitech <contact@trili.tech>
# Copyright (c) 2025 Functori <contact@functori.com>
#
# SPDX-License-Identifier: MIT

# Test: Import preserves both config.json and non-conflicting extra_args
# Verifies: Config settings and CLI args work together when non-conflicting

set -euo pipefail

SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
source "$SCRIPT_DIR/../lib.sh"

# Unique instance name for this test
INSTANCE="node-config-with-args-$$"
PORT=$(alloc_port)
RPC_PORT=$(alloc_port)

# Register for cleanup
register_instance "$INSTANCE"

log "Creating external node with config.json AND non-conflicting CLI args..."

# Create data directory
DATA_DIR="/tmp/octez-external-both-$$"
register_datadir "$DATA_DIR"
mkdir -p "$DATA_DIR"

# Initialize node config and identity
octez-node config init --data-dir="$DATA_DIR" \
	--network=weeklynet \
	--history-mode=rolling \
	--net-addr="127.0.0.1:$PORT" \
	--rpc-addr="127.0.0.1:$RPC_PORT" >/dev/null 2>&1

octez-node identity generate --data-dir="$DATA_DIR" >/dev/null 2>&1

# Customize config.json with RPC and P2P settings
CONFIG_FILE="$DATA_DIR/config.json"
jq '.rpc."cors-origin" = ["https://example.com"] | 
    .rpc."max_active_connections" = 100 |
    .p2p."private-mode" = true' \
	"$CONFIG_FILE" >"$CONFIG_FILE.tmp" && mv "$CONFIG_FILE.tmp" "$CONFIG_FILE"

log "Config.json has:"
log "  - CORS origin: https://example.com"
log "  - Max active connections: 100"
log "  - Private mode: true"

# Take hash of config before import
CONFIG_HASH_BEFORE=$(sha256sum "$CONFIG_FILE" | awk '{print $1}')
log "Config hash before import: $CONFIG_HASH_BEFORE"

# Create systemd service with NON-CONFLICTING CLI args
# These args don't override config.json settings
SERVICE_NAME="octez-node-${INSTANCE}"
SERVICE_FILE="/etc/systemd/system/${SERVICE_NAME}.service"

sudo tee "$SERVICE_FILE" >/dev/null <<EOF
[Unit]
Description=Octez Node - ${INSTANCE}
After=network.target

[Service]
Type=simple
User=octez
ExecStart=/usr/bin/octez-node run --data-dir=${DATA_DIR} --network=weeklynet --log-output=file --connections=50
Restart=on-failure
StandardOutput=journal
StandardError=journal

[Install]
WantedBy=multi-user.target
EOF

log "ExecStart has NON-CONFLICTING args:"
log "  - --log-output=file"
log "  - --connections=50"

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

# Verify config.json settings preserved
CORS_ORIGIN=$(jq -r '.rpc."cors-origin"[0]' "$CONFIG_FILE")
if [ "$CORS_ORIGIN" != "https://example.com" ]; then
	error "CORS origin not preserved: $CORS_ORIGIN"
	exit 1
fi
log "✓ CORS origin preserved in config.json"

MAX_CONN=$(jq -r '.rpc.max_active_connections' "$CONFIG_FILE")
if [ "$MAX_CONN" != "100" ]; then
	error "Max connections not preserved: $MAX_CONN"
	exit 1
fi
log "✓ Max connections preserved in config.json"

PRIVATE_MODE=$(jq -r '.p2p."private-mode"' "$CONFIG_FILE")
if [ "$PRIVATE_MODE" != "true" ]; then
	error "Private mode not preserved: $PRIVATE_MODE"
	exit 1
fi
log "✓ Private mode preserved in config.json"

# Verify extra_args from CLI preserved in metadata
META=$(octez-manager info "$INSTANCE" --json)

if ! echo "$META" | jq -e '.extra_args | contains(["--log-output", "file"])' >/dev/null; then
	error "CLI arg --log-output not preserved"
	exit 1
fi
log "✓ CLI arg --log-output preserved"

if ! echo "$META" | jq -e '.extra_args | contains(["--connections", "50"])' >/dev/null; then
	error "CLI arg --connections not preserved"
	exit 1
fi
log "✓ CLI arg --connections preserved"

log "Verifying service starts with BOTH config.json and extra_args..."
expect_success octez-manager start "$INSTANCE"

# Wait for node to be responsive
wait_for_node_rpc "$INSTANCE" 60

log "✓ Node started successfully with both config and args"

# Check logs to verify --log-output=file is active
sleep 2
if [ -f "$DATA_DIR/node.log" ]; then
	log "✓ Log file created (--log-output=file is active)"
else
	log "⚠ Log file not found, but node is running (may be using journal)"
fi

expect_success octez-manager stop "$INSTANCE"

log "✓ Test passed: Config and non-conflicting args both preserved"
