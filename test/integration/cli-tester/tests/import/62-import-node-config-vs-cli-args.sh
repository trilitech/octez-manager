#!/usr/bin/env bash
# Copyright (c) 2025 Trilitech <contact@trili.tech>
# Copyright (c) 2025 Functori <contact@functori.com>
#
# SPDX-License-Identifier: MIT

# Test: Import handles conflicts between config.json and ExecStart CLI args
# Verifies: CLI args in ExecStart take precedence, config.json preserved as-is

set -euo pipefail

SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
source "$SCRIPT_DIR/../lib.sh"

# Unique instance name for this test
INSTANCE="node-config-conflict-$$"
PORT=$(alloc_port)
RPC_PORT=$(alloc_port)
CONFLICT_RPC_PORT=$(alloc_port)

# Register for cleanup
register_instance "$INSTANCE"

log "Creating external node with conflicting config.json and CLI args..."

# Create data directory
DATA_DIR="/tmp/octez-external-conflict-$$"
register_datadir "$DATA_DIR"
mkdir -p "$DATA_DIR"

# Initialize node config and identity
octez-node config init --data-dir="$DATA_DIR" \
	--network=weeklynet \
	--history-mode=rolling \
	--net-addr="127.0.0.1:$PORT" \
	--rpc-addr="127.0.0.1:$CONFLICT_RPC_PORT" >/dev/null 2>&1

octez-node identity generate --data-dir="$DATA_DIR" >/dev/null 2>&1

# config.json has RPC on $CONFLICT_RPC_PORT
CONFIG_FILE="$DATA_DIR/config.json"
log "Config.json specifies RPC port: $CONFLICT_RPC_PORT"

# Take hash of config before import
CONFIG_HASH_BEFORE=$(sha256sum "$CONFIG_FILE" | awk '{print $1}')
log "Config hash before import: $CONFIG_HASH_BEFORE"

# Create systemd service with DIFFERENT RPC port in CLI args (should take precedence)
SERVICE_NAME="octez-node-${INSTANCE}"
SERVICE_FILE="/etc/systemd/system/${SERVICE_NAME}.service"

sudo tee "$SERVICE_FILE" >/dev/null <<EOF
[Unit]
Description=Octez Node - ${INSTANCE}
After=network.target

[Service]
Type=simple
User=octez
ExecStart=/usr/bin/octez-node run --data-dir=${DATA_DIR} --network=weeklynet --rpc-addr=127.0.0.1:${RPC_PORT}
Restart=on-failure
StandardOutput=journal
StandardError=journal

[Install]
WantedBy=multi-user.target
EOF

log "ExecStart specifies DIFFERENT RPC port: $RPC_PORT (conflict!)"

# Set ownership
sudo chown -R octez:octez "$DATA_DIR"

# Reload systemd
sudo systemctl daemon-reload

log "Importing external service with takeover..."
expect_success octez-manager import detect
expect_success octez-manager import takeover "$SERVICE_NAME" "$INSTANCE"

log "Verifying config.json preservation (despite conflict)..."

# Check hash is unchanged - config.json should be preserved as-is
CONFIG_HASH_AFTER=$(sha256sum "$CONFIG_FILE" | awk '{print $1}')
if [ "$CONFIG_HASH_BEFORE" != "$CONFIG_HASH_AFTER" ]; then
	error "Config file was modified during import!"
	error "Before: $CONFIG_HASH_BEFORE"
	error "After:  $CONFIG_HASH_AFTER"
	exit 1
fi

log "✓ Config hash unchanged (preserved as-is)"

# Verify config.json still has original RPC port
CONFIG_RPC_PORT=$(jq -r '.rpc["listen-addrs"][0]' "$CONFIG_FILE" | cut -d: -f2)
if [ "$CONFIG_RPC_PORT" != "$CONFLICT_RPC_PORT" ]; then
	error "Config RPC port was changed: $CONFIG_RPC_PORT"
	exit 1
fi
log "✓ Config.json still has original RPC port: $CONFLICT_RPC_PORT"

# Verify metadata has CLI arg RPC port (from ExecStart)
META=$(octez-manager info "$INSTANCE" --json)
META_RPC=$(echo "$META" | jq -r '.rpc_addr')

# The RPC addr from CLI args should be in metadata or extra_args
if ! echo "$META" | jq -e '.extra_args | contains(["--rpc-addr", "127.0.0.1:'"$RPC_PORT"'"])' >/dev/null; then
	# If not in extra_args, check rpc_addr field
	if [ "$META_RPC" != "127.0.0.1:$RPC_PORT" ]; then
		error "CLI RPC port not preserved in metadata"
		exit 1
	fi
fi
log "✓ CLI RPC port preserved in metadata: $RPC_PORT"

log "Verifying service starts with CLI args (not config.json)..."
expect_success octez-manager start "$INSTANCE"

# Wait for node to be responsive on CLI arg port
wait_for_node_rpc "$INSTANCE" 60

# Verify node is listening on CLI arg port, not config.json port
if curl -s "http://127.0.0.1:$RPC_PORT/chains/main/blocks/head/hash" >/dev/null 2>&1; then
	log "✓ Node responding on CLI arg port: $RPC_PORT"
else
	error "Node not responding on CLI arg port: $RPC_PORT"
	exit 1
fi

# Verify node is NOT listening on config.json port
if curl -s --connect-timeout 2 "http://127.0.0.1:$CONFLICT_RPC_PORT/chains/main/blocks/head/hash" >/dev/null 2>&1; then
	error "Node unexpectedly responding on config.json port: $CONFLICT_RPC_PORT"
	exit 1
fi
log "✓ Node correctly NOT listening on config.json port: $CONFLICT_RPC_PORT"

expect_success octez-manager stop "$INSTANCE"

log "✓ Test passed: CLI args take precedence, config.json preserved as-is"
