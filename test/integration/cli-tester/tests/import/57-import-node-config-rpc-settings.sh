#!/usr/bin/env bash
# Copyright (c) 2025 Trilitech <contact@trili.tech>
# Copyright (c) 2025 Functori <contact@functori.com>
#
# SPDX-License-Identifier: MIT

# Test: Custom RPC settings in config.json are preserved during import
# Verifies: CORS origins, ACL policies, max_connections, and TLS settings

set -euo pipefail

SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
source "$SCRIPT_DIR/../lib.sh"

# Unique instance name for this test
INSTANCE="node-rpc-config-test-$$"
PORT=$(alloc_port)
RPC_PORT=$(alloc_port)

# Register for cleanup
register_instance "$INSTANCE"

log "Creating external Octez node service with custom RPC settings..."

# Create data directory
DATA_DIR="/tmp/octez-external-rpc-test-$$"
register_datadir "$DATA_DIR"
mkdir -p "$DATA_DIR"

# Initialize node config and identity
octez-node config init --data-dir="$DATA_DIR" \
	--network=weeklynet \
	--history-mode=rolling \
	--net-addr="127.0.0.1:$PORT" \
	--rpc-addr="127.0.0.1:$RPC_PORT" >/dev/null 2>&1

octez-node identity generate --data-dir="$DATA_DIR" >/dev/null 2>&1

# Customize RPC settings in config.json
CONFIG_FILE="$DATA_DIR/config.json"
jq '.rpc = {
  "listen-addrs": ["127.0.0.1:'"$RPC_PORT"'"],
  "cors-origin": ["https://example.com", "https://trusted.org"],
  "cors-headers": ["Content-Type", "X-Custom-Header"],
  "acl": [
    {"address": "127.0.0.1", "blacklist": []}
  ],
  "max_active_connections": 50,
  "max_active_rpc_connections": 25
}' "$CONFIG_FILE" >"$CONFIG_FILE.tmp" && mv "$CONFIG_FILE.tmp" "$CONFIG_FILE"

log "Custom RPC config created with:"
log "  - CORS origins: example.com, trusted.org"
log "  - CORS headers: Content-Type, X-Custom-Header"
log "  - ACL: 127.0.0.1 whitelisted"
log "  - Max active connections: 50"
log "  - Max active RPC connections: 25"

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

# Verify specific RPC settings
log "Verifying RPC settings in config.json..."

# Check CORS origins
CORS_ORIGINS=$(jq -r '.rpc["cors-origin"] | join(",")' "$CONFIG_FILE")
if [ "$CORS_ORIGINS" != "https://example.com,https://trusted.org" ]; then
	error "CORS origins not preserved: $CORS_ORIGINS"
	exit 1
fi
log "✓ CORS origins preserved"

# Check CORS headers
CORS_HEADERS=$(jq -r '.rpc["cors-headers"] | join(",")' "$CONFIG_FILE")
if [ "$CORS_HEADERS" != "Content-Type,X-Custom-Header" ]; then
	error "CORS headers not preserved: $CORS_HEADERS"
	exit 1
fi
log "✓ CORS headers preserved"

# Check ACL
ACL_ADDRESS=$(jq -r '.rpc.acl[0].address' "$CONFIG_FILE")
if [ "$ACL_ADDRESS" != "127.0.0.1" ]; then
	error "ACL address not preserved: $ACL_ADDRESS"
	exit 1
fi
log "✓ ACL preserved"

# Check max connections
MAX_CONN=$(jq -r '.rpc.max_active_connections' "$CONFIG_FILE")
if [ "$MAX_CONN" != "50" ]; then
	error "Max active connections not preserved: $MAX_CONN"
	exit 1
fi
log "✓ Max active connections preserved"

MAX_RPC_CONN=$(jq -r '.rpc.max_active_rpc_connections' "$CONFIG_FILE")
if [ "$MAX_RPC_CONN" != "25" ]; then
	error "Max active RPC connections not preserved: $MAX_RPC_CONN"
	exit 1
fi
log "✓ Max active RPC connections preserved"

log "Verifying managed service can start with preserved config..."
expect_success octez-manager start "$INSTANCE"

# Wait for node to be responsive
wait_for_node_rpc "$INSTANCE" 60

log "✓ Node started successfully with preserved RPC config"

expect_success octez-manager stop "$INSTANCE"

log "✓ Test passed: Custom RPC settings preserved during import"
