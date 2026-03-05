#!/usr/bin/env bash
# Copyright (c) 2025 Trilitech <contact@trili.tech>
# Copyright (c) 2025 Functori <contact@functori.com>
#
# SPDX-License-Identifier: MIT

# Test: Custom P2P settings in config.json are preserved during import
# Verifies: Bootstrap peers, connection limits, private mode, and discovery settings

set -euo pipefail

SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
source "$SCRIPT_DIR/../lib.sh"

# Unique instance name for this test
INSTANCE="node-p2p-config-test-$$"
PORT=$(alloc_port)
RPC_PORT=$(alloc_port)

# Register for cleanup
register_instance "$INSTANCE"

log "Creating external Octez node service with custom P2P settings..."

# Create data directory
DATA_DIR="/tmp/octez-external-p2p-test-$$"
register_datadir "$DATA_DIR"
mkdir -p "$DATA_DIR"

# Initialize node config and identity
octez-node config init --data-dir="$DATA_DIR" \
	--network=weeklynet \
	--history-mode=rolling \
	--net-addr="127.0.0.1:$PORT" \
	--rpc-addr="127.0.0.1:$RPC_PORT" >/dev/null 2>&1

octez-node identity generate --data-dir="$DATA_DIR" >/dev/null 2>&1

# Customize P2P settings in config.json
CONFIG_FILE="$DATA_DIR/config.json"
jq '.p2p = {
  "bootstrap-peers": [
    "boot.tzbeta.net:9732",
    "bootalpha.tzbeta.net:9732"
  ],
  "listen-addr": "127.0.0.1:'"$PORT"'",
  "private-mode": true,
  "limits": {
    "connection-timeout": "20",
    "authentication-timeout": "10",
    "greylist-timeout": "120",
    "maintenance-idle-time": "300",
    "min-connections": "10",
    "expected-connections": "50",
    "max-connections": "100",
    "max-incoming-connections": "50"
  },
  "disable-peer-discovery": true
}' "$CONFIG_FILE" >"$CONFIG_FILE.tmp" && mv "$CONFIG_FILE.tmp" "$CONFIG_FILE"

log "Custom P2P config created with:"
log "  - Bootstrap peers: boot.tzbeta.net, bootalpha.tzbeta.net"
log "  - Private mode: enabled"
log "  - Min/Expected/Max connections: 10/50/100"
log "  - Max incoming connections: 50"
log "  - Peer discovery: disabled"

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

# Verify specific P2P settings
log "Verifying P2P settings in config.json..."

# Check bootstrap peers
BOOTSTRAP_PEERS=$(jq -r '.p2p["bootstrap-peers"] | join(",")' "$CONFIG_FILE")
if [ "$BOOTSTRAP_PEERS" != "boot.tzbeta.net:9732,bootalpha.tzbeta.net:9732" ]; then
	error "Bootstrap peers not preserved: $BOOTSTRAP_PEERS"
	exit 1
fi
log "✓ Bootstrap peers preserved"

# Check private mode
PRIVATE_MODE=$(jq -r '.p2p["private-mode"]' "$CONFIG_FILE")
if [ "$PRIVATE_MODE" != "true" ]; then
	error "Private mode not preserved: $PRIVATE_MODE"
	exit 1
fi
log "✓ Private mode preserved"

# Check peer discovery disabled
PEER_DISCOVERY=$(jq -r '.p2p["disable-peer-discovery"]' "$CONFIG_FILE")
if [ "$PEER_DISCOVERY" != "true" ]; then
	error "Peer discovery setting not preserved: $PEER_DISCOVERY"
	exit 1
fi
log "✓ Peer discovery setting preserved"

# Check connection limits
MIN_CONN=$(jq -r '.p2p.limits["min-connections"]' "$CONFIG_FILE")
if [ "$MIN_CONN" != "10" ]; then
	error "Min connections not preserved: $MIN_CONN"
	exit 1
fi
log "✓ Min connections preserved"

EXPECTED_CONN=$(jq -r '.p2p.limits["expected-connections"]' "$CONFIG_FILE")
if [ "$EXPECTED_CONN" != "50" ]; then
	error "Expected connections not preserved: $EXPECTED_CONN"
	exit 1
fi
log "✓ Expected connections preserved"

MAX_CONN=$(jq -r '.p2p.limits["max-connections"]' "$CONFIG_FILE")
if [ "$MAX_CONN" != "100" ]; then
	error "Max connections not preserved: $MAX_CONN"
	exit 1
fi
log "✓ Max connections preserved"

MAX_INCOMING=$(jq -r '.p2p.limits["max-incoming-connections"]' "$CONFIG_FILE")
if [ "$MAX_INCOMING" != "50" ]; then
	error "Max incoming connections not preserved: $MAX_INCOMING"
	exit 1
fi
log "✓ Max incoming connections preserved"

# Check timeouts
CONN_TIMEOUT=$(jq -r '.p2p.limits["connection-timeout"]' "$CONFIG_FILE")
if [ "$CONN_TIMEOUT" != "20" ]; then
	error "Connection timeout not preserved: $CONN_TIMEOUT"
	exit 1
fi
log "✓ Connection timeout preserved"

log "Verifying managed service can start with preserved config..."
expect_success octez-manager start "$INSTANCE"

# Wait for node to be responsive
wait_for_node_rpc "$INSTANCE" 60

log "✓ Node started successfully with preserved P2P config"

expect_success octez-manager stop "$INSTANCE"

log "✓ Test passed: Custom P2P settings preserved during import"
