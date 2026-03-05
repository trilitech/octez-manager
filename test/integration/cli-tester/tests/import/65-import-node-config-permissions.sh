#!/usr/bin/env bash
# Copyright (c) 2025 Trilitech <contact@trili.tech>
# Copyright (c) 2025 Functori <contact@functori.com>
#
# SPDX-License-Identifier: MIT

# Test: Import preserves and corrects file ownership and permissions
# Verifies: Config files retain correct ownership after import

set -euo pipefail

SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
source "$SCRIPT_DIR/../lib.sh"

# Unique instance name for this test
INSTANCE="node-permissions-$$"
PORT=$(alloc_port)
RPC_PORT=$(alloc_port)

# Register for cleanup
register_instance "$INSTANCE"

log "Creating external node with specific ownership/permissions..."

# Create data directory
DATA_DIR="/tmp/octez-external-perms-$$"
register_datadir "$DATA_DIR"
mkdir -p "$DATA_DIR"

# Initialize node config and identity
octez-node config init --data-dir="$DATA_DIR" \
	--network=weeklynet \
	--history-mode=rolling \
	--net-addr="127.0.0.1:$PORT" \
	--rpc-addr="127.0.0.1:$RPC_PORT" >/dev/null 2>&1

octez-node identity generate --data-dir="$DATA_DIR" >/dev/null 2>&1

CONFIG_FILE="$DATA_DIR/config.json"
IDENTITY_FILE="$DATA_DIR/identity.json"

# Set ownership to octez user (as external service would have)
sudo chown -R octez:octez "$DATA_DIR"

# Set specific permissions
chmod 755 "$DATA_DIR"
chmod 644 "$CONFIG_FILE"
chmod 600 "$IDENTITY_FILE" # Identity should be more restrictive

log "Initial permissions set:"
log "  Directory: $(stat -c '%a' "$DATA_DIR")"
log "  config.json: $(stat -c '%a' "$CONFIG_FILE")"
log "  identity.json: $(stat -c '%a' "$IDENTITY_FILE")"

# Record initial ownership and permissions
INITIAL_DIR_PERMS=$(stat -c '%a' "$DATA_DIR")
INITIAL_CONFIG_PERMS=$(stat -c '%a' "$CONFIG_FILE")
INITIAL_IDENTITY_PERMS=$(stat -c '%a' "$IDENTITY_FILE")
INITIAL_DIR_OWNER=$(stat -c '%U:%G' "$DATA_DIR")
INITIAL_CONFIG_OWNER=$(stat -c '%U:%G' "$CONFIG_FILE")
INITIAL_IDENTITY_OWNER=$(stat -c '%U:%G' "$IDENTITY_FILE")

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

# Reload systemd
sudo systemctl daemon-reload

log "Importing external service with takeover..."
expect_success octez-manager import detect
expect_success octez-manager import takeover "$SERVICE_NAME" "$INSTANCE"

log "Verifying ownership after import..."

# Check ownership - should be octez:octez
CURRENT_DIR_OWNER=$(stat -c '%U:%G' "$DATA_DIR")
CURRENT_CONFIG_OWNER=$(stat -c '%U:%G' "$CONFIG_FILE")
CURRENT_IDENTITY_OWNER=$(stat -c '%U:%G' "$IDENTITY_FILE")

if [ "$CURRENT_DIR_OWNER" != "octez:octez" ]; then
	error "Directory ownership incorrect: $CURRENT_DIR_OWNER (expected octez:octez)"
	exit 1
fi
log "✓ Directory ownership correct: $CURRENT_DIR_OWNER"

if [ "$CURRENT_CONFIG_OWNER" != "octez:octez" ]; then
	error "config.json ownership incorrect: $CURRENT_CONFIG_OWNER (expected octez:octez)"
	exit 1
fi
log "✓ config.json ownership correct: $CURRENT_CONFIG_OWNER"

if [ "$CURRENT_IDENTITY_OWNER" != "octez:octez" ]; then
	error "identity.json ownership incorrect: $CURRENT_IDENTITY_OWNER (expected octez:octez)"
	exit 1
fi
log "✓ identity.json ownership correct: $CURRENT_IDENTITY_OWNER"

log "Verifying permissions after import..."

# Check permissions are reasonable
CURRENT_DIR_PERMS=$(stat -c '%a' "$DATA_DIR")
CURRENT_CONFIG_PERMS=$(stat -c '%a' "$CONFIG_FILE")
CURRENT_IDENTITY_PERMS=$(stat -c '%a' "$IDENTITY_FILE")

log "Current permissions:"
log "  Directory: $CURRENT_DIR_PERMS"
log "  config.json: $CURRENT_CONFIG_PERMS"
log "  identity.json: $CURRENT_IDENTITY_PERMS"

# Directory should be readable/executable by owner
if [ "${CURRENT_DIR_PERMS:0:1}" -lt "7" ]; then
	error "Directory permissions too restrictive: $CURRENT_DIR_PERMS"
	exit 1
fi
log "✓ Directory permissions allow owner access"

# Config should be readable by owner
if [ "${CURRENT_CONFIG_PERMS:0:1}" -lt "4" ]; then
	error "Config permissions too restrictive: $CURRENT_CONFIG_PERMS"
	exit 1
fi
log "✓ Config readable by owner"

# Identity should ideally be 600 or similar (owner only)
if [ "$CURRENT_IDENTITY_PERMS" != "600" ] && [ "$CURRENT_IDENTITY_PERMS" != "400" ]; then
	log "⚠ Warning: identity.json permissions not optimal: $CURRENT_IDENTITY_PERMS (expected 600)"
else
	log "✓ Identity has restrictive permissions: $CURRENT_IDENTITY_PERMS"
fi

log "Testing if service can start with current permissions..."
expect_success octez-manager start "$INSTANCE"

# Wait for node to be responsive
wait_for_node_rpc "$INSTANCE" 60

log "✓ Node started successfully"

# Verify files can still be read by the service
if ! sudo -u octez cat "$CONFIG_FILE" >/dev/null 2>&1; then
	error "Config not readable by octez user"
	exit 1
fi
log "✓ Config readable by octez user"

if ! sudo -u octez cat "$IDENTITY_FILE" >/dev/null 2>&1; then
	error "Identity not readable by octez user"
	exit 1
fi
log "✓ Identity readable by octez user"

expect_success octez-manager stop "$INSTANCE"

log "✓ Test passed: Ownership and permissions preserved/corrected appropriately"
