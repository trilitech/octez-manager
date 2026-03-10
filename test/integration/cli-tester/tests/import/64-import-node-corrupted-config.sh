#!/usr/bin/env bash
# Copyright (c) 2025 Trilitech <contact@trili.tech>
# Copyright (c) 2025 Functori <contact@functori.com>
#
# SPDX-License-Identifier: MIT

# Test: Import handles corrupted/invalid config.json gracefully
# Verifies: Import detects corrupted config and either fails gracefully or regenerates

set -euo pipefail

SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
source "$SCRIPT_DIR/../lib.sh"

# Unique instance name for this test
INSTANCE="node-corrupted-config-$$"
PORT=$(alloc_port)
RPC_PORT=$(alloc_port)

# Register for cleanup
register_instance "$INSTANCE"

log "Creating external node with corrupted config.json..."

# Create data directory
DATA_DIR="/tmp/octez-external-corrupted-$$"
register_data_dir "$DATA_DIR"
mkdir -p "$DATA_DIR"

# Initialize node config and identity
octez-node config init --data-dir="$DATA_DIR" \
	--network=weeklynet \
	--history-mode=rolling \
	--net-addr="127.0.0.1:$PORT" \
	--rpc-addr="127.0.0.1:$RPC_PORT" >/dev/null 2>&1

octez-node identity generate --data-dir="$DATA_DIR" >/dev/null 2>&1

# Corrupt config.json with invalid JSON
CONFIG_FILE="$DATA_DIR/config.json"
echo "{ this is not valid json" >"$CONFIG_FILE"

log "Corrupted config.json with invalid JSON"

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

log "Attempting import with corrupted config..."
expect_success octez-manager import detect

# Import should either:
# 1. Fail with clear error message about corrupted config
# 2. Succeed but warn about regenerating config
# We test for both scenarios

set +e
octez-manager import takeover "$SERVICE_NAME" "$INSTANCE" 2>&1 | tee /tmp/import-output-$$
IMPORT_EXIT=$?
set -e

if [ $IMPORT_EXIT -eq 0 ]; then
	log "Import succeeded - checking if config was regenerated..."

	# Check if config.json is now valid
	if ! jq empty "$CONFIG_FILE" >/dev/null 2>&1; then
		error "Import succeeded but config.json is still invalid"
		exit 1
	fi
	log "✓ Config regenerated as valid JSON"

	# Verify basic structure
	if ! jq -e '.p2p' "$CONFIG_FILE" >/dev/null 2>&1; then
		error "Regenerated config missing expected structure"
		exit 1
	fi
	log "✓ Regenerated config has expected structure"

	# Verify network preserved from ExecStart
	NETWORK=$(jq -r '.network // empty' "$CONFIG_FILE")
	if [ -n "$NETWORK" ] && [ "$NETWORK" != "weeklynet" ]; then
		error "Network not preserved in regenerated config: $NETWORK"
		exit 1
	fi
	log "✓ Network setting preserved"

	log "Testing if service can start with regenerated config..."
	expect_success octez-manager start "$INSTANCE"
	wait_for_node_rpc "$INSTANCE" 60
	log "✓ Service started successfully"
	expect_success octez-manager stop "$INSTANCE"

else
	log "Import failed (expected) - checking error message..."

	# Check that error message mentions config issue
	if grep -iq "config\|json\|parse\|invalid" /tmp/import-output-$$; then
		log "✓ Error message mentions config/JSON issue"
	else
		error "Error message doesn't clearly indicate config problem"
		cat /tmp/import-output-$$
		exit 1
	fi

	# Verify config.json was not modified
	CONTENT=$(cat "$CONFIG_FILE")
	if [ "$CONTENT" != "{ this is not valid json" ]; then
		error "Config was modified despite import failure"
		exit 1
	fi
	log "✓ Config left unchanged after failed import"

	log "✓ Import failed gracefully with clear error"
fi

rm -f /tmp/import-output-$$

log "✓ Test passed: Corrupted config handled appropriately"
