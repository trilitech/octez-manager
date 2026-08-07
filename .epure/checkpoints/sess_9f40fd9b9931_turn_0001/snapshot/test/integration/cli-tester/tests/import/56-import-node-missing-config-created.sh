#!/bin/bash
# Test: Import node with missing config.json - verify config is created
set -euo pipefail
source /tests/lib.sh

test_init "Import node - missing config.json created"

INSTANCE="missing-config-node"
DATA_DIR="/var/lib/octez-external/$INSTANCE"
RPC_ADDR="127.0.0.1:$(alloc_port)"

register_instance "$INSTANCE"
register_external_service "node" "$INSTANCE"
register_data_dir "$DATA_DIR"

# Create external service with identity but WITHOUT config.json (edge case)
echo "Creating external systemd service without config.json..."
mkdir -p "$DATA_DIR"
inject_identity "$INSTANCE" "$DATA_DIR"

# Create a minimal store directory to make it look like a node was running
mkdir -p "$DATA_DIR/store"
mkdir -p "$DATA_DIR/context"

chown -R tezos:tezos "$DATA_DIR"

CONFIG_FILE="$DATA_DIR/config.json"

# Verify config.json does NOT exist before import
if [ -f "$CONFIG_FILE" ]; then
	echo "ERROR: config.json should not exist yet"
	exit 1
fi
echo "✓ Confirmed config.json does not exist"

# Create external systemd service
create_external_service "node" "$INSTANCE" "$DATA_DIR" "$RPC_ADDR" "shadownet"
systemctl enable "octez-node@${INSTANCE}.service"

# Note: Don't start service since it would fail without config.json
# Just make it detectable via systemd

# Import with takeover strategy
echo "Importing with takeover strategy (should create missing config)..."
om import "octez-node@${INSTANCE}" --strategy takeover 2>&1 || {
	echo "Import may have failed - checking if config was created anyway..."
}

# Stop service if it started
systemctl stop "octez-node@${INSTANCE}.service" 2>/dev/null || true

# CRITICAL: Verify config.json was created by import
echo "Verifying config.json was created..."

if [ ! -f "$CONFIG_FILE" ]; then
	echo "ERROR: config.json was not created by import"
	echo "Data dir contents:"
	ls -la "$DATA_DIR" || true
	exit 1
fi

echo "✓ config.json created by import"

# Verify config has network field (value may be short name or full URL)
NETWORK=$(jq -r '.network' "$CONFIG_FILE")
if [ -z "$NETWORK" ] || [ "$NETWORK" = "null" ]; then
	echo "ERROR: config.json is missing network field"
	cat "$CONFIG_FILE"
	exit 1
fi
# Accept both short names and full URLs (e.g. "shadownet" or "https://teztnets.com/shadownet")
if echo "$NETWORK" | grep -qi "shadownet"; then
	echo "✓ config.json has correct network: $NETWORK"
else
	echo "ERROR: config.json has unexpected network: $NETWORK (expected shadownet)"
	cat "$CONFIG_FILE"
	exit 1
fi

# Verify config has history-mode (should default to rolling)
HISTORY_MODE=$(jq -r '.shell."history_mode"' "$CONFIG_FILE")
if [ "$HISTORY_MODE" = "null" ] || [ -z "$HISTORY_MODE" ]; then
	echo "WARNING: history_mode not set in config, checking alternate location..."
	# Try alternate JSON path
	HISTORY_MODE=$(jq -r '."history-mode"' "$CONFIG_FILE")
fi

if [ "$HISTORY_MODE" != "null" ] && [ -n "$HISTORY_MODE" ]; then
	echo "✓ config.json has history mode: $HISTORY_MODE"
else
	echo "NOTE: history-mode not explicitly in config (may use default)"
fi

# Verify service is managed
if ! service_is_managed "$INSTANCE"; then
	echo "ERROR: Service is not managed after import"
	om list 2>&1
	exit 1
fi

# Verify service can start with created config
echo "Verifying service starts with created config..."
if ! systemctl start "octez-node@${INSTANCE}.service"; then
	echo "ERROR: Service failed to start with created config"
	journalctl -u "octez-node@${INSTANCE}.service" -n 50 --no-pager || true
	exit 1
fi

sleep 2
systemctl stop "octez-node@${INSTANCE}.service" 2>/dev/null || true

echo "Missing config creation test passed"
