#!/bin/bash
# Test: Import node with config.json - verify config is preserved exactly
set -euo pipefail
source /tests/lib.sh

test_init "Import node - config.json preservation"

INSTANCE="config-preserve-node"
DATA_DIR="/var/lib/octez-external/$INSTANCE"
RPC_ADDR="127.0.0.1:$(alloc_port)"

register_instance "$INSTANCE"
register_external_service "node" "$INSTANCE"
register_data_dir "$DATA_DIR"

# Create external service with pre-generated identity
echo "Creating external systemd service..."
mkdir -p "$DATA_DIR"
inject_identity "$INSTANCE" "$DATA_DIR"
chown -R tezos:tezos "$DATA_DIR"

# Create initial config.json using octez-node
/usr/local/bin/octez-node config init \
	--data-dir "$DATA_DIR" \
	--network shadownet \
	--history-mode rolling 2>&1 || {
	echo "ERROR: Failed to create initial config"
	exit 1
}

# Customize config.json with additional settings
CONFIG_FILE="$DATA_DIR/config.json"
echo "Customizing config.json with extra settings..."

# Use jq to add custom RPC and P2P settings
jq '.rpc."cors-origins" = ["*"] |
    .rpc."cors-headers" = ["Content-Type"] |
    .p2p.limits."connection-timeout" = 15 |
    .p2p.limits."authentication-timeout" = 10 |
    .p2p.limits."max-download-speed" = 10485760 |
    .p2p.limits."max-upload-speed" = 10485760 |
    .log.level = "debug"' \
	"$CONFIG_FILE" >"$CONFIG_FILE.tmp" && mv "$CONFIG_FILE.tmp" "$CONFIG_FILE"

chown tezos:tezos "$CONFIG_FILE"

# Take hash of config.json before import
CONFIG_HASH_BEFORE=$(sha256sum "$CONFIG_FILE" | awk '{print $1}')
echo "Config hash before import: $CONFIG_HASH_BEFORE"

# Create external systemd service
create_external_service "node" "$INSTANCE" "$DATA_DIR" "$RPC_ADDR" "shadownet"
systemctl enable "octez-node@${INSTANCE}.service"
systemctl start "octez-node@${INSTANCE}.service"

# Wait for service to be detected
echo "Waiting for external service detection..."
if ! wait_for_external_service "$INSTANCE"; then
	echo "ERROR: Failed to detect external service"
	exit 1
fi

# Import with takeover strategy
echo "Importing with takeover strategy..."
om import "octez-node@${INSTANCE}" --strategy takeover 2>&1

# Stop the service immediately after import
systemctl stop "octez-node@${INSTANCE}.service" 2>/dev/null || true

# Verify service is managed
if ! service_is_managed "$INSTANCE"; then
	echo "ERROR: Service is not managed after import"
	om list 2>&1
	exit 1
fi

# CRITICAL: Verify config.json is preserved exactly
echo "Verifying config.json preservation..."

if [ ! -f "$CONFIG_FILE" ]; then
	echo "ERROR: config.json not found after import"
	exit 1
fi

CONFIG_HASH_AFTER=$(sha256sum "$CONFIG_FILE" | awk '{print $1}')
echo "Config hash after import: $CONFIG_HASH_AFTER"

if [ "$CONFIG_HASH_BEFORE" != "$CONFIG_HASH_AFTER" ]; then
	echo "ERROR: config.json was modified during import"
	echo "Expected hash: $CONFIG_HASH_BEFORE"
	echo "Actual hash:   $CONFIG_HASH_AFTER"
	echo "Config diff:"
	diff -u <(echo "$CONFIG_HASH_BEFORE") <(echo "$CONFIG_HASH_AFTER") || true
	exit 1
fi

echo "✓ Config hash matches - file unchanged"

# Verify specific custom settings are still present
echo "Verifying custom settings in config.json..."

if ! jq -e '.rpc."cors-origins" | contains(["*"])' "$CONFIG_FILE" >/dev/null; then
	echo "ERROR: Custom CORS origin setting missing"
	exit 1
fi
echo "✓ CORS origins preserved"

if ! jq -e '.p2p.limits."connection-timeout" == 15' "$CONFIG_FILE" >/dev/null; then
	echo "ERROR: Custom P2P connection-timeout missing"
	exit 1
fi
echo "✓ P2P connection-timeout preserved"

if ! jq -e '.log.level == "debug"' "$CONFIG_FILE" >/dev/null; then
	echo "ERROR: Custom log level missing"
	exit 1
fi
echo "✓ Log level preserved"

# Verify service can start with preserved config
echo "Verifying service starts with preserved config..."
if ! systemctl start "octez-node@${INSTANCE}.service"; then
	echo "ERROR: Service failed to start with preserved config"
	journalctl -u "octez-node@${INSTANCE}.service" -n 50 --no-pager || true
	exit 1
fi

sleep 2
systemctl stop "octez-node@${INSTANCE}.service" 2>/dev/null || true

echo "Config preservation test passed"
