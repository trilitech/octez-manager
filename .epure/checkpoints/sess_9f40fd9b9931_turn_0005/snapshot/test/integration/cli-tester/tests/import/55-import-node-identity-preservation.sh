#!/bin/bash
# Test: Import node - verify identity.json (peer ID) is preserved
set -euo pipefail
source /tests/lib.sh

test_init "Import node - identity.json preservation"

INSTANCE="identity-preserve-node"
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

# Create config.json
/usr/local/bin/octez-node config init \
	--data-dir "$DATA_DIR" \
	--network shadownet \
	--history-mode rolling 2>&1

IDENTITY_FILE="$DATA_DIR/identity.json"

# Verify identity exists
if [ ! -f "$IDENTITY_FILE" ]; then
	echo "ERROR: identity.json not found"
	exit 1
fi

# Record peer ID and hash before import
PEER_ID_BEFORE=$(jq -r '.peer_id' "$IDENTITY_FILE")
IDENTITY_HASH_BEFORE=$(sha256sum "$IDENTITY_FILE" | awk '{print $1}')

echo "Peer ID before import: $PEER_ID_BEFORE"
echo "Identity hash before import: $IDENTITY_HASH_BEFORE"

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

# Stop service immediately
systemctl stop "octez-node@${INSTANCE}.service" 2>/dev/null || true

# Verify service is managed
if ! service_is_managed "$INSTANCE"; then
	echo "ERROR: Service is not managed after import"
	exit 1
fi

# CRITICAL: Verify identity.json is preserved exactly
echo "Verifying identity.json preservation..."

if [ ! -f "$IDENTITY_FILE" ]; then
	echo "ERROR: identity.json not found after import"
	exit 1
fi

IDENTITY_HASH_AFTER=$(sha256sum "$IDENTITY_FILE" | awk '{print $1}')
echo "Identity hash after import: $IDENTITY_HASH_AFTER"

if [ "$IDENTITY_HASH_BEFORE" != "$IDENTITY_HASH_AFTER" ]; then
	echo "ERROR: identity.json was modified during import"
	echo "Expected hash: $IDENTITY_HASH_BEFORE"
	echo "Actual hash:   $IDENTITY_HASH_AFTER"
	exit 1
fi

echo "✓ Identity hash matches - file unchanged"

# Verify peer ID is the same
PEER_ID_AFTER=$(jq -r '.peer_id' "$IDENTITY_FILE")
echo "Peer ID after import: $PEER_ID_AFTER"

if [ "$PEER_ID_BEFORE" != "$PEER_ID_AFTER" ]; then
	echo "ERROR: Peer ID changed during import"
	echo "Expected: $PEER_ID_BEFORE"
	echo "Actual:   $PEER_ID_AFTER"
	exit 1
fi

echo "✓ Peer ID preserved: $PEER_ID_AFTER"

# Verify node can start with preserved identity
echo "Verifying service starts with preserved identity..."
if ! systemctl start "octez-node@${INSTANCE}.service"; then
	echo "ERROR: Service failed to start with preserved identity"
	journalctl -u "octez-node@${INSTANCE}.service" -n 50 --no-pager || true
	exit 1
fi

sleep 2
systemctl stop "octez-node@${INSTANCE}.service" 2>/dev/null || true

echo "Identity preservation test passed"
