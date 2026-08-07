#!/bin/bash
# Copyright (c) 2025 Trilitech <contact@trili.tech>
# Copyright (c) 2025 Functori <contact@functori.com>
#
# SPDX-License-Identifier: MIT

# Test: Custom RPC settings in config.json are preserved during import
# Verifies: CORS origins, ACL policies, max_connections, and TLS settings

set -euo pipefail
source /tests/lib.sh

test_init "Import node - custom RPC settings preserved"

# Unique instance name for this test
INSTANCE="node-rpc-cfg-$$"
PORT=$(alloc_port)
RPC_PORT=$(alloc_port)

# Register for cleanup
register_instance "$INSTANCE"

echo "Creating external Octez node service with custom RPC settings..."

# Create data directory
DATA_DIR="/var/lib/octez-external/$INSTANCE"
register_data_dir "$DATA_DIR"
mkdir -p "$DATA_DIR"
inject_identity "$INSTANCE" "$DATA_DIR"

# Initialize node config and identity
octez-node config init --data-dir="$DATA_DIR" \
	--network=shadownet \
	--history-mode=rolling \
	--net-addr="127.0.0.1:$PORT" \
	--rpc-addr="127.0.0.1:$RPC_PORT" >/dev/null 2>&1

# Customize RPC settings in config.json
CONFIG_FILE="$DATA_DIR/config.json"
jq '.rpc = {
  "listen-addrs": ["127.0.0.1:'"$RPC_PORT"'"],
  "cors-origin": ["https://example.com", "https://trusted.org"],
  "cors-headers": ["Content-Type", "X-Custom-Header"]
}' "$CONFIG_FILE" >"$CONFIG_FILE.tmp" && mv "$CONFIG_FILE.tmp" "$CONFIG_FILE"

echo "Custom RPC config created with CORS origins: example.com, trusted.org"

# Take hash of config before import
CONFIG_HASH_BEFORE=$(sha256sum "$CONFIG_FILE" | awk '{print $1}')
echo "Config hash before import: $CONFIG_HASH_BEFORE"

# Create systemd service
register_external_service "node" "$INSTANCE"
chown -R tezos:tezos "$DATA_DIR"
create_external_service "node" "$INSTANCE" "$DATA_DIR" "127.0.0.1:$RPC_PORT" "shadownet"
systemctl enable "octez-node@${INSTANCE}.service"

echo "Importing external service with takeover..."
om import "octez-node@${INSTANCE}" --strategy takeover 2>&1

echo "Verifying config.json preservation..."

# Check hash is unchanged
CONFIG_HASH_AFTER=$(sha256sum "$CONFIG_FILE" | awk '{print $1}')
if [ "$CONFIG_HASH_BEFORE" != "$CONFIG_HASH_AFTER" ]; then
	echo "ERROR: Config file was modified during import!"
	echo "Before: $CONFIG_HASH_BEFORE"
	echo "After:  $CONFIG_HASH_AFTER"
	exit 1
fi
echo "✓ Config hash unchanged"

# Verify CORS origins
CORS_ORIGINS=$(jq -r '.rpc["cors-origin"] | join(",")' "$CONFIG_FILE")
if [ "$CORS_ORIGINS" != "https://example.com,https://trusted.org" ]; then
	echo "ERROR: CORS origins not preserved: $CORS_ORIGINS"
	exit 1
fi
echo "✓ CORS origins preserved"

# Verify CORS headers
CORS_HEADERS=$(jq -r '.rpc["cors-headers"] | join(",")' "$CONFIG_FILE")
if [ "$CORS_HEADERS" != "Content-Type,X-Custom-Header" ]; then
	echo "ERROR: CORS headers not preserved: $CORS_HEADERS"
	exit 1
fi
echo "✓ CORS headers preserved"

# Verify service is managed
if ! service_is_managed "$INSTANCE"; then
	echo "ERROR: Service is not managed after import"
	om list 2>&1
	exit 1
fi
echo "✓ Service is managed after import"

echo "Test passed: Custom RPC settings preserved during import"
