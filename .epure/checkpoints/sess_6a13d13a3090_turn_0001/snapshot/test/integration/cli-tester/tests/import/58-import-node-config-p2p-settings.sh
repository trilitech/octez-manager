#!/usr/bin/env bash
# Copyright (c) 2025 Trilitech <contact@trili.tech>
# Copyright (c) 2025 Functori <contact@functori.com>
#
# SPDX-License-Identifier: MIT

# Test: Custom P2P settings in config.json are preserved during import
# Verifies: Bootstrap peers, connection limits, private mode, and discovery settings

set -euo pipefail
source /tests/lib.sh

test_init "Import node - custom P2P settings preserved"

# Unique instance name for this test
INSTANCE="node-p2p-cfg-$$"
PORT=$(alloc_port)
RPC_PORT=$(alloc_port)

# Register for cleanup
register_instance "$INSTANCE"

echo "Creating external Octez node service with custom P2P settings..."

# Create data directory
DATA_DIR="/var/lib/octez-external/$INSTANCE"
register_data_dir "$DATA_DIR"
mkdir -p "$DATA_DIR"
inject_identity "$INSTANCE" "$DATA_DIR"

# Initialize node config
octez-node config init --data-dir="$DATA_DIR" \
	--network=shadownet \
	--history-mode=rolling \
	--net-addr="127.0.0.1:$PORT" \
	--rpc-addr="127.0.0.1:$RPC_PORT" >/dev/null 2>&1

# Customize P2P settings in config.json
CONFIG_FILE="$DATA_DIR/config.json"
jq '.p2p."private-mode" = true |
    .p2p."disable-peer-discovery" = true' \
	"$CONFIG_FILE" >"$CONFIG_FILE.tmp" && mv "$CONFIG_FILE.tmp" "$CONFIG_FILE"

echo "Custom P2P config created with private-mode and peer-discovery disabled"

# Take hash of config before import
CONFIG_HASH_BEFORE=$(sha256sum "$CONFIG_FILE" | awk '{print $1}')
echo "Config hash before import: $CONFIG_HASH_BEFORE"

# Create systemd service
register_external_service "node" "$INSTANCE"
chown -R tezos:tezos "$DATA_DIR"
create_external_service "node" "$INSTANCE" "$DATA_DIR" "127.0.0.1:$RPC_PORT" "shadownet"
systemctl enable "octez-node@${INSTANCE}.service"

echo "Importing external service with clone strategy..."
om import "octez-node@${INSTANCE}" --strategy clone --network shadownet 2>&1

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

# Verify specific P2P settings
PRIVATE_MODE=$(jq -r '.p2p."private-mode"' "$CONFIG_FILE")
if [ "$PRIVATE_MODE" != "true" ]; then
	echo "ERROR: Private mode not preserved: $PRIVATE_MODE"
	exit 1
fi
echo "✓ Private mode preserved"

PEER_DISCOVERY=$(jq -r '.p2p."disable-peer-discovery"' "$CONFIG_FILE")
if [ "$PEER_DISCOVERY" != "true" ]; then
	echo "ERROR: Peer discovery setting not preserved: $PEER_DISCOVERY"
	exit 1
fi
echo "✓ Peer discovery setting preserved"

# Verify service is managed
if ! service_is_managed "$INSTANCE"; then
	echo "ERROR: Service is not managed after import"
	om list 2>&1
	exit 1
fi
echo "✓ Service is managed after import"

echo "Test passed: Custom P2P settings preserved during import"
