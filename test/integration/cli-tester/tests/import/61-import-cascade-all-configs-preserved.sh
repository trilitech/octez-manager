#!/usr/bin/env bash
# Copyright (c) 2025 Trilitech <contact@trili.tech>
# Copyright (c) 2025 Functori <contact@functori.com>
#
# SPDX-License-Identifier: MIT

# Test: Import preserves custom node config (CORS + private-mode)
# Verifies: Node config.json is not modified during import takeover

set -euo pipefail
source /tests/lib.sh

test_init "Import - node config preserved on import"

# Unique instance name for this test
NODE_INSTANCE="cfg-pres-node-$$"
NODE_PORT=$(alloc_port)
NODE_RPC_PORT=$(alloc_port)

# Register for cleanup
register_instance "$NODE_INSTANCE"

echo "Setting up external node with custom config..."

NODE_DATA_DIR="/var/lib/octez-external/$NODE_INSTANCE"
register_data_dir "$NODE_DATA_DIR"
mkdir -p "$NODE_DATA_DIR"

inject_identity "$NODE_INSTANCE" "$NODE_DATA_DIR"

octez-node config init --data-dir="$NODE_DATA_DIR" \
	--network=shadownet \
	--history-mode=rolling \
	--net-addr="127.0.0.1:$NODE_PORT" \
	--rpc-addr="127.0.0.1:$NODE_RPC_PORT" >/dev/null 2>&1

# Customize node config
NODE_CONFIG="$NODE_DATA_DIR/config.json"
jq '.rpc."cors-origin" = ["https://cascade-test.com"] | .p2p."private-mode" = true' \
	"$NODE_CONFIG" >"$NODE_CONFIG.tmp" && mv "$NODE_CONFIG.tmp" "$NODE_CONFIG"

NODE_CONFIG_HASH=$(sha256sum "$NODE_CONFIG" | awk '{print $1}')
echo "Node config hash: $NODE_CONFIG_HASH"

chown -R tezos:tezos "$NODE_DATA_DIR"
register_external_service "node" "$NODE_INSTANCE"
create_external_service "node" "$NODE_INSTANCE" "$NODE_DATA_DIR" "127.0.0.1:$NODE_RPC_PORT" "shadownet"
systemctl enable "octez-node@${NODE_INSTANCE}.service"

# Do NOT start the node — we just need the service file to exist for import detection.
# Import does not require the service to be actively running.

echo "Performing import of node service (--no-start via clone strategy)..."
# Use clone so we don't stop/start the external service (it's not running).
# Provide --network since the node isn't running so RPC can't be queried.
om import "octez-node@${NODE_INSTANCE}" --strategy clone --network shadownet 2>&1 || {
	echo "ERROR: Import command failed"
	om list 2>&1
	exit 1
}

echo "Verifying node config was preserved..."

# Check node config hash unchanged
NODE_CONFIG_HASH_AFTER=$(sha256sum "$NODE_CONFIG" | awk '{print $1}')
if [ "$NODE_CONFIG_HASH" != "$NODE_CONFIG_HASH_AFTER" ]; then
	echo "ERROR: Node config was modified by import!"
	echo "Before: $NODE_CONFIG_HASH"
	echo "After:  $NODE_CONFIG_HASH_AFTER"
	exit 1
fi
echo "✓ Node config preserved"

# Verify node config contents
if ! jq -e '.rpc."cors-origin"[0] == "https://cascade-test.com"' "$NODE_CONFIG" >/dev/null; then
	echo "ERROR: Node CORS origin not preserved"
	jq '.rpc."cors-origin"' "$NODE_CONFIG"
	exit 1
fi
echo "✓ Node CORS setting preserved"

if ! jq -e '.p2p."private-mode" == true' "$NODE_CONFIG" >/dev/null; then
	echo "ERROR: Node private mode not preserved"
	jq '.p2p."private-mode"' "$NODE_CONFIG"
	exit 1
fi
echo "✓ Node private mode preserved"

# Verify node service is managed
if ! service_is_managed "$NODE_INSTANCE"; then
	echo "ERROR: Node not managed after import"
	om list 2>&1
	exit 1
fi
echo "✓ Node is managed"

echo "Test passed: All configs preserved during import"
