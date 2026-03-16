#!/usr/bin/env bash
# Copyright (c) 2025 Trilitech <contact@trili.tech>
# Copyright (c) 2025 Functori <contact@functori.com>
#
# SPDX-License-Identifier: MIT

# Test: Cascade import preserves configs across node + baker stack
# Verifies: Node config preserved (CORS, private-mode) in cascade import

set -euo pipefail
source /tests/lib.sh

test_init "Cascade import - all configs preserved"

# Unique instance names for this test
NODE_INSTANCE="casc-cfg-node-$$"
BAKER_INSTANCE="casc-cfg-baker-$$"

NODE_PORT=$(alloc_port)
NODE_RPC_PORT=$(alloc_port)

# Register for cleanup
register_instance "$NODE_INSTANCE"
register_instance "$BAKER_INSTANCE"

echo "Setting up external node with custom config..."

# === NODE ===
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
systemctl start "octez-node@${NODE_INSTANCE}.service"

# Wait for node to be ready
wait_for_node_ready "127.0.0.1:$NODE_RPC_PORT" 30

# === BAKER ===
echo "Creating external baker depending on node..."
BAKER_DATA_DIR="/var/lib/octez-external/$BAKER_INSTANCE"
register_data_dir "$BAKER_DATA_DIR"
mkdir -p "$BAKER_DATA_DIR"
chown -R tezos:tezos "$BAKER_DATA_DIR"

register_external_service "baker" "$BAKER_INSTANCE"
# Create external baker service pointing to node
create_external_service "baker" "$BAKER_INSTANCE" "$BAKER_DATA_DIR" "" "shadownet" "http://127.0.0.1:$NODE_RPC_PORT"
systemctl daemon-reload

echo "Performing cascade import of baker (should also import node)..."
om import "octez-baker@${BAKER_INSTANCE}" --cascade --network shadownet 2>&1

echo "Verifying node config was preserved..."

# Check node config hash unchanged
NODE_CONFIG_HASH_AFTER=$(sha256sum "$NODE_CONFIG" | awk '{print $1}')
if [ "$NODE_CONFIG_HASH" != "$NODE_CONFIG_HASH_AFTER" ]; then
	echo "ERROR: Node config was modified by cascade import!"
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

# Verify both services are managed
if ! service_is_managed "$NODE_INSTANCE"; then
	echo "ERROR: Node not managed after cascade import"
	om list 2>&1
	exit 1
fi
echo "✓ Node is managed"

if ! service_is_managed "$BAKER_INSTANCE"; then
	echo "ERROR: Baker not managed after cascade import"
	om list 2>&1
	exit 1
fi
echo "✓ Baker is managed"

echo "Test passed: All configs preserved in cascade import"
