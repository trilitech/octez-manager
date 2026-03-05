#!/usr/bin/env bash
# Copyright (c) 2025 Trilitech <contact@trili.tech>
# Copyright (c) 2025 Functori <contact@functori.com>
#
# SPDX-License-Identifier: MIT

# Test: Cascade import preserves all configs across full stack
# Verifies: Node, DAL, Baker, and Accuser configs all preserved in cascade import

set -euo pipefail

SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
source "$SCRIPT_DIR/../lib.sh"

# Unique instance names for this test
NODE_INSTANCE="node-cascade-full-$$"
DAL_INSTANCE="dal-cascade-full-$$"
BAKER_INSTANCE="baker-cascade-full-$$"
ACCUSER_INSTANCE="accuser-cascade-full-$$"

NODE_PORT=$(alloc_port)
NODE_RPC_PORT=$(alloc_port)
DAL_RPC_PORT=$(alloc_port)

# Register for cleanup
register_instance "$NODE_INSTANCE"
register_instance "$DAL_INSTANCE"
register_instance "$BAKER_INSTANCE"
register_instance "$ACCUSER_INSTANCE"

log "Setting up full Octez stack (node + DAL + baker + accuser) as external services..."

# === NODE ===
log "Creating external node with custom config..."
NODE_DATA_DIR="/tmp/octez-node-cascade-$$"
register_datadir "$NODE_DATA_DIR"
mkdir -p "$NODE_DATA_DIR"

octez-node config init --data-dir="$NODE_DATA_DIR" \
	--network=weeklynet \
	--history-mode=rolling \
	--net-addr="127.0.0.1:$NODE_PORT" \
	--rpc-addr="127.0.0.1:$NODE_RPC_PORT" >/dev/null 2>&1

octez-node identity generate --data-dir="$NODE_DATA_DIR" >/dev/null 2>&1

# Customize node config
NODE_CONFIG="$NODE_DATA_DIR/config.json"
jq '.rpc."cors-origin" = ["https://cascade-test.com"] | .p2p."private-mode" = true' \
	"$NODE_CONFIG" >"$NODE_CONFIG.tmp" && mv "$NODE_CONFIG.tmp" "$NODE_CONFIG"

NODE_CONFIG_HASH=$(sha256sum "$NODE_CONFIG" | awk '{print $1}')
log "Node config hash: $NODE_CONFIG_HASH"

# Create node service
NODE_SERVICE="octez-node-${NODE_INSTANCE}"
sudo tee "/etc/systemd/system/${NODE_SERVICE}.service" >/dev/null <<EOF
[Unit]
Description=Octez Node - ${NODE_INSTANCE}
After=network.target

[Service]
Type=simple
User=octez
ExecStart=/usr/bin/octez-node run --data-dir=${NODE_DATA_DIR} --network=weeklynet
Restart=on-failure

[Install]
WantedBy=multi-user.target
EOF

sudo chown -R octez:octez "$NODE_DATA_DIR"

# === DAL ===
log "Creating external DAL with custom config..."
DAL_DATA_DIR="/tmp/octez-dal-cascade-$$"
register_datadir "$DAL_DATA_DIR"
mkdir -p "$DAL_DATA_DIR"

octez-dal-node config init --data-dir="$DAL_DATA_DIR" \
	--endpoint="http://127.0.0.1:$NODE_RPC_PORT" \
	--rpc-addr="127.0.0.1:$DAL_RPC_PORT" >/dev/null 2>&1

# Customize DAL config
DAL_CONFIG="$DAL_DATA_DIR/config.json"
jq '.public_addr = "dal-cascade.example.com:10732"' \
	"$DAL_CONFIG" >"$DAL_CONFIG.tmp" && mv "$DAL_CONFIG.tmp" "$DAL_CONFIG"

DAL_CONFIG_HASH=$(sha256sum "$DAL_CONFIG" | awk '{print $1}')
log "DAL config hash: $DAL_CONFIG_HASH"

# Create DAL service
DAL_SERVICE="octez-dal-node-${DAL_INSTANCE}"
sudo tee "/etc/systemd/system/${DAL_SERVICE}.service" >/dev/null <<EOF
[Unit]
Description=Octez DAL - ${DAL_INSTANCE}
After=network.target ${NODE_SERVICE}.service
Requires=${NODE_SERVICE}.service

[Service]
Type=simple
User=octez
ExecStart=/usr/bin/octez-dal-node run --data-dir=${DAL_DATA_DIR}
Restart=on-failure

[Install]
WantedBy=multi-user.target
EOF

sudo chown -R octez:octez "$DAL_DATA_DIR"

# === BAKER ===
log "Creating external baker with custom args..."
BAKER_DATA_DIR="/tmp/octez-baker-cascade-$$"
register_datadir "$BAKER_DATA_DIR"
mkdir -p "$BAKER_DATA_DIR"

# Create baker service with extra args
BAKER_SERVICE="octez-baker-${BAKER_INSTANCE}"
sudo tee "/etc/systemd/system/${BAKER_SERVICE}.service" >/dev/null <<EOF
[Unit]
Description=Octez Baker - ${BAKER_INSTANCE}
After=network.target ${NODE_SERVICE}.service
Requires=${NODE_SERVICE}.service

[Service]
Type=simple
User=octez
ExecStart=/usr/bin/octez-baker-PsQuebec run with local node ${NODE_DATA_DIR} --liquidity-baking-toggle-vote pass --adaptive-issuance-vote pass
Restart=on-failure
Environment="TEZOS_LOG=* -> info"

[Install]
WantedBy=multi-user.target
EOF

# === ACCUSER ===
log "Creating external accuser with custom args..."
ACCUSER_DATA_DIR="/tmp/octez-accuser-cascade-$$"
register_datadir "$ACCUSER_DATA_DIR"
mkdir -p "$ACCUSER_DATA_DIR"

# Create accuser service
ACCUSER_SERVICE="octez-accuser-${ACCUSER_INSTANCE}"
sudo tee "/etc/systemd/system/${ACCUSER_SERVICE}.service" >/dev/null <<EOF
[Unit]
Description=Octez Accuser - ${ACCUSER_INSTANCE}
After=network.target ${NODE_SERVICE}.service
Requires=${NODE_SERVICE}.service

[Service]
Type=simple
User=octez
ExecStart=/usr/bin/octez-accuser-PsQuebec run --endpoint=http://127.0.0.1:${NODE_RPC_PORT} --preserved-levels=10
Restart=on-failure

[Install]
WantedBy=multi-user.target
EOF

sudo systemctl daemon-reload

log "Performing cascade import..."
expect_success octez-manager import detect
expect_success octez-manager import cascade "$NODE_SERVICE" "$NODE_INSTANCE"

log "Verifying all configs preserved..."

# Check node config
NODE_CONFIG_HASH_AFTER=$(sha256sum "$NODE_CONFIG" | awk '{print $1}')
if [ "$NODE_CONFIG_HASH" != "$NODE_CONFIG_HASH_AFTER" ]; then
	error "Node config was modified!"
	exit 1
fi
log "✓ Node config preserved"

# Verify node config contents
if ! jq -e '.rpc."cors-origin"[0] == "https://cascade-test.com"' "$NODE_CONFIG" >/dev/null; then
	error "Node CORS origin not preserved"
	exit 1
fi
log "✓ Node CORS setting preserved"

if ! jq -e '.p2p."private-mode" == true' "$NODE_CONFIG" >/dev/null; then
	error "Node private mode not preserved"
	exit 1
fi
log "✓ Node private mode preserved"

# Check DAL config
DAL_CONFIG_HASH_AFTER=$(sha256sum "$DAL_CONFIG" | awk '{print $1}')
if [ "$DAL_CONFIG_HASH" != "$DAL_CONFIG_HASH_AFTER" ]; then
	error "DAL config was modified!"
	exit 1
fi
log "✓ DAL config preserved"

# Verify DAL config contents
if ! jq -e '.public_addr == "dal-cascade.example.com:10732"' "$DAL_CONFIG" >/dev/null; then
	error "DAL public address not preserved"
	exit 1
fi
log "✓ DAL public address preserved"

# Check baker metadata has extra_args
BAKER_META=$(octez-manager info "$BAKER_INSTANCE" --json)
if ! echo "$BAKER_META" | jq -e '.extra_args | contains(["--liquidity-baking-toggle-vote", "pass"])' >/dev/null; then
	error "Baker extra args not preserved"
	exit 1
fi
log "✓ Baker extra args preserved"

# Check accuser metadata has extra_args
ACCUSER_META=$(octez-manager info "$ACCUSER_INSTANCE" --json)
if ! echo "$ACCUSER_META" | jq -e '.extra_args | contains(["--preserved-levels", "10"])' >/dev/null; then
	error "Accuser extra args not preserved"
	exit 1
fi
log "✓ Accuser extra args preserved"

log "Verifying all services can start..."
expect_success octez-manager start "$NODE_INSTANCE"
wait_for_node_rpc "$NODE_INSTANCE" 60

expect_success octez-manager start "$DAL_INSTANCE"
sleep 5

# Don't start baker/accuser (requires keys), just verify they were imported correctly
if ! octez-manager status "$NODE_INSTANCE" | grep -q "running"; then
	error "Node failed to start"
	exit 1
fi
log "✓ Node running"

if ! octez-manager status "$DAL_INSTANCE" | grep -q "running"; then
	error "DAL failed to start"
	exit 1
fi
log "✓ DAL running"

expect_success octez-manager stop "$DAL_INSTANCE"
expect_success octez-manager stop "$NODE_INSTANCE"

log "✓ Test passed: All configs preserved in cascade import"
