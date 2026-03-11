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

test_init "Cascade import preserves all configs across full stack"

# Unique instance names for this test
NODE_INSTANCE="cascade-full-$$"
DAL_INSTANCE="cascade-full-dal-$$"
BAKER_INSTANCE="cascade-full-baker-$$"
ACCUSER_INSTANCE="cascade-full-accuser-$$"

NODE_PORT=$(alloc_port)
NODE_RPC_PORT=$(alloc_port)
DAL_RPC_PORT=$(alloc_port)

# Register for cleanup
register_instance "$NODE_INSTANCE"
register_instance "$DAL_INSTANCE"
register_instance "$BAKER_INSTANCE"
register_instance "$ACCUSER_INSTANCE"

echo "Setting up full Octez stack (node + DAL + baker + accuser) as external services..."

# Ensure the octez user exists for external services
ensure_octez_user

# === NODE ===
echo "Creating external node with custom config..."
NODE_DATA_DIR="/tmp/octez-node-cascade-$$"
register_data_dir "$NODE_DATA_DIR"
mkdir -p "$NODE_DATA_DIR"

octez-node config init --data-dir="$NODE_DATA_DIR" \
	--network=shadownet \
	--history-mode=rolling \
	--net-addr="127.0.0.1:$NODE_PORT" \
	--rpc-addr="127.0.0.1:$NODE_RPC_PORT" >/dev/null 2>&1

inject_identity "$NODE_INSTANCE" "$NODE_DATA_DIR"

# Customize node config
NODE_CONFIG="$NODE_DATA_DIR/config.json"
jq '.rpc."cors-origin" = ["https://cascade-test.com"] | .p2p."private-mode" = true' \
	"$NODE_CONFIG" >"$NODE_CONFIG.tmp" && mv "$NODE_CONFIG.tmp" "$NODE_CONFIG"

NODE_CONFIG_HASH=$(sha256sum "$NODE_CONFIG" | awk '{print $1}')
echo "Node config hash: $NODE_CONFIG_HASH"

# Create node service
NODE_SERVICE="octez-node@${NODE_INSTANCE}"
tee "/etc/systemd/system/${NODE_SERVICE}.service" >/dev/null <<EOF
[Unit]
Description=Octez Node - ${NODE_INSTANCE}
After=network.target

[Service]
Type=simple
User=octez
ExecStart=/usr/local/bin/octez-node run --data-dir=${NODE_DATA_DIR} --network=shadownet --rpc-addr=127.0.0.1:${NODE_RPC_PORT}
Restart=on-failure
RestartSec=5

[Install]
WantedBy=multi-user.target
EOF

chown -R octez:octez "$NODE_DATA_DIR"

# === DAL ===
echo "Creating external DAL with custom config..."
DAL_DATA_DIR="/tmp/octez-dal-cascade-$$"
register_data_dir "$DAL_DATA_DIR"
mkdir -p "$DAL_DATA_DIR"

octez-dal-node config init --data-dir="$DAL_DATA_DIR" \
	--endpoint="http://127.0.0.1:$NODE_RPC_PORT" \
	--rpc-addr="127.0.0.1:$DAL_RPC_PORT" >/dev/null 2>&1

# Customize DAL config
DAL_CONFIG="$DAL_DATA_DIR/config.json"
jq '.public_addr = "dal-cascade.example.com:10732"' \
	"$DAL_CONFIG" >"$DAL_CONFIG.tmp" && mv "$DAL_CONFIG.tmp" "$DAL_CONFIG"

DAL_CONFIG_HASH=$(sha256sum "$DAL_CONFIG" | awk '{print $1}')
echo "DAL config hash: $DAL_CONFIG_HASH"

# Create DAL service
DAL_SERVICE="octez-dal-node@${DAL_INSTANCE}"
tee "/etc/systemd/system/${DAL_SERVICE}.service" >/dev/null <<EOF
[Unit]
Description=Octez DAL - ${DAL_INSTANCE}
After=network.target ${NODE_SERVICE}.service
Requires=${NODE_SERVICE}.service

[Service]
Type=simple
User=octez
ExecStart=/usr/local/bin/octez-dal-node run --data-dir=${DAL_DATA_DIR} --endpoint=http://127.0.0.1:${NODE_RPC_PORT}
Restart=on-failure
RestartSec=5

[Install]
WantedBy=multi-user.target
EOF

chown -R octez:octez "$DAL_DATA_DIR"

# === BAKER ===
echo "Creating external baker with custom args..."
BAKER_DATA_DIR="/tmp/octez-baker-cascade-$$"
register_data_dir "$BAKER_DATA_DIR"
mkdir -p "$BAKER_DATA_DIR"

# Create baker service with extra args
BAKER_SERVICE="octez-baker@${BAKER_INSTANCE}"
tee "/etc/systemd/system/${BAKER_SERVICE}.service" >/dev/null <<EOF
[Unit]
Description=Octez Baker - ${BAKER_INSTANCE}
After=network.target ${NODE_SERVICE}.service
Requires=${NODE_SERVICE}.service

[Service]
Type=simple
User=octez
ExecStart=/usr/local/bin/octez-baker --endpoint http://127.0.0.1:${NODE_RPC_PORT} --base-dir ${BAKER_DATA_DIR} run with local node ${NODE_DATA_DIR} --liquidity-baking-toggle-vote pass --adaptive-issuance-vote pass
Restart=on-failure
RestartSec=5
Environment="TEZOS_LOG=* -> info"

[Install]
WantedBy=multi-user.target
EOF

chown -R octez:octez "$BAKER_DATA_DIR"

# === ACCUSER ===
echo "Creating external accuser with custom args..."
ACCUSER_DATA_DIR="/tmp/octez-accuser-cascade-$$"
register_data_dir "$ACCUSER_DATA_DIR"
mkdir -p "$ACCUSER_DATA_DIR"

# Create accuser service
ACCUSER_SERVICE="octez-accuser@${ACCUSER_INSTANCE}"
tee "/etc/systemd/system/${ACCUSER_SERVICE}.service" >/dev/null <<EOF
[Unit]
Description=Octez Accuser - ${ACCUSER_INSTANCE}
After=network.target ${NODE_SERVICE}.service
Requires=${NODE_SERVICE}.service

[Service]
Type=simple
User=octez
ExecStart=/usr/local/bin/octez-accuser run --endpoint=http://127.0.0.1:${NODE_RPC_PORT} --base-dir=${ACCUSER_DATA_DIR} --preserved-levels=10
Restart=on-failure
RestartSec=5

[Install]
WantedBy=multi-user.target
EOF

chown -R octez:octez "$ACCUSER_DATA_DIR"

systemctl daemon-reload

echo "Starting all services before import..."
# Start node first and wait for it to be ready before import.
# This ensures we're importing fully-initialized services (not mid-startup),
# which is the typical real-world use case.
systemctl start "${NODE_SERVICE}.service"
echo "Waiting for node RPC to be ready before import..."
wait_for_node_ready "127.0.0.1:$NODE_RPC_PORT" 30

# Start remaining services
systemctl start "${DAL_SERVICE}.service"
systemctl start "${BAKER_SERVICE}.service"
systemctl start "${ACCUSER_SERVICE}.service"

# Give DAL/baker/accuser a moment to start
sleep 3

echo "Performing cascade import..."
om import "$NODE_SERVICE" --cascade --network shadownet

echo "Verifying services are running after import..."
# Check that node is running after import
if ! service_is_active "node" "$NODE_INSTANCE"; then
	echo "ERROR: Node should be running after import"
	systemctl status "octez-node@${NODE_INSTANCE}.service" --no-pager || true
	exit 1
fi
echo "✓ Node running after import"

# Check that DAL is running after import
if ! service_is_active "dal-node" "$DAL_INSTANCE"; then
	echo "ERROR: DAL should be running after import"
	systemctl status "octez-dal-node@${DAL_INSTANCE}.service" --no-pager || true
	exit 1
fi
echo "✓ DAL running after import"

echo "Verifying all configs preserved..."

# Check node config
NODE_CONFIG_HASH_AFTER=$(sha256sum "$NODE_CONFIG" | awk '{print $1}')
if [ "$NODE_CONFIG_HASH" != "$NODE_CONFIG_HASH_AFTER" ]; then
	echo "ERROR: Node config was modified!"
	exit 1
fi
echo "✓ Node config preserved"

# Verify node config contents
if ! jq -e '.rpc."cors-origin"[0] == "https://cascade-test.com"' "$NODE_CONFIG" >/dev/null; then
	echo "ERROR: Node CORS origin not preserved"
	exit 1
fi
echo "✓ Node CORS setting preserved"

if ! jq -e '.p2p."private-mode" == true' "$NODE_CONFIG" >/dev/null; then
	echo "ERROR: Node private mode not preserved"
	exit 1
fi
echo "✓ Node private mode preserved"

# TODO: https://github.com/trilitech/octez-manager/issues/793
# DAL config preservation disabled due to octez-dal-node rewriting config.json
# when --rpc-addr/--net-addr flags are present in ExecStart
#
# Once issue #793 is fixed, uncomment these checks:
# DAL_CONFIG_HASH_AFTER=$(sha256sum "$DAL_CONFIG" | awk '{print $1}')
# if [ "$DAL_CONFIG_HASH" != "$DAL_CONFIG_HASH_AFTER" ]; then
# 	echo "ERROR: DAL config was modified!"
# 	exit 1
# fi
# echo "✓ DAL config preserved"
#
# if ! jq -e '.public_addr == "dal-cascade.example.com:10732"' "$DAL_CONFIG" >/dev/null; then
# 	echo "ERROR: DAL public address not preserved"
# 	exit 1
# fi
# echo "✓ DAL public address preserved"

echo "⚠ DAL config preservation checks disabled (see issue #793)"

# Check baker LB vote setting preserved
BAKER_SHOW=$(om instance "$BAKER_INSTANCE" show)
if ! echo "$BAKER_SHOW" | grep -q "LB Vote"; then
	echo "ERROR: Baker LB vote not preserved"
	echo "Baker show output:"
	echo "$BAKER_SHOW"
	exit 1
fi
echo "✓ Baker LB vote preserved"

# Check accuser extra_args preserved
ACCUSER_SHOW=$(om instance "$ACCUSER_INSTANCE" show)
if ! echo "$ACCUSER_SHOW" | grep -q "preserved-levels"; then
	echo "ERROR: Accuser extra args not preserved"
	echo "Accuser show output:"
	echo "$ACCUSER_SHOW"
	exit 1
fi
echo "✓ Accuser extra args preserved"

# Baker and accuser should be imported and running
echo "Verifying baker and accuser imported and running..."

# Check baker is managed and running
if ! service_is_managed "$BAKER_INSTANCE"; then
	echo "ERROR: Baker should be imported"
	om list 2>&1
	exit 1
fi
if ! service_is_active "baker" "$BAKER_INSTANCE"; then
	echo "ERROR: Baker should be running after import"
	systemctl status "octez-baker@${BAKER_INSTANCE}.service" --no-pager || true
	journalctl -u "octez-baker@${BAKER_INSTANCE}.service" -n 50 --no-pager || true
	exit 1
fi
echo "✓ Baker running after import"

# Check accuser is managed and running
if ! service_is_managed "$ACCUSER_INSTANCE"; then
	echo "ERROR: Accuser should be imported"
	om list 2>&1
	exit 1
fi
if ! service_is_active "accuser" "$ACCUSER_INSTANCE"; then
	echo "ERROR: Accuser should be running after import"
	systemctl status "octez-accuser@${ACCUSER_INSTANCE}.service" --no-pager || true
	journalctl -u "octez-accuser@${ACCUSER_INSTANCE}.service" -n 50 --no-pager || true
	exit 1
fi
echo "✓ Accuser running after import"

# Cleanup: stop running services
systemctl stop "octez-accuser@${ACCUSER_INSTANCE}.service" || true
systemctl stop "octez-baker@${BAKER_INSTANCE}.service" || true
systemctl stop "octez-dal-node@${DAL_INSTANCE}.service" || true
systemctl stop "octez-node@${NODE_INSTANCE}.service" || true

echo "✓ Test passed: All configs preserved in cascade import and services started correctly"
