#!/bin/bash
# Copyright (c) 2026 Nomadic Labs <contact@nomadic-labs.com>
#
# SPDX-License-Identifier: MIT
# Test: Import node with custom net-addr - verify P2P settings preserved
set -euo pipefail
source /tests/lib.sh

test_init "Import node with custom net-addr P2P settings"

INSTANCE="netaddr-node-74"
DATA_DIR="/var/lib/octez-external/$INSTANCE"
RPC_ADDR="127.0.0.1:$(alloc_port)"
CUSTOM_NET_ADDR="0.0.0.0:$(alloc_port)"

register_instance "$INSTANCE"
register_external_service "node" "$INSTANCE"
register_data_dir "$DATA_DIR"

# Create external service with custom net-addr
echo "Creating external systemd service with custom net-addr $CUSTOM_NET_ADDR..."
mkdir -p "$DATA_DIR"
inject_identity "$INSTANCE" "$DATA_DIR"
chown -R tezos:tezos "$DATA_DIR"

unit_name="octez-node@${INSTANCE}.service"
unit_dir="/etc/systemd/system"
octez_bin_path="/usr/local/bin"

cat >"$unit_dir/$unit_name" <<SERVICE
[Unit]
Description=External Octez Node with Custom Net Addr - $INSTANCE
After=network.target

[Service]
Type=simple
User=tezos
ExecStart=$octez_bin_path/octez-node run --data-dir $DATA_DIR --network shadownet --rpc-addr $RPC_ADDR --net-addr $CUSTOM_NET_ADDR
Restart=on-failure
RestartSec=5

[Install]
WantedBy=multi-user.target
SERVICE

systemctl daemon-reload
systemctl enable "$unit_name"

# Initialize node config with custom net-addr
echo "Initializing node config with custom net-addr..."
runuser -u tezos -- "$octez_bin_path/octez-node" config init --data-dir="$DATA_DIR" --network=shadownet --rpc-addr="$RPC_ADDR" --net-addr="$CUSTOM_NET_ADDR"

# Verify config has custom net-addr before import
echo "Verifying config.json has custom net-addr..."
if ! grep -q '"p2p"' "$DATA_DIR/config.json"; then
	echo "ERROR: config.json should contain p2p configuration"
	cat "$DATA_DIR/config.json"
	exit 1
fi

# Extract the port from CUSTOM_NET_ADDR for verification
CUSTOM_PORT="${CUSTOM_NET_ADDR##*:}"

# Start service briefly so it can be detected
systemctl start "$unit_name"

# Wait for service to be detected
echo "Waiting for external service detection..."
if ! wait_for_external_service "$INSTANCE"; then
	echo "DEBUG: Systemd unit status:"
	systemctl status "$unit_name" --no-pager || true
	exit 1
fi

# Import with takeover strategy
echo "Importing with takeover strategy..."
om import "$unit_name" --strategy takeover 2>&1

# Stop the service immediately after import
systemctl stop "$unit_name" 2>/dev/null || true

# Verify service is now managed
if ! service_is_managed "$INSTANCE"; then
	echo "ERROR: Service is not managed after import"
	om list 2>&1
	exit 1
fi

# Verify config.json still has custom net-addr P2P settings
echo "Verifying P2P configuration is preserved..."
if ! grep -q '"p2p"' "$DATA_DIR/config.json"; then
	echo "ERROR: config.json should still contain p2p configuration after import"
	cat "$DATA_DIR/config.json"
	exit 1
fi

# Verify the custom port is in the config
if ! grep -q "$CUSTOM_PORT" "$DATA_DIR/config.json"; then
	echo "ERROR: config.json should contain custom P2P port $CUSTOM_PORT"
	cat "$DATA_DIR/config.json"
	exit 1
fi

# Verify the managed service preserves the P2P configuration
om instance "$INSTANCE" show 2>&1 | grep -q "$CUSTOM_PORT" || {
	echo "WARNING: Custom P2P port may not be visible in show output (this is acceptable if config.json is correct)"
}

# Verify original external service is disabled
if ! external_service_disabled "node" "$INSTANCE"; then
	echo "ERROR: Original service should be disabled after takeover"
	systemctl status "$unit_name" || true
	exit 1
fi

echo "Custom net-addr import test passed"
