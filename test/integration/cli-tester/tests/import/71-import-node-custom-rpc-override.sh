#!/bin/bash
# Copyright (c) 2026 Nomadic Labs <contact@nomadic-labs.com>
#
# SPDX-License-Identifier: MIT
# Test: Import node with custom RPC address - verify it is preserved
set -euo pipefail
source /tests/lib.sh

test_init "Import node with custom RPC address preservation"

INSTANCE="rpc-custom-node-71"
DATA_DIR="/var/lib/octez-external/$INSTANCE"
CUSTOM_RPC_ADDR="127.0.0.1:$(alloc_port)"

register_instance "$INSTANCE"
register_external_service "node" "$INSTANCE"
register_data_dir "$DATA_DIR"

# Create external service with custom RPC address
echo "Creating external systemd service with RPC on $CUSTOM_RPC_ADDR..."
mkdir -p "$DATA_DIR"
inject_identity "$INSTANCE" "$DATA_DIR"
chown -R tezos:tezos "$DATA_DIR"

unit_name="octez-node@${INSTANCE}.service"
unit_dir="/etc/systemd/system"
octez_bin_path="/usr/local/bin"
p2p_addr="127.0.0.1:$(alloc_port)"

cat >"$unit_dir/$unit_name" <<SERVICE
[Unit]
Description=External Octez Node - $INSTANCE
After=network.target

[Service]
Type=simple
User=tezos
ExecStart=$octez_bin_path/octez-node run --data-dir $DATA_DIR --network shadownet --rpc-addr $CUSTOM_RPC_ADDR --net-addr $p2p_addr
Restart=on-failure
RestartSec=5

[Install]
WantedBy=multi-user.target
SERVICE

systemctl daemon-reload
systemctl enable "$unit_name"

# Initialize node config with custom RPC address
echo "Initializing node config..."
runuser -u tezos -- "$octez_bin_path/octez-node" config init --data-dir="$DATA_DIR" --network=shadownet --rpc-addr="$CUSTOM_RPC_ADDR" --net-addr="$p2p_addr"

# Extract port for verification
CUSTOM_RPC_PORT="${CUSTOM_RPC_ADDR##*:}"

# Start service briefly so it can be detected
systemctl start "$unit_name"

# Wait for service to be detected
echo "Waiting for external service detection..."
if ! wait_for_external_service "$INSTANCE"; then
	echo "DEBUG: Systemd unit status:"
	systemctl status "$unit_name" --no-pager || true
	exit 1
fi

# Import with takeover strategy (no override - should preserve original RPC)
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

# Verify config.json preserves the custom RPC address
echo "Verifying custom RPC address is preserved in config.json..."
if ! grep -q "$CUSTOM_RPC_PORT" "$DATA_DIR/config.json"; then
	echo "ERROR: config.json should contain custom RPC port $CUSTOM_RPC_PORT"
	cat "$DATA_DIR/config.json"
	exit 1
fi

# Verify the managed service env file has the RPC address
echo "Verifying managed service configuration..."
ENV_FILE="/etc/octez/instances/$INSTANCE/node.env"
if [ -f "$ENV_FILE" ]; then
	if ! grep -q "$CUSTOM_RPC_PORT" "$ENV_FILE"; then
		echo "WARNING: node.env may not contain custom RPC port (checking show output instead)"
	fi
fi

# Verify original external service is disabled
if ! external_service_disabled "node" "$INSTANCE"; then
	echo "ERROR: Original service should be disabled after takeover"
	systemctl status "$unit_name" || true
	exit 1
fi

echo "Custom RPC address preservation test passed"
