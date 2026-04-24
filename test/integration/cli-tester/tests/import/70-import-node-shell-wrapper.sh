#!/bin/bash
# Copyright (c) 2026 Nomadic Labs <contact@nomadic-labs.com>
#
# SPDX-License-Identifier: MIT
# Test: Import node service with shell-wrapped ExecStart
set -euo pipefail
source /tests/lib.sh

test_init "Import node with shell-wrapped ExecStart"

INSTANCE="shell-node-70"
DATA_DIR="/var/lib/octez-external/$INSTANCE"
RPC_ADDR="127.0.0.1:$(alloc_port)"

register_instance "$INSTANCE"
register_external_service "node" "$INSTANCE"
register_data_dir "$DATA_DIR"

# Create external service with shell-wrapped ExecStart
echo "Creating external systemd service with shell wrapper..."
mkdir -p "$DATA_DIR"
inject_identity "$INSTANCE" "$DATA_DIR"
chown -R tezos:tezos "$DATA_DIR"

unit_name="octez-node@${INSTANCE}.service"
unit_dir="/etc/systemd/system"
octez_bin_path="/usr/local/bin"
p2p_addr="127.0.0.1:$(alloc_port)"

cat >"$unit_dir/$unit_name" <<SERVICE
[Unit]
Description=External Octez Node with Shell Wrapper - $INSTANCE
After=network.target

[Service]
Type=simple
User=tezos
ExecStart=/bin/sh -c '$octez_bin_path/octez-node run --data-dir $DATA_DIR --network shadownet --rpc-addr $RPC_ADDR --net-addr $p2p_addr'
Restart=on-failure
RestartSec=5

[Install]
WantedBy=multi-user.target
SERVICE

systemctl daemon-reload
systemctl enable "$unit_name"

# Initialize node config
echo "Initializing node config..."
runuser -u tezos -- "$octez_bin_path/octez-node" config init --data-dir="$DATA_DIR" --network=shadownet --rpc-addr="$RPC_ADDR" --net-addr="$p2p_addr"

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

# Verify data directory is preserved
if [ ! -d "$DATA_DIR" ]; then
	echo "ERROR: Data directory should be preserved"
	exit 1
fi

# Verify network detected correctly (shadownet)
echo "Verifying network configuration..."
om instance "$INSTANCE" show 2>&1 | grep -q "shadownet" || {
	echo "ERROR: Network should be shadownet"
	om instance "$INSTANCE" show 2>&1
	exit 1
}

# Verify original external service is disabled
if ! external_service_disabled "node" "$INSTANCE"; then
	echo "ERROR: Original service should be disabled after takeover"
	systemctl status "$unit_name" || true
	exit 1
fi

echo "Shell wrapper import test passed"
