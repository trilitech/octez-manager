#!/bin/bash
# Copyright (c) 2026 Nomadic Labs <contact@nomadic-labs.com>
#
# SPDX-License-Identifier: MIT
# Test: Import node and override RPC address
set -euo pipefail
source /tests/lib.sh

test_init "Import node with RPC address override"

INSTANCE="rpc-override-node-71"
DATA_DIR="/var/lib/octez-external/$INSTANCE"
ORIGINAL_RPC_ADDR="127.0.0.1:$(alloc_port)"
OVERRIDE_RPC_ADDR="127.0.0.1:$(alloc_port)"

register_instance "$INSTANCE"
register_external_service "node" "$INSTANCE"
register_data_dir "$DATA_DIR"

# Create external service with original RPC address
echo "Creating external systemd service with RPC on $ORIGINAL_RPC_ADDR..."
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
ExecStart=$octez_bin_path/octez-node run --data-dir $DATA_DIR --network shadownet --rpc-addr $ORIGINAL_RPC_ADDR --net-addr $p2p_addr
Restart=on-failure
RestartSec=5

[Install]
WantedBy=multi-user.target
SERVICE

systemctl daemon-reload
systemctl enable "$unit_name"

# Initialize node config with original RPC address
echo "Initializing node config..."
runuser -u tezos -- "$octez_bin_path/octez-node" config init --data-dir="$DATA_DIR" --network=shadownet --rpc-addr="$ORIGINAL_RPC_ADDR" --net-addr="$p2p_addr"

# Start service briefly so it can be detected
systemctl start "$unit_name"

# Wait for service to be detected
echo "Waiting for external service detection..."
if ! wait_for_external_service "$INSTANCE"; then
	echo "DEBUG: Systemd unit status:"
	systemctl status "$unit_name" --no-pager || true
	exit 1
fi

# Import with RPC address override
echo "Importing with RPC address override to $OVERRIDE_RPC_ADDR..."
om import "$unit_name" --strategy takeover --rpc-addr "$OVERRIDE_RPC_ADDR" 2>&1

# Stop the service immediately after import
systemctl stop "$unit_name" 2>/dev/null || true

# Verify service is now managed
if ! service_is_managed "$INSTANCE"; then
	echo "ERROR: Service is not managed after import"
	om list 2>&1
	exit 1
fi

# Verify the managed service uses the overridden RPC address
echo "Verifying overridden RPC address..."
om instance "$INSTANCE" show 2>&1 | grep -q "$OVERRIDE_RPC_ADDR" || {
	echo "ERROR: RPC address should be overridden to $OVERRIDE_RPC_ADDR"
	om instance "$INSTANCE" show 2>&1
	exit 1
}

# Verify it does NOT contain the original RPC address
if om instance "$INSTANCE" show 2>&1 | grep -q "$ORIGINAL_RPC_ADDR"; then
	echo "ERROR: Original RPC address should not be present"
	om instance "$INSTANCE" show 2>&1
	exit 1
fi

# Verify original external service is disabled
if ! external_service_disabled "node" "$INSTANCE"; then
	echo "ERROR: Original service should be disabled after takeover"
	systemctl status "$unit_name" || true
	exit 1
fi

echo "RPC override import test passed"
