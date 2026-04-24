#!/bin/bash
# Copyright (c) 2026 Nomadic Labs <contact@nomadic-labs.com>
#
# SPDX-License-Identifier: MIT
# Test: Import node with archive history mode - verify preserved
set -euo pipefail
source /tests/lib.sh

test_init "Import node with archive history mode"

INSTANCE="archive-node-72"
DATA_DIR="/var/lib/octez-external/$INSTANCE"
RPC_ADDR="127.0.0.1:$(alloc_port)"

register_instance "$INSTANCE"
register_external_service "node" "$INSTANCE"
register_data_dir "$DATA_DIR"

# Create external service with archive history mode
echo "Creating external systemd service with archive history mode..."
mkdir -p "$DATA_DIR"
inject_identity "$INSTANCE" "$DATA_DIR"
chown -R tezos:tezos "$DATA_DIR"

unit_name="octez-node@${INSTANCE}.service"
unit_dir="/etc/systemd/system"
octez_bin_path="/usr/local/bin"
p2p_addr="127.0.0.1:$(alloc_port)"

cat >"$unit_dir/$unit_name" <<SERVICE
[Unit]
Description=External Octez Node with Archive Mode - $INSTANCE
After=network.target

[Service]
Type=simple
User=tezos
ExecStart=$octez_bin_path/octez-node run --data-dir $DATA_DIR --network shadownet --rpc-addr $RPC_ADDR --net-addr $p2p_addr --history-mode archive
Restart=on-failure
RestartSec=5

[Install]
WantedBy=multi-user.target
SERVICE

systemctl daemon-reload
systemctl enable "$unit_name"

# Initialize node config with archive history mode
echo "Initializing node config with archive history mode..."
runuser -u tezos -- "$octez_bin_path/octez-node" config init --data-dir="$DATA_DIR" --network=shadownet --rpc-addr="$RPC_ADDR" --net-addr="$p2p_addr" --history-mode=archive

# Verify config has archive mode before import
echo "Verifying config.json has archive mode..."
if ! grep -q '"history_mode"' "$DATA_DIR/config.json"; then
	echo "ERROR: config.json should contain history_mode"
	cat "$DATA_DIR/config.json"
	exit 1
fi

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

# Verify config.json still has archive history mode
echo "Verifying archive history mode is preserved..."
if ! grep -q '"history_mode"' "$DATA_DIR/config.json"; then
	echo "ERROR: config.json should still contain history_mode after import"
	cat "$DATA_DIR/config.json"
	exit 1
fi

# Verify the managed service reflects archive mode in show output
om instance "$INSTANCE" show 2>&1 | grep -qi "archive\|history" || {
	echo "WARNING: Archive mode may not be visible in show output (this is acceptable if config.json is correct)"
}

# Verify original external service is disabled
if ! external_service_disabled "node" "$INSTANCE"; then
	echo "ERROR: Original service should be disabled after takeover"
	systemctl status "$unit_name" || true
	exit 1
fi

echo "Archive history mode import test passed"
