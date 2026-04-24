#!/bin/bash
# Copyright (c) 2026 Nomadic Labs <contact@nomadic-labs.com>
#
# SPDX-License-Identifier: MIT
# Test: Import, purge, then re-import the same external service
set -euo pipefail
source /tests/lib.sh

test_init "Import, purge, and re-import external service"

INSTANCE="reimport-node-73"
DATA_DIR="/var/lib/octez-external/$INSTANCE"
RPC_ADDR="127.0.0.1:$(alloc_port)"

register_instance "$INSTANCE"
register_external_service "node" "$INSTANCE"
register_data_dir "$DATA_DIR"

# Create external service
echo "Creating external systemd service..."
mkdir -p "$DATA_DIR"
inject_identity "$INSTANCE" "$DATA_DIR"
chown -R tezos:tezos "$DATA_DIR"

local unit_name="octez-node@${INSTANCE}.service"
local unit_dir="/etc/systemd/system"
local octez_bin_path="/usr/local/bin"
local p2p_addr="127.0.0.1:$(alloc_port)"

cat >"$unit_dir/$unit_name" <<SERVICE
[Unit]
Description=External Octez Node - $INSTANCE
After=network.target

[Service]
Type=simple
User=tezos
ExecStart=$octez_bin_path/octez-node run --data-dir $DATA_DIR --network shadownet --rpc-addr $RPC_ADDR --net-addr $p2p_addr
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

# First import with clone (so original stays)
echo "First import with clone strategy..."
om import "$unit_name" --strategy clone 2>&1

# Stop the managed service
systemctl stop "$unit_name" 2>/dev/null || true

# Verify service is managed
if ! service_is_managed "$INSTANCE"; then
	echo "ERROR: Service should be managed after first import"
	om list 2>&1
	exit 1
fi

# Verify original external service is still enabled (clone doesn't disable)
if external_service_disabled "node" "$INSTANCE"; then
	echo "ERROR: Original service should still be enabled after clone"
	systemctl status "$unit_name" || true
	exit 1
fi

# Purge the managed instance
echo "Purging managed instance..."
om instance "$INSTANCE" purge 2>&1

# Verify instance is gone
if service_is_managed "$INSTANCE"; then
	echo "ERROR: Service should not be managed after purge"
	om list 2>&1
	exit 1
fi

# Re-enable and restart the external service (it should still exist)
echo "Re-enabling external service..."
systemctl enable "$unit_name"
systemctl start "$unit_name"

# Wait for external service to be detected again
echo "Waiting for external service detection after purge..."
if ! wait_for_external_service "$INSTANCE"; then
	echo "DEBUG: Systemd unit status:"
	systemctl status "$unit_name" --no-pager || true
	echo "DEBUG: om list output:"
	om list 2>&1
	exit 1
fi

# Re-import with takeover
echo "Re-importing with takeover strategy..."
om import "$unit_name" --strategy takeover 2>&1

# Stop the service
systemctl stop "$unit_name" 2>/dev/null || true

# Verify service is managed again
if ! service_is_managed "$INSTANCE"; then
	echo "ERROR: Service should be managed after re-import"
	om list 2>&1
	exit 1
fi

# Verify original external service is disabled after takeover
if ! external_service_disabled "node" "$INSTANCE"; then
	echo "ERROR: Original service should be disabled after takeover"
	systemctl status "$unit_name" || true
	exit 1
fi

echo "Re-import test passed"
