#!/bin/bash
# Copyright (c) 2026 Nomadic Labs <contact@nomadic-labs.com>
#
# SPDX-License-Identifier: MIT
# Test: Import node service using EnvironmentFile directive with variable expansion
set -euo pipefail
source /tests/lib.sh

test_init "Import node with EnvironmentFile and variable expansion"

INSTANCE="envfile-node-69"
DATA_DIR="/var/lib/octez-external/$INSTANCE"
RPC_ADDR="127.0.0.1:$(alloc_port)"
ENV_FILE="/etc/octez-external/env-node-69"

register_instance "$INSTANCE"
register_external_service "node" "$INSTANCE"
register_data_dir "$DATA_DIR"
register_data_dir "/etc/octez-external"

# Create environment file directory and file
echo "Creating environment file..."
mkdir -p "$(dirname "$ENV_FILE")"
cat >"$ENV_FILE" <<ENVFILE
DATA_DIR=$DATA_DIR
NETWORK=shadownet
RPC_ADDR=$RPC_ADDR
ENVFILE

# Create systemd service that uses EnvironmentFile
echo "Creating external systemd service with EnvironmentFile..."
mkdir -p "$DATA_DIR"
inject_identity "$INSTANCE" "$DATA_DIR"
chown -R tezos:tezos "$DATA_DIR"

unit_name="octez-node@${INSTANCE}.service"
unit_dir="/etc/systemd/system"
octez_bin_path="/usr/local/bin"
p2p_addr="127.0.0.1:$(alloc_port)"

cat >"$unit_dir/$unit_name" <<SERVICE
[Unit]
Description=External Octez Node with EnvironmentFile - $INSTANCE
After=network.target

[Service]
Type=simple
User=tezos
EnvironmentFile=$ENV_FILE
ExecStart=$octez_bin_path/octez-node run --data-dir=\${DATA_DIR} --network=\${NETWORK} --rpc-addr=\${RPC_ADDR} --net-addr=$p2p_addr
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
	echo "DEBUG: Environment file contents:"
	cat "$ENV_FILE" || true
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

# Verify data directory is preserved (same path)
if [ ! -d "$DATA_DIR" ]; then
	echo "ERROR: Data directory should be preserved"
	exit 1
fi

# Verify config.json is preserved
if [ ! -f "$DATA_DIR/config.json" ]; then
	echo "ERROR: config.json should be preserved"
	exit 1
fi

# Verify the managed service has correct configuration
echo "Verifying managed service configuration..."
om instance "$INSTANCE" show 2>&1 | grep -q "shadownet" || {
	echo "ERROR: Network should be shadownet"
	om instance "$INSTANCE" show 2>&1
	exit 1
}

om instance "$INSTANCE" show 2>&1 | grep -q "$RPC_ADDR" || {
	echo "ERROR: RPC address should be preserved"
	om instance "$INSTANCE" show 2>&1
	exit 1
}

# Verify original external service is disabled
if ! external_service_disabled "node" "$INSTANCE"; then
	echo "ERROR: Original service should be disabled after takeover"
	systemctl status "$unit_name" || true
	exit 1
fi

echo "EnvironmentFile import test passed"
