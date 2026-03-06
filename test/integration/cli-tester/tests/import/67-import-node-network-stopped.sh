#!/bin/bash
# Copyright (c) 2025 Trilitech <contact@trili.tech>
# Copyright (c) 2025 Functori <contact@functori.com>
#
# SPDX-License-Identifier: MIT

# Test: Network preserved during import when node is not running
# Verifies: Network extracted from config.json when service is stopped

set -euo pipefail
source /tests/lib.sh

test_init "Import stopped node with network detection from config.json"

INSTANCE="node-network-stopped"
EXTERNAL_DATA="/var/lib/octez-external/$INSTANCE"
RPC_PORT=$(alloc_port)
P2P_PORT=$(alloc_port)

register_instance "$INSTANCE"
register_external_service "node" "$INSTANCE"
register_data_dir "$EXTERNAL_DATA"

echo "Creating external node service (STOPPED) with ghostnet network..."

# Create data directory
mkdir -p "$EXTERNAL_DATA"

# Initialize node config for ghostnet
octez-node config init --data-dir="$EXTERNAL_DATA" \
	--network=ghostnet \
	--history-mode=rolling \
	--net-addr="127.0.0.1:$P2P_PORT" \
	--rpc-addr="127.0.0.1:$RPC_PORT" >/dev/null 2>&1

inject_identity "$INSTANCE" "$EXTERNAL_DATA"
chown -R tezos:tezos "$EXTERNAL_DATA"

CONFIG_FILE="$EXTERNAL_DATA/config.json"

# Verify network is ghostnet in config
NETWORK_IN_CONFIG=$(jq -r '.network // empty' "$CONFIG_FILE")
if [ -z "$NETWORK_IN_CONFIG" ]; then
	CHAIN_NAME=$(jq -r '.["chain-name"] // .network."chain-name" // empty' "$CONFIG_FILE")
	if [[ "$CHAIN_NAME" =~ GHOSTNET|ghostnet ]]; then
		NETWORK_IN_CONFIG="ghostnet"
	fi
fi

echo "Network in config.json: ${NETWORK_IN_CONFIG:-<not set>}"

# Create systemd service WITHOUT --network flag in ExecStart
# The network should be detected from config.json
SERVICE_NAME="octez-node@${INSTANCE}"
SERVICE_FILE="/etc/systemd/system/${SERVICE_NAME}.service"

cat >"$SERVICE_FILE" <<EOF
[Unit]
Description=External Octez Node - ${INSTANCE}
After=network.target

[Service]
Type=simple
User=tezos
ExecStart=/usr/bin/octez-node run --data-dir=${EXTERNAL_DATA} --history-mode=rolling
Restart=on-failure
StandardOutput=journal
StandardError=journal

[Install]
WantedBy=multi-user.target
EOF

systemctl daemon-reload

# Keep service STOPPED - do not start it
echo "Service created but STOPPED (not running)"

echo "Importing STOPPED external service with takeover..."
om import "$SERVICE_NAME" --strategy takeover

echo "Verifying network detected from config.json..."

# Check metadata for correct network
if ! service_is_managed "$INSTANCE"; then
	echo "ERROR: Service not imported"
	om list 2>&1
	exit 1
fi

META=$(om info "$INSTANCE" --json)
DETECTED_NETWORK=$(echo "$META" | jq -r '.network')

if [ "$DETECTED_NETWORK" != "ghostnet" ]; then
	echo "ERROR: Network not correctly detected: got '$DETECTED_NETWORK', expected 'ghostnet'"
	echo "Metadata: $META"
	exit 1
fi

echo "✓ Network correctly detected: $DETECTED_NETWORK"

# Verify config.json unchanged
NETWORK_AFTER=$(jq -r '.network // empty' "$CONFIG_FILE")
if [ -z "$NETWORK_AFTER" ]; then
	CHAIN_NAME=$(jq -r '.["chain-name"] // .network."chain-name" // empty' "$CONFIG_FILE")
	if [[ "$CHAIN_NAME" =~ GHOSTNET|ghostnet ]]; then
		NETWORK_AFTER="ghostnet"
	fi
fi

if [ "$NETWORK_IN_CONFIG" != "$NETWORK_AFTER" ]; then
	echo "ERROR: Network in config.json was modified"
	echo "Before: $NETWORK_IN_CONFIG"
	echo "After: $NETWORK_AFTER"
	exit 1
fi
echo "✓ Config.json network unchanged"

echo "Verifying imported service can start with preserved network..."
om start "$INSTANCE"

# Wait for node to be responsive
wait_for_node_rpc "$INSTANCE" 60

# Verify node is actually on ghostnet
CHAIN_ID=$(curl -s "http://127.0.0.1:$RPC_PORT/chains/main/chain_id" | tr -d '"')
# Ghostnet chain ID starts with NetX
if [[ ! "$CHAIN_ID" =~ ^NetX ]]; then
	echo "ERROR: Node not on expected network. Chain ID: $CHAIN_ID"
	exit 1
fi

echo "✓ Node started on correct network (chain_id: $CHAIN_ID)"

om stop "$INSTANCE"

echo "✓ Test passed: Network preserved when importing stopped node"
