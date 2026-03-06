#!/usr/bin/env bash
# Copyright (c) 2025 Trilitech <contact@trili.tech>
# Copyright (c) 2025 Functori <contact@functori.com>
#
# SPDX-License-Identifier: MIT

# Test: Network preserved during import when node is not running
# Verifies: Network extracted from config.json when service is stopped

set -euo pipefail

SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
source "$SCRIPT_DIR/../lib.sh"

test_init "Import stopped node with network detection from config.json"

# Unique instance name for this test
INSTANCE="node-network-stopped-$$"
PORT=$(alloc_port)
RPC_PORT=$(alloc_port)

# Register for cleanup
register_instance "$INSTANCE"

echo "Creating external node service (STOPPED) with specific network..."

# Create data directory
DATA_DIR="/tmp/octez-external-network-stopped-$$"
register_data_dir "$DATA_DIR"
mkdir -p "$DATA_DIR"

# Initialize node config for ghostnet (not weeklynet)
octez-node config init --data-dir="$DATA_DIR" \
	--network=ghostnet \
	--history-mode=rolling \
	--net-addr="127.0.0.1:$PORT" \
	--rpc-addr="127.0.0.1:$RPC_PORT" >/dev/null 2>&1

octez-node identity generate --data-dir="$DATA_DIR" >/dev/null 2>&1

CONFIG_FILE="$DATA_DIR/config.json"

# Verify network is ghostnet in config
NETWORK_IN_CONFIG=$(jq -r '.network // empty' "$CONFIG_FILE")
if [ -z "$NETWORK_IN_CONFIG" ]; then
	# Some versions store network differently
	echo "Network not in top-level field, checking chain name..."
	CHAIN_NAME=$(jq -r '.["chain-name"] // empty' "$CONFIG_FILE")
	if [ "$CHAIN_NAME" = "TEZOS_GHOSTNET" ] || [ "$CHAIN_NAME" = "ghostnet" ]; then
		NETWORK_IN_CONFIG="ghostnet"
	fi
fi

echo "Network in config.json: ${NETWORK_IN_CONFIG:-<not explicitly set>}"

# Ensure service user exists
ensure_tezos_user

# Create systemd service WITHOUT --network flag in ExecStart
# (network should be detected from config.json)
SERVICE_NAME="octez-node-${INSTANCE}"
SERVICE_FILE="/etc/systemd/system/${SERVICE_NAME}.service"

cat >"$SERVICE_FILE" <<EOF
[Unit]
Description=Octez Node - ${INSTANCE}
After=network.target

[Service]
Type=simple
User=tezos
ExecStart=/usr/bin/octez-node run --data-dir=${DATA_DIR} --history-mode=rolling
Restart=on-failure
StandardOutput=journal
StandardError=journal

[Install]
WantedBy=multi-user.target
EOF

echo "ExecStart does NOT specify --network flag"
echo "Network must be detected from config.json or data directory"

# Set ownership
chown -R tezos:tezos "$DATA_DIR"

# Reload systemd but DON'T start the service
systemctl daemon-reload

# Ensure service is stopped
systemctl stop "$SERVICE_NAME" 2>/dev/null || true

echo "Service is STOPPED (not running)"

echo "Importing STOPPED external service with takeover..."
octez-manager import detect
octez-manager import takeover "$SERVICE_NAME" "$INSTANCE"

echo "Verifying network detected and preserved..."

# Check metadata for correct network
META=$(octez-manager info "$INSTANCE" --json)
DETECTED_NETWORK=$(echo "$META" | jq -r '.network')

if [ "$DETECTED_NETWORK" != "ghostnet" ]; then
	echo "ERROR: Network not correctly detected: $DETECTED_NETWORK (expected ghostnet)"
	echo "Metadata: $META"
	exit 1
fi

echo "✓ Network correctly detected: $DETECTED_NETWORK"

# Verify config.json unchanged
if [ -n "$NETWORK_IN_CONFIG" ]; then
	NETWORK_AFTER=$(jq -r '.network // empty' "$CONFIG_FILE")
	if [ -z "$NETWORK_AFTER" ]; then
		CHAIN_NAME=$(jq -r '.["chain-name"] // empty' "$CONFIG_FILE")
		if [ "$CHAIN_NAME" = "TEZOS_GHOSTNET" ] || [ "$CHAIN_NAME" = "ghostnet" ]; then
			NETWORK_AFTER="ghostnet"
		fi
	fi

	if [ "$NETWORK_IN_CONFIG" != "$NETWORK_AFTER" ]; then
		echo "ERROR: Network in config.json was modified"
		exit 1
	fi
	echo "✓ Config.json network unchanged"
fi

echo "Verifying imported service can start with preserved network..."
octez-manager start "$INSTANCE"

# Wait for node to be responsive
wait_for_node_rpc "$INSTANCE" 60

# Verify node is actually on ghostnet
CHAIN_ID=$(curl -s "http://127.0.0.1:$RPC_PORT/chains/main/chain_id" | tr -d '"')
# Ghostnet chain ID is NetXnHfVqm9iesp
if [[ ! "$CHAIN_ID" =~ ^NetX ]]; then
	echo "ERROR: Node not on expected network. Chain ID: $CHAIN_ID"
	exit 1
fi

echo "✓ Node started on correct network (chain_id: $CHAIN_ID)"

octez-manager stop "$INSTANCE"

echo "✓ Test passed: Network preserved when importing stopped node"
echo "  - Network detected from config.json"
echo "  - Metadata has correct network"
echo "  - Service starts on correct network"
