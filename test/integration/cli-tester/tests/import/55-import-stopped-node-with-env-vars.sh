#!/bin/bash
# Test: Import stopped node with environment variables (reads EnvironmentFiles and expands vars)
set -euo pipefail
source /tests/lib.sh

test_init "Import stopped node with EnvironmentFiles"

INSTANCE="stopped-node-env"
DATA_DIR="/var/lib/octez-external/$INSTANCE"
RPC_ADDR="127.0.0.1:$(alloc_port)"
NET_ADDR="0.0.0.0:$(alloc_port)"
ENV_FILE="/etc/octez-external/$INSTANCE.env"

register_instance "$INSTANCE"
register_external_service "node" "$INSTANCE"
register_data_dir "$DATA_DIR"

# Create external service data directory
echo "Creating external node data directory..."
mkdir -p "$DATA_DIR"
inject_identity "$INSTANCE" "$DATA_DIR"

# Create config.json with network information
echo "Creating config.json with network information..."
cat >"$DATA_DIR/config.json" <<EOF
{
  "data-dir": "$DATA_DIR",
  "rpc": {
    "listen-addrs": ["$RPC_ADDR"]
  },
  "p2p": {
    "listen-addr": "$NET_ADDR",
    "bootstrap-peers": ["shadownet.teztnets.com"]
  },
  "network": "shadownet"
}
EOF

chown -R tezos:tezos "$DATA_DIR"

# Create environment file with variables
echo "Creating environment file..."
mkdir -p "$(dirname "$ENV_FILE")"
cat >"$ENV_FILE" <<EOF
OCTEZ_DATA_DIR=$DATA_DIR
OCTEZ_RPC_ADDR=$RPC_ADDR
OCTEZ_NET_ADDR=$NET_ADDR
OCTEZ_NETWORK=shadownet
OCTEZ_BIN_DIR=/usr/bin
EOF

# Create systemd service that uses environment variables
echo "Creating systemd service with EnvironmentFile..."
UNIT_NAME="octez-external-node-${INSTANCE}.service"
cat >"/etc/systemd/system/$UNIT_NAME" <<EOF
[Unit]
Description=External Octez Node (with env vars) - $INSTANCE
After=network.target

[Service]
Type=simple
User=tezos
Group=tezos
EnvironmentFile=$ENV_FILE
ExecStart=/bin/sh -c 'exec "\${OCTEZ_BIN_DIR}/octez-node" run --data-dir "\${OCTEZ_DATA_DIR}" --rpc-addr "\${OCTEZ_RPC_ADDR}" --net-addr "\${OCTEZ_NET_ADDR}" --network "\${OCTEZ_NETWORK}" --history-mode rolling'
Restart=on-failure

[Install]
WantedBy=multi-user.target
EOF

systemctl daemon-reload
systemctl enable "$UNIT_NAME"

# DO NOT START - test import of stopped node
echo "Service created but NOT started (testing stopped node import)..."

# Verify service is not active
if systemctl is-active "$UNIT_NAME" 2>/dev/null; then
	echo "ERROR: Service should not be active before import test"
	exit 1
fi

# Verify network is detected from environment variables before import
echo "Verifying detection shows network from expanded environment variables..."
if ! om list --external 2>&1 | grep -A2 "$UNIT_NAME" | grep -q "shadownet"; then
	echo "ERROR: Network should be detected as shadownet from environment variables"
	om list --external 2>&1
	exit 1
fi

# Import WITHOUT --network flag - should read environment variables and expand them
echo "Importing stopped node (network should be detected from EnvironmentFile)..."
om import "$UNIT_NAME" --strategy takeover 2>&1

# Verify service is now managed
if ! service_is_managed "$INSTANCE"; then
	echo "ERROR: Service is not managed after import"
	om list 2>&1
	exit 1
fi

# Verify network was detected correctly
echo "Verifying network was detected from environment variables..."
if ! om show "$INSTANCE" 2>&1 | grep -q "shadownet"; then
	echo "ERROR: Network should be shadownet (from environment variables)"
	om show "$INSTANCE" 2>&1
	exit 1
fi

# Verify data directory was correctly resolved and preserved
echo "Verifying data directory was correctly resolved..."
if ! om show "$INSTANCE" 2>&1 | grep -q "$DATA_DIR"; then
	echo "ERROR: Data directory should be resolved to $DATA_DIR"
	om show "$INSTANCE" 2>&1
	exit 1
fi

# Verify original external service is disabled
if systemctl is-enabled "$UNIT_NAME" 2>/dev/null; then
	echo "ERROR: Original service should be disabled after takeover"
	systemctl status "$UNIT_NAME" --no-pager || true
	exit 1
fi

# Verify data directory contents were preserved
if [ ! -f "$DATA_DIR/config.json" ]; then
	echo "ERROR: config.json should be preserved in data directory"
	ls -la "$DATA_DIR" || true
	exit 1
fi

if [ ! -f "$DATA_DIR/identity.json" ]; then
	echo "ERROR: identity.json should be preserved in data directory"
	ls -la "$DATA_DIR" || true
	exit 1
fi

# Verify RPC address was correctly resolved
echo "Verifying RPC address was correctly resolved..."
if ! om show "$INSTANCE" 2>&1 | grep -q "$RPC_ADDR"; then
	echo "ERROR: RPC address should be resolved to $RPC_ADDR"
	om show "$INSTANCE" 2>&1
	exit 1
fi

echo "Stopped node with EnvironmentFile import test passed"
