#!/bin/bash
# Test: Import stopped node without --network flag (reads from config.json)
set -euo pipefail
source /tests/lib.sh

test_init "Import stopped node via config.json"

INSTANCE="stopped-node"
DATA_DIR="/var/lib/octez-external/$INSTANCE"
RPC_ADDR="127.0.0.1:$(alloc_port)"
NET_ADDR="0.0.0.0:$(alloc_port)"

register_instance "$INSTANCE"
register_external_service "node" "$INSTANCE"
register_data_dir "$DATA_DIR"

# Create external service data directory
echo "Creating external node data directory..."
mkdir -p "$DATA_DIR"
inject_identity "$INSTANCE" "$DATA_DIR"

# Create config.json with network information (this is what octez-node config init creates)
echo "Creating config.json with network information..."
cat >"$DATA_DIR/config.json" <<EOF
{
  "data-dir": "$DATA_DIR",
  "rpc": {
    "listen-addrs": ["$RPC_ADDR"]
  },
  "p2p": {
    "listen-addr": "$NET_ADDR",
    "bootstrap-peers": ["shadownet.teztnets.com", "shadownet.tzinit.org"]
  },
  "shell": {
    "history_mode": "rolling"
  },
  "network": "shadownet"
}
EOF

chown -R tezos:tezos "$DATA_DIR"

# Create external systemd service unit
echo "Creating external systemd service..."
create_external_service "node" "$INSTANCE" "$DATA_DIR" "$RPC_ADDR" "shadownet"
systemctl enable "octez-node@${INSTANCE}.service"

# DO NOT START - test import of stopped node
echo "Service created but NOT started (testing stopped node import)..."
systemctl status "octez-node@${INSTANCE}.service" --no-pager || true

# Verify service is not active
if systemctl is-active "octez-node@${INSTANCE}.service" 2>/dev/null; then
	echo "ERROR: Service should not be active before import test"
	exit 1
fi

# Import WITHOUT --network flag - should read from config.json
echo "Importing stopped node (network should be detected from config.json)..."
om import "octez-node@${INSTANCE}" --strategy takeover 2>&1

# Verify service is now managed
if ! service_is_managed "$INSTANCE"; then
	echo "ERROR: Service is not managed after import"
	om list 2>&1
	exit 1
fi

# Verify network was detected correctly from config.json
echo "Verifying network was detected from config.json..."
if ! om show "$INSTANCE" 2>&1 | grep -q "shadownet"; then
	echo "ERROR: Network should be shadownet (from config.json)"
	om show "$INSTANCE" 2>&1
	exit 1
fi

# Verify original external service is disabled
if ! external_service_disabled "node" "$INSTANCE"; then
	echo "ERROR: Original service should be disabled after takeover"
	systemctl status "octez-node@${INSTANCE}.service" || true
	exit 1
fi

# Verify data directory was preserved
if [ ! -f "$DATA_DIR/config.json" ]; then
	echo "ERROR: config.json should be preserved in data directory"
	ls -la "$DATA_DIR" || true
	exit 1
fi

# Verify identity was preserved
if [ ! -f "$DATA_DIR/identity.json" ]; then
	echo "ERROR: identity.json should be preserved in data directory"
	ls -la "$DATA_DIR" || true
	exit 1
fi

echo "Stopped node import test passed"
