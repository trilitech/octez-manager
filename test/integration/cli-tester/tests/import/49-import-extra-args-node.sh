#!/bin/bash
# Test: Import node with custom flags that become extra_args
set -euo pipefail
source /tests/lib.sh

INSTANCE="extraargs-node"
DATA_DIR="/var/lib/octez-external/$INSTANCE"
RPC_ADDR="127.0.0.1:18749"

echo "Test: Import node with extra args"

# Cleanup
cleanup_instance "$INSTANCE" || true
rm -rf "$DATA_DIR" || true
systemctl stop "octez-node@${INSTANCE}.service" 2>/dev/null || true
systemctl disable "octez-node@${INSTANCE}.service" 2>/dev/null || true
rm -f "/etc/systemd/system/octez-node@${INSTANCE}.service" || true
systemctl daemon-reload

# Create external service with custom flags
echo "Creating external service with extra args..."
mkdir -p "$DATA_DIR"
inject_identity "$INSTANCE" "$DATA_DIR"
chown -R tezos:tezos "$DATA_DIR"

cat >"/etc/systemd/system/octez-node@${INSTANCE}.service" <<SERVICE
[Unit]
Description=External Octez Node with Extra Args
After=network.target

[Service]
Type=simple
User=tezos
ExecStart=/usr/local/bin/octez-node run \
  --data-dir $DATA_DIR \
  --network shadownet \
  --rpc-addr $RPC_ADDR \
  --connections 100 \
  --synchronisation-threshold 4
Restart=on-failure
RestartSec=5

[Install]
WantedBy=multi-user.target
SERVICE

systemctl daemon-reload
systemctl start "octez-node@${INSTANCE}.service"
sleep 2
systemctl enable "octez-node@${INSTANCE}.service"

# Import the service
echo "Importing service with extra args..."
om import "octez-node@${INSTANCE}" --strategy takeover 2>&1 || {
	# Stop service
	systemctl stop "octez-node@${INSTANCE}.service" 2>/dev/null || true
	echo "Import failed, showing current state..."
	om list 2>&1
}

# Verify service is managed
if ! service_is_managed "$INSTANCE"; then
	echo "ERROR: Service should be managed after import"
	om list 2>&1
	exit 1
fi

# TODO: Verify extra_args are preserved in metadata
# This would require checking the service JSON or testing that the service
# can be started with all its original flags

echo "Extra args import test completed"

# Cleanup
cleanup_instance "$INSTANCE"
rm -rf "$DATA_DIR"

echo "Extra args node test passed"
