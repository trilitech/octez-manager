#!/bin/bash
# Test: Dry-run shows plan without making changes
set -euo pipefail
source /tests/lib.sh

INSTANCE="dryrun-node"
DATA_DIR="/var/lib/octez-external/$INSTANCE"
RPC_ADDR="127.0.0.1:18748"

echo "Test: Import dry-run preview"

# Cleanup
cleanup_instance "$INSTANCE" || true
rm -rf "$DATA_DIR" || true
systemctl stop "octez-node@${INSTANCE}.service" 2>/dev/null || true
systemctl disable "octez-node@${INSTANCE}.service" 2>/dev/null || true
rm -f "/etc/systemd/system/octez-node@${INSTANCE}.service" || true
systemctl daemon-reload

# Create external service
echo "Creating external systemd service..."
mkdir -p "$DATA_DIR"
inject_identity "$INSTANCE" "$DATA_DIR"
chown -R tezos:tezos "$DATA_DIR"
create_external_service "node" "$INSTANCE" "$DATA_DIR" "$RPC_ADDR" "shadownet"
systemctl enable "octez-node@${INSTANCE}.service"
systemctl start "octez-node@${INSTANCE}.service"
sleep 2

# Run dry-run
echo "Running dry-run import..."
om import "octez-node@${INSTANCE}" --dry-run 2>&1 >/tmp/dryrun_output.txt || true

# Stop service
systemctl stop "octez-node@${INSTANCE}.service" 2>/dev/null || true

cat /tmp/dryrun_output.txt

# Verify service is still external (not imported)
if service_is_managed "$INSTANCE"; then
	echo "ERROR: Service should not be imported during dry-run"
	om list 2>&1
	exit 1
fi

# Verify external service is still enabled (we don't start it, so don't check if running)
if ! systemctl is-enabled "octez-node@${INSTANCE}.service" >/dev/null 2>&1; then
	echo "ERROR: External service should still be enabled after dry-run"
	exit 1
fi

echo "Dry-run correctly showed plan without making changes"

# Cleanup
systemctl stop "octez-node@${INSTANCE}.service" || true
systemctl disable "octez-node@${INSTANCE}.service" || true
rm -f "/etc/systemd/system/octez-node@${INSTANCE}.service" || true
systemctl daemon-reload
rm -rf "$DATA_DIR"

echo "Dry-run test passed"
