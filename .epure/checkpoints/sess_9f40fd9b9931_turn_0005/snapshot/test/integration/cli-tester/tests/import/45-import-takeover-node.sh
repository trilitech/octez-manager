#!/bin/bash
# Test: Import with takeover strategy
set -euo pipefail
source /tests/lib.sh

test_init "Import node with takeover strategy"

INSTANCE="takeover-node"
DATA_DIR="/var/lib/octez-external/$INSTANCE"
RPC_ADDR="127.0.0.1:$(alloc_port)"

register_instance "$INSTANCE"
register_external_service "node" "$INSTANCE"
register_data_dir "$DATA_DIR"

# Create external service with pre-generated identity
echo "Creating external systemd service..."
mkdir -p "$DATA_DIR"
inject_identity "$INSTANCE" "$DATA_DIR"
chown -R tezos:tezos "$DATA_DIR"
create_external_service "node" "$INSTANCE" "$DATA_DIR" "$RPC_ADDR" "shadownet"
systemctl enable "octez-node@${INSTANCE}.service"

# Start service briefly so it can be detected
systemctl start "octez-node@${INSTANCE}.service"

# Wait for service to be detected (retries to handle concurrent daemon-reload)
echo "Waiting for external service detection..."
if ! wait_for_external_service "$INSTANCE"; then
	echo "DEBUG: Systemd unit status:"
	systemctl status "octez-node@${INSTANCE}.service" --no-pager || true
	echo "DEBUG: List of octez unit files:"
	systemctl list-unit-files "octez-*.service" --no-legend || true
	exit 1
fi

# Import with takeover strategy
echo "Importing with takeover strategy..."
om import "octez-node@${INSTANCE}" --strategy takeover 2>&1

# Stop the service immediately after import to avoid long sync
systemctl stop "octez-node@${INSTANCE}.service" 2>/dev/null || true

# Verify service is now managed
if ! service_is_managed "$INSTANCE"; then
	echo "ERROR: Service is not managed after import"
	om list 2>&1
	exit 1
fi

# Verify original external service is disabled
if ! external_service_disabled "node" "$INSTANCE"; then
	echo "ERROR: Original service should be disabled after takeover"
	systemctl status "octez-node@${INSTANCE}.service" || true
	exit 1
fi

# Verify data directory is preserved
if [ ! -d "$DATA_DIR" ]; then
	echo "ERROR: Data directory should be preserved"
	exit 1
fi

echo "Takeover import test passed"
