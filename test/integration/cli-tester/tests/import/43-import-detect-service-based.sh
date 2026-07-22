#!/bin/bash
# Test: Detect service-based external instances
set -euo pipefail
source /tests/lib.sh

test_init "Detect service-based external Octez services"

INSTANCE="external-node-detect"
DATA_DIR="/var/lib/octez-external/$INSTANCE"
RPC_ADDR="127.0.0.1:$(alloc_port)"

register_instance "$INSTANCE"
register_external_service "node" "$INSTANCE"
register_data_dir "$DATA_DIR"

# Create external systemd service
echo "Creating external systemd service..."
create_external_service "node" "$INSTANCE" "$DATA_DIR" "$RPC_ADDR" "shadownet"

# Enable and start it
systemctl enable "octez-node@${INSTANCE}.service"
systemctl start "octez-node@${INSTANCE}.service"

# Wait for it to be detected (retries to handle concurrent daemon-reload)
echo "Checking if external service is detected..."
if ! wait_for_external_service "$INSTANCE"; then
	om list --external 2>&1
	exit 1
fi

echo "External service detected correctly"

# Verify it shows up with 'external' marker when using --all or --external
if ! om list --external 2>&1 | grep "$INSTANCE" >/dev/null; then
	echo "ERROR: External service not marked as external"
	om list --external 2>&1
	exit 1
fi

echo "Detect service-based test passed"
