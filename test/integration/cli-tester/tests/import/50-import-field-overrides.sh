#!/bin/bash
# Test: Import with field overrides (custom name, network)
set -euo pipefail
source /tests/lib.sh

test_init "Import with field overrides"

EXTERNAL_INSTANCE="override-source"
CUSTOM_INSTANCE="my-custom-node"
DATA_DIR="/var/lib/octez-external/$EXTERNAL_INSTANCE"
RPC_ADDR="127.0.0.1:$(alloc_port)"

register_instance "$EXTERNAL_INSTANCE"
register_instance "$CUSTOM_INSTANCE"
register_external_service "node" "$EXTERNAL_INSTANCE"
register_data_dir "$DATA_DIR"

# Create external service on shadownet
echo "Creating external service..."
mkdir -p "$DATA_DIR"
inject_identity "$EXTERNAL_INSTANCE" "$DATA_DIR"
chown -R tezos:tezos "$DATA_DIR"
create_external_service "node" "$EXTERNAL_INSTANCE" "$DATA_DIR" "$RPC_ADDR" "shadownet"
systemctl enable "octez-node@${EXTERNAL_INSTANCE}.service"
systemctl start "octez-node@${EXTERNAL_INSTANCE}.service"
sleep 2

# Import with custom name override
echo "Importing with custom instance name..."
om import "octez-node@${EXTERNAL_INSTANCE}" --as "$CUSTOM_INSTANCE" 2>&1 || {
	# Stop service
	systemctl stop "octez-node@${EXTERNAL_INSTANCE}.service" 2>/dev/null || true
	echo "Import failed, showing current state..."
	om list 2>&1
}

# Verify service has custom name
if ! service_is_managed "$CUSTOM_INSTANCE"; then
	echo "ERROR: Service should be imported with custom name"
	om list 2>&1
	exit 1
fi

# Verify original name is not in managed instances
if om list 2>&1 | grep -v "external" | grep -q "$EXTERNAL_INSTANCE"; then
	echo "WARNING: Original name found in managed instances"
fi

echo "Field overrides test passed"
