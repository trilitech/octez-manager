#!/bin/bash
# Test: Import to conflicting instance name shows error
set -euo pipefail
source /tests/lib.sh

test_init "Import with instance name conflict"

MANAGED_INSTANCE="conflict-test"
EXTERNAL_INSTANCE="external-conflict"
EXTERNAL_DATA="/var/lib/octez-external/$EXTERNAL_INSTANCE"
MANAGED_RPC="127.0.0.1:$(alloc_port)"
EXTERNAL_RPC="127.0.0.1:$(alloc_port)"

register_instance "$MANAGED_INSTANCE"
register_instance "$EXTERNAL_INSTANCE"
register_external_service "node" "$EXTERNAL_INSTANCE"
register_data_dir "$EXTERNAL_DATA"

# Create a managed instance
echo "Creating managed instance..."
om install-node \
	--instance "$MANAGED_INSTANCE" \
	--network shadownet \
	--rpc-addr "$MANAGED_RPC" \
	--service-user tezos \
	--no-enable 2>&1

# Create external service
echo "Creating external service..."
create_external_service "node" "$EXTERNAL_INSTANCE" "$EXTERNAL_DATA" "$EXTERNAL_RPC" "shadownet"
systemctl enable "octez-node@${EXTERNAL_INSTANCE}.service"

# Try to import with same name as managed instance
echo "Attempting to import with conflicting name..."
if om import "octez-node@${EXTERNAL_INSTANCE}" --as "$MANAGED_INSTANCE" 2>&1 | tee /tmp/conflict_error.txt; then
	echo "ERROR: Import should fail when instance name conflicts"
	exit 1
fi

# Verify error message mentions conflict
if ! grep -qi "already exists\|conflict\|in use" /tmp/conflict_error.txt; then
	echo "ERROR: Error message should mention name conflict"
	cat /tmp/conflict_error.txt
	exit 1
fi

echo "Name conflict test passed"
