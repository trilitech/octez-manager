#!/bin/bash
# Test: Import to conflicting instance name shows error
set -euo pipefail
source /tests/lib.sh

MANAGED_INSTANCE="conflict-test"
EXTERNAL_INSTANCE="external-conflict"
EXTERNAL_DATA="/var/lib/octez-external/$EXTERNAL_INSTANCE"
RPC_ADDR="127.0.0.1:18752"

echo "Test: Import with instance name conflict"

# Cleanup
cleanup_instance "$MANAGED_INSTANCE" || true
cleanup_instance "$EXTERNAL_INSTANCE" || true
rm -rf "$EXTERNAL_DATA" || true
systemctl stop "octez-node@${EXTERNAL_INSTANCE}.service" 2>/dev/null || true
systemctl disable "octez-node@${EXTERNAL_INSTANCE}.service" 2>/dev/null || true
rm -f "/etc/systemd/system/octez-node@${EXTERNAL_INSTANCE}.service" || true
systemctl daemon-reload

# Create a managed instance
echo "Creating managed instance..."
om install-node \
    --instance "$MANAGED_INSTANCE" \
    --network shadownet \
    --rpc-addr "127.0.0.1:18751" \
    --service-user tezos \
    --no-enable 2>&1

# Create external service
echo "Creating external service..."
create_external_service "node" "$EXTERNAL_INSTANCE" "$EXTERNAL_DATA" "$RPC_ADDR" "shadownet"
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

echo "Name conflict handling passed"

# Cleanup
cleanup_instance "$MANAGED_INSTANCE"
systemctl disable "octez-node@${EXTERNAL_INSTANCE}.service" || true
rm -f "/etc/systemd/system/octez-node@${EXTERNAL_INSTANCE}.service" || true
systemctl daemon-reload
rm -rf "$EXTERNAL_DATA"

echo "Name conflict test passed"
