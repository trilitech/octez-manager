#!/bin/bash
# Test: Remove node instance
set -euo pipefail
source /tests/lib.sh

echo "Test: Remove node instance"

# Use unique instance name for this test
TEST_INSTANCE="test-remove"

# Cleanup any previous state
cleanup_instance "$TEST_INSTANCE" || true

# Install node for this test
echo "Installing node '$TEST_INSTANCE'..."
om install-node \
	--instance "$TEST_INSTANCE" \
	--network shadownet \
	--snapshot \
	--snapshot-no-check \
	--snapshot-uri "$SANDBOX_URL/snapshot.rolling" \
	--rpc-addr "127.0.0.1:18733" \
	--service-user tezos \
	--no-enable 2>&1 || true

# Verify instance exists
if ! instance_exists "$TEST_INSTANCE"; then
	echo "ERROR: Failed to create instance"
	exit 1
fi

# Remove instance
echo "Removing instance..."
om_instance "$TEST_INSTANCE" remove

# Verify instance is not in list anymore
if instance_exists "$TEST_INSTANCE"; then
	echo "ERROR: Instance still exists after remove"
	exit 1
fi

echo "Remove instance test passed"
