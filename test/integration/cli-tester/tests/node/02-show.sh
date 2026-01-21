#!/bin/bash
# Test: Show node instance details
set -euo pipefail
source /tests/lib.sh

echo "Test: Show node instance"

# Use unique instance name for this test
TEST_INSTANCE="test-show"

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
	--rpc-addr "127.0.0.1:18732" \
	--service-user tezos \
	--no-enable 2>&1 || true

# Verify instance exists
if ! instance_exists "$TEST_INSTANCE"; then
	echo "ERROR: Failed to create instance"
	exit 1
fi

# Show instance details
echo "Getting instance details..."
SHOW_OUTPUT=$(om_instance "$TEST_INSTANCE" show 2>&1)

# Verify key fields are present
assert_contains "$SHOW_OUTPUT" "$TEST_INSTANCE" "Should show instance name"
assert_contains "$SHOW_OUTPUT" "node" "Should show role"
assert_contains "$SHOW_OUTPUT" "shadownet" "Should show network"

# Cleanup
cleanup_instance "$TEST_INSTANCE" || true

echo "Show instance test passed"
