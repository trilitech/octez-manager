#!/bin/bash
# Test: Binary accessibility validation for service user
# Verifies that octez-manager validates service user can access binaries
set -euo pipefail
source /tests/lib.sh

test_init "Binary accessibility validation"

INSTANCE="test-bin-access"
RESTRICTED_DIR="/tmp/restricted-bin"

# Register instance and data dir for automatic cleanup (also pre-cleans leftovers)
register_instance "$INSTANCE"
register_data_dir "$RESTRICTED_DIR"

# Test 1: Binaries in /usr/local/bin should work (accessible to all users)
echo "Test 1: Installing with accessible binaries (/usr/local/bin)"
om install-node \
	--instance "$INSTANCE" \
	--network shadownet \
	--service-user tezos \
	--app-bin-dir /usr/local/bin \
	--no-enable 2>&1

if ! instance_exists "$INSTANCE"; then
	echo "ERROR: Installation with accessible binaries failed"
	exit 1
fi

echo "✓ Installation succeeded with accessible binaries"
cleanup_instance "$INSTANCE"

# Test 2: Create a restricted directory in /tmp (writable location)
echo "Test 2: Testing restricted directory (should fail validation)"
mkdir -p "$RESTRICTED_DIR"
cp /usr/local/bin/octez-node "$RESTRICTED_DIR/"
chmod 755 "$RESTRICTED_DIR/octez-node"
chmod 700 "$RESTRICTED_DIR" # Owner (root) only

# This should fail because tezos user cannot access the directory
if om install-node \
	--instance "$INSTANCE" \
	--network shadownet \
	--service-user tezos \
	--app-bin-dir "$RESTRICTED_DIR" \
	--no-enable 2>&1; then
	echo "ERROR: Installation should have failed with restricted directory"
	cleanup_instance "$INSTANCE" || true
	exit 1
fi

echo "✓ Installation correctly rejected restricted directory"

# Test 3: Make the directory accessible and verify it works
echo "Test 3: Making directory accessible should allow installation"
chmod 755 "$RESTRICTED_DIR" # Now tezos user can traverse

om install-node \
	--instance "$INSTANCE" \
	--network shadownet \
	--service-user tezos \
	--app-bin-dir "$RESTRICTED_DIR" \
	--no-enable 2>&1

if ! instance_exists "$INSTANCE"; then
	echo "ERROR: Installation failed after making directory accessible"
	exit 1
fi

echo "✓ Installation succeeded after fixing permissions"

echo "Binary accessibility validation test passed"
