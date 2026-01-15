#!/bin/bash
# Test: Binary accessibility validation for service user
# Verifies that octez-manager validates service user can access binaries
set -euo pipefail
source /tests/lib.sh

INSTANCE="test-bin-access"

echo "Test: Binary accessibility validation"

# Cleanup from previous runs
cleanup_instance "$INSTANCE" || true

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

# Test 2: Create a restricted directory and copy binaries there
echo "Test 2: Testing restricted directory (should fail validation)"
RESTRICTED_DIR="/root/restricted-bin"
mkdir -p "$RESTRICTED_DIR"
cp /usr/local/bin/octez-node "$RESTRICTED_DIR/"
chmod 755 "$RESTRICTED_DIR/octez-node"
chmod 700 "$RESTRICTED_DIR"  # Owner (root) only

# This should fail because tezos user cannot access /root/restricted-bin
if om install-node \
    --instance "$INSTANCE" \
    --network shadownet \
    --service-user tezos \
    --app-bin-dir "$RESTRICTED_DIR" \
    --no-enable 2>&1; then
    echo "ERROR: Installation should have failed with restricted directory"
    cleanup_instance "$INSTANCE" || true
    rm -rf "$RESTRICTED_DIR"
    exit 1
fi

echo "✓ Installation correctly rejected restricted directory"

# Test 3: Make the directory accessible and verify it works
echo "Test 3: Making directory accessible should allow installation"
chmod 755 "$RESTRICTED_DIR"  # Now tezos user can traverse

om install-node \
    --instance "$INSTANCE" \
    --network shadownet \
    --service-user tezos \
    --app-bin-dir "$RESTRICTED_DIR" \
    --no-enable 2>&1

if ! instance_exists "$INSTANCE"; then
    echo "ERROR: Installation failed after making directory accessible"
    rm -rf "$RESTRICTED_DIR"
    exit 1
fi

echo "✓ Installation succeeded after fixing permissions"

# Cleanup
cleanup_instance "$INSTANCE"
rm -rf "$RESTRICTED_DIR"

echo "Binary accessibility validation test passed"
