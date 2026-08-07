#!/bin/bash
# Test: Group names with path traversal or invalid characters should be rejected
set -euo pipefail
source /tests/lib.sh

test_init "Group name validation"

# ── Path traversal attempts ──
echo "Testing path traversal in group name..."
if om group create "../evil" --network shadownet --app-bin-dir /usr/local/bin --service-user tezos 2>&1; then
	echo "ERROR: Should have rejected '../evil'"
	om group delete "../evil" 2>/dev/null || true
	exit 1
fi
echo "Correctly rejected: ../evil"

if om group create "foo/bar" --network shadownet --app-bin-dir /usr/local/bin --service-user tezos 2>&1; then
	echo "ERROR: Should have rejected 'foo/bar'"
	om group delete "foo/bar" 2>/dev/null || true
	exit 1
fi
echo "Correctly rejected: foo/bar"

# ── Empty name ──
if om group create "" --network shadownet --app-bin-dir /usr/local/bin --service-user tezos 2>&1; then
	echo "ERROR: Should have rejected empty name"
	exit 1
fi
echo "Correctly rejected: empty name"

# ── Valid names should work ──
VALID_NAME="test-valid-name_123-$$"
om group create "$VALID_NAME" --network shadownet --app-bin-dir /usr/local/bin --service-user tezos
echo "Accepted valid name: $VALID_NAME"

# Cleanup
om group delete "$VALID_NAME" 2>/dev/null || true

echo "Test passed"
