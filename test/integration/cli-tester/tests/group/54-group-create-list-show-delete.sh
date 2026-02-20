#!/bin/bash
# Test: Group CRUD operations (create, list, show, delete)
set -euo pipefail
source /tests/lib.sh

test_init "Group create, list, show, delete"

GROUP_NAME="test-group-crud-$$"

# Pre-cleanup
om group delete "$GROUP_NAME" 2>/dev/null || true

# ── Create ──
echo "Creating group '$GROUP_NAME'..."
om group create "$GROUP_NAME" \
	--network shadownet \
	--app-bin-dir /usr/local/bin \
	--service-user tezos

echo "Group created successfully"

# ── List ──
echo "Listing groups..."
OUTPUT=$(om group list)
assert_contains "$OUTPUT" "$GROUP_NAME" "Group should appear in list"
echo "Group found in list"

# ── Show ──
echo "Showing group '$GROUP_NAME'..."
OUTPUT=$(om group show "$GROUP_NAME")
assert_contains "$OUTPUT" "$GROUP_NAME" "Show should display group name"
assert_contains "$OUTPUT" "shadownet" "Show should display network"
echo "Group show displays correct info"

# ── Show JSON ──
echo "Showing group as JSON..."
OUTPUT=$(om group show --json "$GROUP_NAME")
assert_contains "$OUTPUT" "\"name\"" "JSON should have name field"
assert_contains "$OUTPUT" "shadownet" "JSON should have network"
echo "Group JSON output correct"

# ── Delete ──
echo "Deleting group '$GROUP_NAME'..."
om group delete "$GROUP_NAME"
echo "Group deleted"

# Verify it's gone
OUTPUT=$(om group list 2>&1)
if echo "$OUTPUT" | grep -q "$GROUP_NAME"; then
	echo "ERROR: Group still in list after delete"
	exit 1
fi
echo "Group no longer in list after delete"

echo "Test passed"
