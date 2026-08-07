#!/bin/bash
# Test: Multiple groups on the same network can coexist
set -euo pipefail
source /tests/lib.sh

test_init "Multiple groups on same network"

GROUP_A="test-group-a-$$"
GROUP_B="test-group-b-$$"

# Pre-cleanup
om group delete "$GROUP_A" 2>/dev/null || true
om group delete "$GROUP_B" 2>/dev/null || true

# ── Create two groups on same network ──
echo "Creating group '$GROUP_A' (shadownet)..."
om group create "$GROUP_A" \
	--network shadownet \
	--app-bin-dir /usr/local/bin \
	--service-user tezos

echo "Creating group '$GROUP_B' (shadownet)..."
om group create "$GROUP_B" \
	--network shadownet \
	--app-bin-dir /usr/local/bin \
	--service-user tezos

# ── Both should appear in list ──
OUTPUT=$(om group list)
assert_contains "$OUTPUT" "$GROUP_A" "Group A should appear in list"
assert_contains "$OUTPUT" "$GROUP_B" "Group B should appear in list"
echo "Both groups appear in list"

# ── Show each independently ──
OUTPUT_A=$(om group show "$GROUP_A")
OUTPUT_B=$(om group show "$GROUP_B")
assert_contains "$OUTPUT_A" "$GROUP_A" "Show A should display group A"
assert_contains "$OUTPUT_B" "$GROUP_B" "Show B should display group B"
echo "Each group shows independently"

# ── Delete one, other survives ──
echo "Deleting group '$GROUP_A'..."
om group delete "$GROUP_A"

OUTPUT=$(om group list)
if echo "$OUTPUT" | grep -q "$GROUP_A"; then
	echo "ERROR: Deleted group A still in list"
	exit 1
fi
assert_contains "$OUTPUT" "$GROUP_B" "Group B should survive after deleting A"
echo "Group B survives after deleting A"

# ── Cleanup ──
om group delete "$GROUP_B" 2>/dev/null || true

echo "Test passed"
