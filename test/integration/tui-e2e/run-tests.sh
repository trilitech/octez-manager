#!/bin/bash
set -euo pipefail

SHARD="${SHARD:-1}"
TOTAL_SHARDS="${TOTAL_SHARDS:-3}"
SHARDS_FILE="/tests/shards.json"

echo "=========================================="
echo "TUI E2E Tests - Shard $SHARD/$TOTAL_SHARDS"
echo "=========================================="

# Verify shards.json exists
if [ ! -f "$SHARDS_FILE" ]; then
	echo "ERROR: shards.json not found at $SHARDS_FILE"
	exit 1
fi

# Load shard configuration
if ! SHARD_TESTS=$(jq -r ".shards.\"$SHARD\".tests[]" "$SHARDS_FILE" 2>/dev/null); then
	echo "ERROR: Failed to read shard $SHARD from shards.json"
	jq . "$SHARDS_FILE" || echo "Invalid JSON"
	exit 1
fi

# Check if shard has any tests
if [ -z "$SHARD_TESTS" ]; then
	echo "Shard $SHARD has no tests, exiting successfully"
	exit 0
fi

# Setup coverage
export BISECT_FILE="/coverage/bisect-tui-shard-$SHARD"
mkdir -p /coverage

echo ""
echo "Tests for shard $SHARD:"
echo "$SHARD_TESTS" | sed 's/^/  - /'
echo ""

# Test counters
PASSED=0
FAILED=0
FAILED_TESTS=""

# Run each test
for test in $SHARD_TESTS; do
	TEST_PATH="/tests/$test"

	if [ ! -f "$TEST_PATH" ]; then
		echo ""
		echo "=========================================="
		echo "ERROR: Test file not found: $TEST_PATH"
		echo "=========================================="
		FAILED=$((FAILED + 1))
		FAILED_TESTS="$FAILED_TESTS $test"
		continue
	fi

	echo ""
	echo "=========================================="
	echo "Running: $test"
	echo "=========================================="

	if bash "$TEST_PATH"; then
		echo ""
		echo "✓ PASSED: $test"
		PASSED=$((PASSED + 1))
	else
		EXIT_CODE=$?
		echo ""
		echo "✗ FAILED: $test (exit code: $EXIT_CODE)"
		FAILED=$((FAILED + 1))
		FAILED_TESTS="$FAILED_TESTS $test"
	fi
done

# Summary
echo ""
echo "=========================================="
echo "Shard $SHARD/$TOTAL_SHARDS Results:"
echo "  Passed: $PASSED"
echo "  Failed: $FAILED"
if [ $FAILED -gt 0 ]; then
	echo "  Failed tests:$FAILED_TESTS"
fi
echo "=========================================="

# Coverage files
echo ""
echo "Coverage files generated:"
ls -lh /coverage/ 2>/dev/null || echo "  (no coverage files)"

# Exit with failure if any tests failed
if [ $FAILED -gt 0 ]; then
	exit 1
fi

exit 0
