#!/bin/bash
# Selftest: Verify all integration tests are registered in shards.json
#
# This script ensures that every test file in test/integration/cli-tester/tests/
# is registered in the shards.json manifest. Tests not in the manifest will not
# run in CI, leading to silent test coverage gaps.
#
# Usage:
#   ./selftest-shard-registration.sh
#
# Exit codes:
#   0 - All tests are registered
#   1 - Some tests are not registered (prints list to stderr)

set -euo pipefail

# Colors for output
RED='\033[0;31m'
GREEN='\033[0;32m'
YELLOW='\033[1;33m'
NC='\033[0m' # No Color

# Determine script directory
SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
TESTS_DIR="$SCRIPT_DIR/tests"
SHARD_MANIFEST="$TESTS_DIR/shards.json"

# Check dependencies
if ! command -v jq >/dev/null 2>&1; then
	echo -e "${RED}ERROR${NC}: jq is required but not installed" >&2
	exit 1
fi

if [ ! -f "$SHARD_MANIFEST" ]; then
	echo -e "${RED}ERROR${NC}: Shard manifest not found: $SHARD_MANIFEST" >&2
	exit 1
fi

if [ ! -d "$TESTS_DIR" ]; then
	echo -e "${RED}ERROR${NC}: Tests directory not found: $TESTS_DIR" >&2
	exit 1
fi

echo "Checking shard registration..."
echo "Tests directory: $TESTS_DIR"
echo "Shard manifest: $SHARD_MANIFEST"
echo ""

# Discover all test files (excluding lib.sh)
# Sort to ensure consistent ordering
ALL_TESTS=$(find "$TESTS_DIR" -name "*.sh" -type f ! -name "lib.sh" |
	sed "s|^$TESTS_DIR/||" |
	sort)

TOTAL_TEST_FILES=$(echo "$ALL_TESTS" | wc -l)

# Extract all tests registered in shards.json
# The manifest has structure: { "shard-1": { "tests": [...] }, "shard-2": { "tests": [...] }, ... }
REGISTERED_TESTS=$(jq -r '
	[.["shard-1", "shard-2", "shard-3", "shard-4", "shard-5"] | .tests[]]
	| unique
	| .[]
' "$SHARD_MANIFEST" | sort)

TOTAL_REGISTERED=$(echo "$REGISTERED_TESTS" | wc -l)

# Find tests that exist in filesystem but not in manifest
UNREGISTERED_TESTS=$(comm -23 <(echo "$ALL_TESTS") <(echo "$REGISTERED_TESTS"))
if [ -z "$UNREGISTERED_TESTS" ]; then
	UNREGISTERED_COUNT=0
else
	UNREGISTERED_COUNT=$(echo "$UNREGISTERED_TESTS" | wc -l)
fi

# Find tests that are in manifest but don't exist in filesystem (dead entries)
DEAD_ENTRIES=$(comm -13 <(echo "$ALL_TESTS") <(echo "$REGISTERED_TESTS"))
if [ -z "$DEAD_ENTRIES" ]; then
	DEAD_COUNT=0
else
	DEAD_COUNT=$(echo "$DEAD_ENTRIES" | wc -l)
fi

# Print summary
echo "Summary:"
echo "  Total test files found: $TOTAL_TEST_FILES"
echo "  Tests registered in shards: $TOTAL_REGISTERED"
echo "  Tests not registered: $UNREGISTERED_COUNT"
echo "  Dead entries in manifest: $DEAD_COUNT"
echo ""

# Check for problems
FAILED=0

if [ "$UNREGISTERED_COUNT" -gt 0 ]; then
	FAILED=1
	echo -e "${RED}❌ FAIL${NC}: $UNREGISTERED_COUNT test(s) not registered in shards.json" >&2
	echo "" >&2
	echo "The following tests are not registered and will NOT run in CI:" >&2
	echo "" >&2
	echo "$UNREGISTERED_TESTS" | sed 's/^/  - /' >&2
	echo "" >&2
	echo "To fix this issue:" >&2
	echo "  1. Add these tests to one of the shards in $SHARD_MANIFEST" >&2
	echo "  2. Balance tests across shards to keep similar durations" >&2
	echo "  3. Run this selftest again to verify registration" >&2
	echo "" >&2
fi

if [ "$DEAD_COUNT" -gt 0 ]; then
	FAILED=1
	echo -e "${YELLOW}⚠️  WARNING${NC}: $DEAD_COUNT dead entry(ies) in shards.json" >&2
	echo "" >&2
	echo "The following tests are registered but don't exist:" >&2
	echo "" >&2
	echo "$DEAD_ENTRIES" | sed 's/^/  - /' >&2
	echo "" >&2
	echo "These should be removed from $SHARD_MANIFEST" >&2
	echo "" >&2
fi

if [ "$FAILED" -eq 0 ]; then
	echo -e "${GREEN}✓ PASS${NC}: All tests are registered in shards.json"
	exit 0
else
	exit 1
fi
