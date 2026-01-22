#!/bin/bash
# Integration test runner
set -euo pipefail

TESTS_DIR="/tests"
SANDBOX_URL="${SANDBOX_URL:-http://sandbox:8080}"
NODE_RPC="${NODE_RPC:-http://sandbox:8732}"

# Colors
RED='\033[0;31m'
GREEN='\033[0;32m'
YELLOW='\033[1;33m'
NC='\033[0m' # No Color

log() { echo -e "[runner] $*"; }
pass() { echo -e "${GREEN}PASS${NC}: $1"; }
fail() { echo -e "${RED}FAIL${NC}: $1"; }

# Source test library
source "$TESTS_DIR/lib.sh"

# Wait for sandbox to be ready
wait_for_sandbox() {
	log "Waiting for sandbox at $SANDBOX_URL..."
	local max_attempts=60
	local attempt=0

	while [ $attempt -lt $max_attempts ]; do
		if curl -sf "$SANDBOX_URL/health" | jq -e '.status == "ready"' >/dev/null 2>&1; then
			log "Sandbox is ready"
			return 0
		fi
		attempt=$((attempt + 1))
		sleep 2
	done

	fail "Sandbox did not become ready"
	return 1
}

# Run a single test
run_test() {
	local test_file="$1"
	local test_name=$(basename "$test_file" .sh)

	log "Running test: $test_name"

	# Record start time for duration tracking
	local start_time=$(date +%s.%N 2>/dev/null || date +%s)

	if bash -e "$test_file"; then
		# Calculate duration
		local end_time=$(date +%s.%N 2>/dev/null || date +%s)
		local duration=$(echo "$end_time - $start_time" | bc 2>/dev/null || echo "0")

		# Log timing for manifest generation
		echo "TIMING: $test_file $duration" >>/tmp/test-timings.log

		pass "$test_name (${duration}s)"
		return 0
	else
		fail "$test_name"
		return 1
	fi
}

# Run tests with shard support
run_tests() {
	local mode="${1:-}"
	local shard_id="${2:-}"
	local failed=0
	local passed=0
	local tests=()
	local shard_manifest="$TESTS_DIR/shards.json"

	# Determine which tests to run based on mode
	if [ "$mode" == "--shard" ]; then
		# Shard mode: load tests from manifest
		if [ ! -f "$shard_manifest" ]; then
			fail "Shard manifest not found: $shard_manifest"
			return 1
		fi

		log "Running shard $shard_id"

		# Extract test list from JSON manifest
		# Convert JSON array to bash array
		while IFS= read -r test_path; do
			tests+=("$TESTS_DIR/$test_path")
		done < <(jq -r ".\"shard-${shard_id}\".tests[]" "$shard_manifest" 2>/dev/null)

		if [ ${#tests[@]} -eq 0 ]; then
			fail "No tests found for shard $shard_id"
			return 1
		fi

		log "Shard $shard_id contains ${#tests[@]} tests"
	elif [ "$mode" == "all" ] || [ -z "$mode" ]; then
		# All mode: run all tests (for local dev)
		log "Running all test categories"
		for category in node dal baker accuser import binaries; do
			if [ -d "$TESTS_DIR/$category" ]; then
				for test in $(ls "$TESTS_DIR/$category"/*.sh 2>/dev/null | sort); do
					tests+=("$test")
				done
			fi
		done
		log "Found ${#tests[@]} tests to run"
	else
		# Legacy category mode: run specific category
		log "Running tests for category: $mode"
		if [ -d "$TESTS_DIR/$mode" ]; then
			for test in $(ls "$TESTS_DIR/$mode"/*.sh 2>/dev/null | sort); do
				tests+=("$test")
			done
		fi
		log "Found ${#tests[@]} tests in category $mode"
	fi

	# Run all collected tests
	for test in "${tests[@]}"; do
		if run_test "$test"; then
			passed=$((passed + 1))
		else
			failed=$((failed + 1))
		fi
	done

	echo ""
	log "Results: ${GREEN}$passed passed${NC}, ${RED}$failed failed${NC}"

	if [ $failed -gt 0 ]; then
		return 1
	fi
	return 0
}

# Main
main() {
	local mode="${1:-}"
	local shard_id="${2:-}"

	log "Starting integration tests..."
	log "SANDBOX_URL=$SANDBOX_URL"
	log "NODE_RPC=$NODE_RPC"

	if [ "$mode" == "--shard" ]; then
		log "Mode: Shard-based execution (shard $shard_id)"
	elif [ -n "$mode" ]; then
		log "Mode: Category filter ($mode)"
	else
		log "Mode: All tests"
	fi

	# Export for tests
	export SANDBOX_URL NODE_RPC
	export OCTEZ_BIN_DIR="/opt/octez"
	export TEST_INSTANCE="test-node"

	wait_for_sandbox

	if [ "$mode" == "--shard" ]; then
		run_tests --shard "$shard_id"
	else
		run_tests "$mode"
	fi
}

main "$@"
