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
		for category in node dal baker accuser import binaries group headless signatory; do
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

	# Check if parallel execution is available and enabled
	local parallel_jobs="${PARALLEL_JOBS:-0}"

	if [ "$parallel_jobs" -gt 1 ] && command -v parallel >/dev/null 2>&1; then
		log "Running tests in parallel (jobs: $parallel_jobs)"
		run_tests_parallel "${tests[@]}"
		return $?
	else
		# Run tests sequentially (default behavior)
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
	fi
}

# Check if a test should run serially (not in parallel with others).
# Serial tests are listed in serial-tests.json and run before the
# parallel batch to avoid shared-state interference.
is_serial_test() {
	local test_file="$1"
	local serial_manifest="$TESTS_DIR/serial-tests.json"

	if [ ! -f "$serial_manifest" ]; then
		return 1
	fi

	# Get the relative path (strip TESTS_DIR prefix)
	local rel_path="${test_file#$TESTS_DIR/}"
	jq -e --arg t "$rel_path" 'any(. == $t)' "$serial_manifest" >/dev/null 2>&1
}

# Run tests in parallel using GNU parallel, with serial tests first
run_tests_parallel() {
	local tests=("$@")
	local passed=0
	local failed=0
	local serial_tests=()
	local parallel_tests=()

	# Split into serial and parallel groups
	for test in "${tests[@]}"; do
		if is_serial_test "$test"; then
			serial_tests+=("$test")
		else
			parallel_tests+=("$test")
		fi
	done

	# Run serial tests first (one at a time, no parallelism)
	if [ ${#serial_tests[@]} -gt 0 ]; then
		log "Running ${#serial_tests[@]} serial test(s) before parallel batch..."
		for test in "${serial_tests[@]}"; do
			if run_test "$test"; then
				passed=$((passed + 1))
			else
				failed=$((failed + 1))
			fi
		done
	fi

	# Run remaining tests in parallel
	if [ ${#parallel_tests[@]} -gt 0 ]; then
		log "Running ${#parallel_tests[@]} tests in parallel (jobs: $PARALLEL_JOBS)..."

		local results_dir="/tmp/test-results-$$"
		mkdir -p "$results_dir"

		# Export functions and variables needed by parallel jobs
		export -f run_test pass fail log
		export RED GREEN YELLOW NC SANDBOX_URL NODE_RPC TESTS_DIR

		# Run tests in parallel, capturing exit codes
		printf '%s\n' "${parallel_tests[@]}" | parallel \
			--jobs "$PARALLEL_JOBS" \
			--halt soon,fail=1 \
			--line-buffer \
			--tagstring '[{#}]' \
			'
			test_file="{}"
			result_file="'"$results_dir"'/$(basename "$test_file" .sh).result"
			if run_test "$test_file"; then
				echo "PASS" > "$result_file"
				exit 0
			else
				echo "FAIL" > "$result_file"
				exit 1
			fi
			'

		# Count results
		for result_file in "$results_dir"/*.result; do
			if [ -f "$result_file" ]; then
				if grep -q "PASS" "$result_file"; then
					passed=$((passed + 1))
				else
					failed=$((failed + 1))
				fi
			fi
		done

		# Cleanup
		rm -rf "$results_dir"
	fi

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

	# Check for shard environment variables (used in CI)
	if [ -n "${SHARD:-}" ]; then
		mode="--shard"
		shard_id="$SHARD"
	fi

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
