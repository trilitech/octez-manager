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

    if bash -e "$test_file"; then
        pass "$test_name"
        return 0
    else
        fail "$test_name"
        return 1
    fi
}

# Run all tests in order
run_all_tests() {
    local failed=0
    local passed=0
    local tests=()

    # Collect tests in order
    for category in node baker accuser; do
        if [ -d "$TESTS_DIR/$category" ]; then
            for test in $(ls "$TESTS_DIR/$category"/*.sh 2>/dev/null | sort); do
                tests+=("$test")
            done
        fi
    done

    log "Found ${#tests[@]} tests to run"

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
    log "Starting integration tests..."
    log "SANDBOX_URL=$SANDBOX_URL"
    log "NODE_RPC=$NODE_RPC"

    # Export for tests
    export SANDBOX_URL NODE_RPC
    export OCTEZ_BIN_DIR="/opt/octez"
    export TEST_INSTANCE="test-node"

    wait_for_sandbox

    run_all_tests
}

main "$@"
