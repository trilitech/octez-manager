#!/bin/bash
# Run specific integration test(s) locally
# Usage: ./run-integration-test.sh <test-name-or-pattern>
# Examples:
#   ./run-integration-test.sh 43                    # Run test 43
#   ./run-integration-test.sh import                # Run all import tests
#   ./run-integration-test.sh "node/01-install"     # Run specific test

set -euo pipefail

SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
cd "$SCRIPT_DIR"

# Colors
RED='\033[0;31m'
GREEN='\033[0;32m'
BLUE='\033[0;34m'
YELLOW='\033[1;33m'
NC='\033[0m'

log() { echo -e "${BLUE}[TEST-RUNNER]${NC} $*"; }
success() { echo -e "${GREEN}✓${NC} $*"; }
error() { echo -e "${RED}✗${NC} $*"; }
warn() { echo -e "${YELLOW}⚠${NC} $*"; }

if [ $# -eq 0 ]; then
	echo "Usage: $0 <test-name-or-pattern>"
	echo ""
	echo "Examples:"
	echo "  $0 43                      # Run test 43"
	echo "  $0 import                  # Run all import tests"
	echo "  $0 node/01-install         # Run specific test"
	echo "  $0 'baker/*'               # Run all baker tests"
	echo ""
	echo "Available test categories:"
	echo "  - node"
	echo "  - baker"
	echo "  - accuser"
	echo "  - dal"
	echo "  - import"
	echo "  - binaries"
	exit 1
fi

TEST_PATTERN="$1"

# Build fresh binary
log "Building fresh binary..."
if ! dune build; then
	error "Build failed"
	exit 1
fi

cp _build/default/src/main.exe test/integration/cli-tester/octez-manager
chmod +x test/integration/cli-tester/octez-manager
success "Binary ready"

cd test/integration

# Find matching tests
log "Finding tests matching pattern: $TEST_PATTERN"
TESTS=()

# Check if it's a category
if [ -d "cli-tester/tests/$TEST_PATTERN" ]; then
	for test in cli-tester/tests/$TEST_PATTERN/*.sh; do
		if [ -f "$test" ]; then
			TESTS+=("$test")
		fi
	done
# Check if it's a test number (e.g., "43")
elif ls cli-tester/tests/*/${TEST_PATTERN}-*.sh 2>/dev/null | head -1 >/dev/null; then
	for test in cli-tester/tests/*/${TEST_PATTERN}-*.sh; do
		if [ -f "$test" ]; then
			TESTS+=("$test")
		fi
	done
# Check if it's a full path pattern
else
	for test in cli-tester/tests/${TEST_PATTERN}.sh cli-tester/tests/${TEST_PATTERN}; do
		if [ -f "$test" ]; then
			TESTS+=("$test")
		fi
	done
fi

if [ ${#TESTS[@]} -eq 0 ]; then
	error "No tests found matching pattern: $TEST_PATTERN"
	echo ""
	echo "Available tests:"
	find cli-tester/tests -name "*.sh" -type f | sed 's|cli-tester/tests/||' | sort
	exit 1
fi

log "Found ${#TESTS[@]} test(s) to run:"
for test in "${TESTS[@]}"; do
	echo "  - $(basename $test .sh)"
done
echo ""

# Start environment
log "Setting up test environment..."
docker compose down -v 2>/dev/null || true
docker compose up -d sandbox cli-tester

# Wait for services
log "Waiting for sandbox..."
timeout 60 bash -c 'until docker compose exec -T sandbox curl -sf http://localhost:8080/health 2>/dev/null; do sleep 2; done' || {
	error "Sandbox failed to start"
	docker compose logs sandbox
	docker compose down -v
	exit 1
}

log "Waiting for systemd..."
timeout 60 bash -c 'until docker compose exec -T cli-tester systemctl is-system-running --wait 2>/dev/null | grep -qE "running|degraded"; do sleep 2; done' || {
	error "Systemd failed to start"
	docker compose down -v
	exit 1
}

success "Environment ready"
echo ""

# Run tests
PASSED=0
FAILED=0
FAILED_TESTS=()

for test in "${TESTS[@]}"; do
	test_path="/tests/$(echo $test | sed 's|cli-tester/tests/||')"
	test_name=$(basename $test .sh)

	log "Running: $test_name"

	if docker compose exec -T cli-tester bash -c "source /tests/lib.sh && bash $test_path"; then
		success "$test_name passed"
		PASSED=$((PASSED + 1))
	else
		error "$test_name failed"
		FAILED=$((FAILED + 1))
		FAILED_TESTS+=("$test_name")
	fi
	echo ""
done

# Summary
echo "========================================="
echo "Test Results:"
echo "  ${GREEN}Passed: $PASSED${NC}"
echo "  ${RED}Failed: $FAILED${NC}"

if [ $FAILED -gt 0 ]; then
	echo ""
	echo "Failed tests:"
	for test in "${FAILED_TESTS[@]}"; do
		echo "  - $test"
	done
	echo ""
	warn "Containers left running for investigation"
	echo "  docker compose exec cli-tester bash     # Interactive shell"
	echo "  docker compose logs cli-tester          # View logs"
	echo "  docker compose down -v                  # Cleanup"
	echo "========================================="
	exit 1
else
	log "Cleaning up..."
	docker compose down -v
	success "All tests passed!"
	echo "========================================="
	exit 0
fi
