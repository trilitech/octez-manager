#!/bin/bash
# Debug test 43 (import-detect-service-based) interactively
set -euo pipefail

SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
cd "$SCRIPT_DIR"

# Colors
RED='\033[0;31m'
GREEN='\033[0;32m'
YELLOW='\033[1;33m'
BLUE='\033[0;34m'
NC='\033[0m' # No Color

log() { echo -e "${BLUE}[DEBUG-43]${NC} $*"; }
success() { echo -e "${GREEN}✓${NC} $*"; }
error() { echo -e "${RED}✗${NC} $*"; }

# Build fresh binary
log "Building fresh binary..."
dune build
cp _build/default/src/main.exe test/integration/cli-tester/octez-manager
chmod +x test/integration/cli-tester/octez-manager
success "Binary updated"

# Go to integration test directory
cd test/integration

# Clean up
log "Cleaning up any existing containers..."
docker compose down -v 2>/dev/null || true

# Start environment
log "Starting test environment..."
docker compose up -d sandbox cli-tester

# Wait for sandbox
log "Waiting for sandbox..."
timeout 60 bash -c 'until docker compose exec -T sandbox curl -sf http://localhost:8080/health 2>/dev/null; do sleep 2; done'
success "Sandbox ready"

# Wait for systemd
log "Waiting for systemd..."
timeout 60 bash -c 'until docker compose exec -T cli-tester systemctl is-system-running --wait 2>/dev/null | grep -qE "running|degraded"; do sleep 2; done'
success "Systemd ready"

# Run test 43 with verbose output
log "Running test 43..."
echo ""
echo "========================================="
echo "Starting test execution..."
echo "========================================="
echo ""

docker compose exec -T cli-tester bash -c "source /tests/lib.sh && bash -x /tests/import/43-import-detect-service-based.sh"
TEST_RESULT=$?

echo ""
echo "========================================="

if [ $TEST_RESULT -eq 0 ]; then
	success "Test 43 PASSED"
else
	error "Test 43 FAILED"
	echo ""
	log "Gathering diagnostic information..."
	echo ""

	echo "--- systemctl list-unit-files (octez-node@) ---"
	docker compose exec -T cli-tester systemctl list-unit-files | grep "octez-node@" || echo "No octez-node@ units found"
	echo ""

	echo "--- /etc/systemd/system/octez-node@* files ---"
	docker compose exec -T cli-tester ls -la /etc/systemd/system/octez-node@* 2>/dev/null || echo "No service files found"
	echo ""

	echo "--- om list output ---"
	docker compose exec -T cli-tester om list 2>&1
	echo ""

	echo "--- om list --external output ---"
	docker compose exec -T cli-tester om list --external 2>&1
	echo ""

	echo ""
	log "To investigate further, run:"
	echo "  docker compose exec cli-tester bash"
	echo ""
	log "Then inside the container, you can:"
	echo "  source /tests/lib.sh"
	echo "  systemctl list-unit-files | grep octez"
	echo "  om list --external"
	echo "  cat /etc/systemd/system/octez-node@external-node-detect.service"
	echo ""
	log "When done investigating:"
	echo "  docker compose down -v"
	echo ""

	# Don't cleanup automatically on failure so user can investigate
	log "Containers left running for investigation"
	log "Run 'cd test/integration && docker compose down -v' to cleanup"
	exit 1
fi

# Cleanup on success
log "Cleaning up..."
docker compose down -v
success "All done!"
