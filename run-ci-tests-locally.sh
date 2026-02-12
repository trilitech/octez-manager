#!/bin/bash
# Run CI tests locally
# This script mimics what CI does: builds a fresh binary and runs all tests
set -euo pipefail

SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
cd "$SCRIPT_DIR"

# Colors
RED='\033[0;31m'
GREEN='\033[0;32m'
YELLOW='\033[1;33m'
BLUE='\033[0;34m'
NC='\033[0m' # No Color

log() { echo -e "${BLUE}[CI-LOCAL]${NC} $*"; }
success() { echo -e "${GREEN}✓${NC} $*"; }
error() { echo -e "${RED}✗${NC} $*"; }
warn() { echo -e "${YELLOW}⚠${NC} $*"; }

# Step 1: Build fresh binary
log "Building octez-manager binary..."
if ! dune build; then
	error "Build failed"
	exit 1
fi
success "Build completed"

# Step 2: Run unit tests
log "Running unit tests..."
if ! dune runtest; then
	error "Unit tests failed"
	exit 1
fi
success "Unit tests passed"

# Step 3: Check formatting
log "Checking code formatting..."
if ! make fmt-check; then
	error "Code is not formatted. Run 'make fmt' to fix."
	exit 1
fi
success "Code formatting OK"

# Step 4: Check copyright headers
log "Checking copyright headers..."
if [ -x "./scripts/check-copyright.sh" ]; then
	if ! ./scripts/check-copyright.sh; then
		error "Copyright headers check failed. Run './scripts/check-copyright.sh --fix' to fix."
		exit 1
	fi
	success "Copyright headers OK"
else
	warn "Copyright check script not found, skipping"
fi

# Step 5: Copy fresh binary to integration test directory
log "Copying binary to integration test directory..."
BINARY="_build/default/src/main.exe"
TARGET="test/integration/cli-tester/octez-manager"

if [ ! -f "$BINARY" ]; then
	error "Binary not found at $BINARY"
	exit 1
fi

# Remove old binary if it exists (might be read-only)
rm -f "$TARGET"
cp "$BINARY" "$TARGET"
chmod +x "$TARGET"
success "Binary copied to $TARGET"

# Step 6: Build and run integration tests
log "Starting integration tests..."
cd test/integration

# Check for port conflicts
log "Checking for port conflicts..."
PORTS_IN_USE=""
for port in 8732 8080; do
	if lsof -i :$port >/dev/null 2>&1 || netstat -tln 2>/dev/null | grep -q ":$port " || ss -tln 2>/dev/null | grep -q ":$port "; then
		PORTS_IN_USE="$PORTS_IN_USE $port"
	fi
done

if [ -n "$PORTS_IN_USE" ]; then
	error "Ports already in use:$PORTS_IN_USE"
	echo ""
	echo "Integration tests need these ports to be free."
	echo "Please stop any services using these ports:"
	echo ""
	for port in $PORTS_IN_USE; do
		echo "  Port $port:"
		lsof -i :$port 2>/dev/null | grep LISTEN || netstat -tlnp 2>/dev/null | grep ":$port " || ss -tlnp 2>/dev/null | grep ":$port " || echo "    (process info unavailable)"
	done
	echo ""
	echo "Common causes:"
	echo "  - Octez node running locally (systemctl stop octez-node@...)"
	echo "  - Development server on port 8080"
	echo "  - Previous test run didn't cleanup (cd test/integration && docker compose down -v)"
	exit 1
fi

# Clean up any existing containers
log "Cleaning up any existing containers..."
docker compose down -v 2>/dev/null || true

# Build containers
log "Building Docker containers..."
if ! docker compose build; then
	error "Docker build failed"
	exit 1
fi
success "Containers built"

# Start sandbox
log "Starting sandbox..."
docker compose up -d sandbox

# Wait for sandbox
log "Waiting for sandbox to be ready..."
timeout 180 bash -c 'until docker compose exec -T sandbox curl -sf http://localhost:8080/health 2>/dev/null; do sleep 2; done' || {
	error "Sandbox failed to start"
	docker compose logs sandbox
	docker compose down -v
	exit 1
}
success "Sandbox ready"

# Start cli-tester
log "Starting cli-tester container..."
docker compose up -d cli-tester

# Wait for systemd
log "Waiting for systemd to initialize..."
timeout 60 bash -c 'until docker compose exec -T cli-tester systemctl is-system-running --wait 2>/dev/null | grep -qE "running|degraded"; do sleep 2; done' || {
	error "Systemd failed to initialize"
	docker compose exec -T cli-tester systemctl status || true
	docker compose down -v
	exit 1
}
success "Systemd ready"

# Run tests
log "Running all integration tests..."
echo ""
if docker compose exec -T cli-tester /run-tests.sh; then
	TEST_RESULT=0
	echo ""
	success "All integration tests passed!"
else
	TEST_RESULT=1
	echo ""
	error "Integration tests failed"
fi

# Cleanup
log "Cleaning up containers..."
docker compose down -v

# Summary
echo ""
echo "========================================="
if [ $TEST_RESULT -eq 0 ]; then
	success "ALL CI CHECKS PASSED ✓"
	echo ""
	echo "Your code is ready for CI:"
	echo "  ✓ Build successful"
	echo "  ✓ Unit tests passed"
	echo "  ✓ Code formatting OK"
	echo "  ✓ Copyright headers OK"
	echo "  ✓ Integration tests passed"
else
	error "CI CHECKS FAILED ✗"
	echo ""
	echo "Fix the integration test failures before pushing."
fi
echo "========================================="

exit $TEST_RESULT
