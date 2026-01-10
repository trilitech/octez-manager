#!/bin/bash
# Build and run integration tests
set -euo pipefail

SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
PROJECT_ROOT="$(cd "$SCRIPT_DIR/../.." && pwd)"

usage() {
    echo "Usage: $0 [--build]"
    echo ""
    echo "Options:"
    echo "  --build    Build static binary using CI Docker image before running tests"
    echo "             (requires MIAOU_GIT_URL env var and ghcr.io authentication)"
    echo ""
    echo "Without --build, expects a statically-linked binary at:"
    echo "  $SCRIPT_DIR/cli-tester/octez-manager"
}

BUILD_BINARY=false

while [[ $# -gt 0 ]]; do
    case $1 in
        --build)
            BUILD_BINARY=true
            shift
            ;;
        -h|--help)
            usage
            exit 0
            ;;
        *)
            echo "Unknown option: $1"
            usage
            exit 1
            ;;
    esac
done

cd "$SCRIPT_DIR"

if [ "$BUILD_BINARY" = true ]; then
    echo "Building static binary using CI image..."
    ./build-static.sh
fi

# Check for binary
if [ ! -f "$SCRIPT_DIR/cli-tester/octez-manager" ]; then
    echo "ERROR: No octez-manager binary found at $SCRIPT_DIR/cli-tester/octez-manager"
    echo ""
    echo "Options:"
    echo "  1. Run with --build flag to build a static binary (requires MIAOU_GIT_URL)"
    echo "  2. Download a static binary from CI artifacts"
    echo "  3. Copy a pre-built static binary to $SCRIPT_DIR/cli-tester/octez-manager"
    exit 1
fi

# Build containers
echo "Building containers..."
docker compose build

# Start sandbox
echo "Starting sandbox..."
docker compose up -d sandbox

# Wait for sandbox to be healthy
echo "Waiting for sandbox to be ready..."
timeout 180 bash -c 'until docker compose exec -T sandbox curl -sf http://localhost:8080/health; do sleep 5; done'

# Start cli-tester container
echo "Starting cli-tester..."
docker compose up -d cli-tester

# Wait for container to be ready
echo "Waiting for container to initialize..."
sleep 5

# Run tests
echo "Running integration tests..."
docker compose exec -T cli-tester /run-tests.sh
TEST_EXIT=$?

# Cleanup
echo "Cleaning up..."
docker compose down -v

exit $TEST_EXIT
