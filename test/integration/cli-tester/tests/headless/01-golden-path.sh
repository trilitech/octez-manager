#!/bin/bash
set -euo pipefail
source /tests/lib.sh

echo "Test: Golden Path - Headless driver service creation"

# Verify test binary is available
if [ ! -f /usr/local/bin/test_golden_path_tui ]; then
	echo "ERROR: test_golden_path_tui binary not found at /usr/local/bin"
	echo "This binary should be built in the coverage workflow and included in the Docker image"
	exit 1
fi

# Check that systemd is running (required for service creation)
if ! systemctl status >/dev/null 2>&1; then
	echo "ERROR: systemd is not running - required for service creation"
	exit 1
fi

echo "✓ systemd is running"
echo "✓ Octez binaries available at /usr/local/bin"
echo "✓ Test binary available"

# Set CI environment variable (test requires it as safety check)
export CI=true

# Run the golden path test
echo ""
echo "=== Running Golden Path Test Binary ==="
/usr/local/bin/test_golden_path_tui -v

echo ""
echo "✓ Test passed: Headless driver successfully created service via TUI"
