#!/bin/bash
set -euo pipefail
source /tests/lib.sh

echo "Test: Golden Path - Declarative TUI command runner"

# Verify test binary is available (new v2 version)
if [ ! -f /usr/local/bin/test_golden_path_tui_v2 ]; then
	echo "ERROR: test_golden_path_tui_v2 binary not found at /usr/local/bin"
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

# Run the golden path test (new declarative version)
echo ""
echo "=== Running Golden Path Test Binary (V2 - Declarative) ==="
/usr/local/bin/test_golden_path_tui_v2 -v

echo ""
echo "✓ Test passed: Declarative TUI command runner successfully created service"
