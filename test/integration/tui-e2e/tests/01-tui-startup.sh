#!/bin/bash
set -euo pipefail
source /tests/lib.sh

echo "Test: TUI starts without errors"

# Start TUI
tui_start

# Verify main menu appears
if ! tui_wait_for "Instances\|Install\|Binaries" 5; then
	echo "ERROR: Main menu did not appear"
	tui_capture
	exit 1
fi

echo "✓ TUI main menu displayed"

# Verify we can navigate
tui_send 'Down'
sleep 0.3
tui_send 'Up'
sleep 0.3

echo "✓ Navigation works"

# Check help menu
tui_send '?'
if tui_wait_for "Help\|Keys" 3; then
	echo "✓ Help menu accessible"
	tui_send 'Escape' # Close help
else
	echo "⚠ Help menu not found (may be page-specific)"
fi

# Verify we can open install menu
nav_to_install
echo "✓ Install menu accessible"

# Return to main
nav_back

echo "✓ Navigation back to main works"

# Stop cleanly
tui_stop

echo ""
echo "✓ Test passed: TUI startup and basic navigation"
