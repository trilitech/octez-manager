#!/bin/bash
set -euo pipefail
source /tests/lib.sh

echo "Test: TUI menu navigation and modal interaction"

INSTANCE="tui-node-02"

# Cleanup any previous state
cleanup_instance "$INSTANCE" || true

# Install node via CLI first (form filling via tmux is complex and flaky)
echo "Installing node via CLI for TUI verification..."
om install-node \
	--instance "$INSTANCE" \
	--network shadownet \
	--snapshot \
	--snapshot-no-check \
	--snapshot-uri "$SANDBOX_URL/snapshot.rolling" \
	--rpc-addr "127.0.0.1:18732" \
	--service-user tezos \
	--no-enable 2>&1

echo "✓ Node installed via CLI"

# Start TUI
tui_start

# Test 1: Verify node appears in instances list
if ! verify_in_instances "$INSTANCE"; then
	exit 1
fi
echo "✓ Installed node visible in TUI"

# Test 2: Open and close create menu
nav_to_install
if tui_capture | grep -q "Node"; then
	echo "✓ Create menu opens and shows service options"
else
	echo "ERROR: Create menu did not open properly"
	tui_capture
	exit 1
fi

# Close menu
nav_back
sleep 0.5

# Test 3: Verify we can navigate and interact with the instance
tui_send 'Down' # Move to the installed node
sleep 0.5

# Check if we can see instance details
if tui_capture | grep -q "$INSTANCE"; then
	echo "✓ Can navigate to and view instance"
else
	echo "ERROR: Cannot see instance details"
	tui_capture
	exit 1
fi

echo ""
echo "✓ Test passed: TUI menu navigation and modal interaction"

# Cleanup
tui_stop
cleanup_instance "$INSTANCE"
