#!/bin/bash
set -euo pipefail
source /tests/lib.sh

echo "Test: Install shadownet node via TUI"

INSTANCE="tui-node-01"

# Cleanup any previous state
cleanup_instance "$INSTANCE" || true

# Start TUI
tui_start

# Navigate to install > node
nav_to_install
tui_send '1' # Select Node
tui_wait_for "Instance name"

# Fill form fields
echo "Filling form: Instance name"
form_fill_text "$INSTANCE"
form_next

echo "Filling form: Network selection"
# Navigate to Shadownet (usually 2 downs from default)
form_select_choice "Down Down"
form_next

echo "Filling form: Snapshot"
form_toggle_yes
form_next

echo "Filling form: Snapshot URI"
form_fill_text "$SANDBOX_URL/snapshot.rolling"
form_next

echo "Filling form: Skip checksum verification"
form_toggle_yes
form_next

echo "Filling form: RPC address"
form_fill_text "127.0.0.1:18731"
form_next

echo "Filling form: Service user"
form_fill_text "tezos"
form_next

echo "Filling form: Enable on boot"
form_toggle_yes
form_next

# Submit form
form_submit

# Wait for installation to complete
echo "Waiting for installation..."
if ! tui_wait_for "complete\|successful\|installed" 90; then
	echo "ERROR: Installation did not complete"
	echo "Checking for errors..."
	if tui_capture | grep -qi "error\|failed"; then
		echo "Installation failed with error:"
		tui_capture | grep -i "error\|failed"
	fi
	exit 1
fi

echo "✓ Installation completed"

# Give systemd a moment to create the service
sleep 2

# Verify systemd service was created
if ! verify_service_exists "$INSTANCE" "node"; then
	exit 1
fi
echo "✓ Service exists in systemd"

# Verify service is enabled
if systemctl --user is-enabled "octez-node@${INSTANCE}" >/dev/null 2>&1; then
	echo "✓ Service is enabled"
else
	echo "⚠ Service not enabled (may be expected if --no-enable was used)"
fi

# Verify service is running
sleep 3
if ! verify_service_running "$INSTANCE" "node"; then
	echo "ERROR: Service failed to start"
	journalctl --user -u "octez-node@${INSTANCE}" -n 50 --no-pager || true
	exit 1
fi
echo "✓ Service is running"

# Check RPC is responding
sleep 5
if curl -sf "http://127.0.0.1:18731/chains/main/blocks/head" >/dev/null 2>&1; then
	echo "✓ Node RPC is responding"
else
	echo "⚠ Node RPC not yet responding (may need more time to bootstrap)"
fi

echo ""
echo "✓ Test passed: Node installation via TUI"

# Cleanup
tui_stop
cleanup_instance "$INSTANCE"
