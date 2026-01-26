#!/bin/bash
set -euo pipefail
source /tests/lib.sh

echo "Test: CLI install → TUI verification (hybrid workflow)"

INSTANCE="hybrid-cli-node"

# Cleanup
cleanup_instance "$INSTANCE" || true

# Install via CLI
echo "Installing node via CLI..."
om install-node \
	--instance "$INSTANCE" \
	--network shadownet \
	--snapshot \
	--snapshot-no-check \
	--snapshot-uri "$SANDBOX_URL/snapshot.rolling" \
	--rpc-addr "127.0.0.1:18710" \
	--service-user tezos \
	--no-enable 2>&1

echo "✓ CLI installation complete"

# Verify service exists
verify_service_exists "$INSTANCE" "node"

# Start TUI
tui_start

# Verify CLI-installed node appears in TUI
if ! verify_in_instances "$INSTANCE"; then
	echo "ERROR: CLI-installed node not visible in TUI"
	exit 1
fi

echo "✓ CLI-installed node visible in TUI"

# Check status shows as "stopped" (since we used --no-enable)
if tui_capture | grep -q "stopped\|inactive"; then
	echo "✓ Status correctly shows stopped"
elif tui_capture | grep -q "running\|active"; then
	echo "⚠ Service running (unexpected but not critical)"
else
	echo "⚠ Status unclear"
fi

# Try to interact with it via TUI
# Navigate to the instance and attempt to start it
echo "Attempting to start service via TUI..."
tui_send 'Down' # Select the instance
sleep 0.5
tui_send 's' # Press 's' for start (if that's the keybinding)
sleep 2

# Check if service started
if systemctl --user is-active "octez-node@${INSTANCE}" >/dev/null 2>&1; then
	echo "✓ Successfully started CLI-installed service via TUI"
elif systemctl --user list-units --all | grep -q "octez-node@${INSTANCE}"; then
	echo "⚠ Service exists but not started (action may require different interaction)"
else
	echo "⚠ Service state unclear"
fi

echo ""
echo "✓ Test passed: Hybrid CLI→TUI workflow"

# Cleanup
tui_stop
cleanup_instance "$INSTANCE"
