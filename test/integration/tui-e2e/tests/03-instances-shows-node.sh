#!/bin/bash
set -euo pipefail
source /tests/lib.sh

echo "Test: Node appears in instances page with correct status"

INSTANCE="tui-node-03"

# Cleanup
cleanup_instance "$INSTANCE" || true

# Install node via CLI first (faster for this specific test)
echo "Installing node via CLI..."
om install-node \
	--instance "$INSTANCE" \
	--network shadownet \
	--snapshot \
	--snapshot-no-check \
	--snapshot-uri "$SANDBOX_URL/snapshot.rolling" \
	--rpc-addr "127.0.0.1:18733" \
	--service-user tezos \
	--no-enable 2>&1

echo "✓ Node installed"

# Start TUI
tui_start

# Navigate to instances and verify node appears
if ! verify_in_instances "$INSTANCE"; then
	exit 1
fi

# Verify status is shown
if tui_capture | grep -q "stopped\|running\|inactive\|active"; then
	echo "✓ Service status displayed"
else
	echo "ERROR: Service status not shown"
	tui_capture
	exit 1
fi

# Verify instance details are shown
if tui_capture | grep -q "$INSTANCE"; then
	echo "✓ Instance name visible"
else
	echo "ERROR: Instance name not visible"
	exit 1
fi

# Try to select the instance (Down arrow)
tui_send 'Down'
sleep 0.5

# Check if we can see service actions menu
if tui_capture | grep -q "Start\|Stop\|Restart\|View"; then
	echo "✓ Service actions menu accessible"
else
	echo "⚠ Service actions menu not visible (may require Enter key)"
fi

echo ""
echo "✓ Test passed: Node appears in instances with status"

# Cleanup
tui_stop
cleanup_instance "$INSTANCE"
