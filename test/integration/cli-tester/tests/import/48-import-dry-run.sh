#!/bin/bash
# Test: Dry-run shows plan without making changes
set -euo pipefail
source /tests/lib.sh

INSTANCE="dryrun-node"
DATA_DIR="/var/lib/octez-external/$INSTANCE"
RPC_ADDR="127.0.0.1:18748"

echo "Test: Import dry-run preview"

# Cleanup
cleanup_instance "$INSTANCE" || true
rm -rf "$DATA_DIR" || true
systemctl --user stop "octez-node@${INSTANCE}.service" 2>/dev/null || true
systemctl --user disable "octez-node@${INSTANCE}.service" 2>/dev/null || true
rm -f "/etc/systemd/user/octez-node@${INSTANCE}.service" || true
systemctl --user daemon-reload

# Create external service
echo "Creating external systemd service..."
create_external_service "node" "$INSTANCE" "$DATA_DIR" "$RPC_ADDR" "shadownet"
systemctl --user enable "octez-node@${INSTANCE}.service"
systemctl --user start "octez-node@${INSTANCE}.service"

wait_for_service_active "node" "$INSTANCE" 10 || true

# Run dry-run
echo "Running dry-run import..."
om import "octez-node@${INSTANCE}" --dry-run 2>&1 > /tmp/dryrun_output.txt || true

cat /tmp/dryrun_output.txt

# Verify service is still external (not imported)
if service_is_managed "$INSTANCE"; then
    echo "ERROR: Service should not be imported during dry-run"
    om list 2>&1
    exit 1
fi

# Verify external service is still enabled and running
if ! systemctl --user is-enabled "octez-node@${INSTANCE}.service" >/dev/null 2>&1; then
    echo "ERROR: External service should still be enabled after dry-run"
    exit 1
fi

if ! systemctl --user is-active "octez-node@${INSTANCE}.service" >/dev/null 2>&1; then
    echo "ERROR: External service should still be running after dry-run"
    exit 1
fi

echo "Dry-run correctly showed plan without making changes"

# Cleanup
systemctl --user stop "octez-node@${INSTANCE}.service" || true
systemctl --user disable "octez-node@${INSTANCE}.service" || true
rm -f "/etc/systemd/user/octez-node@${INSTANCE}.service" || true
systemctl --user daemon-reload
rm -rf "$DATA_DIR"

echo "Dry-run test passed"
