#!/bin/bash
# Script to clean up unmanaged test services

set -euo pipefail

echo "Cleaning up unmanaged test services..."
echo ""

# Stop services
echo "[1/5] Stopping services..."
systemctl --user stop test-octez-node.service 2>/dev/null || true
systemctl --user stop test-octez-dal-node.service 2>/dev/null || true
systemctl --user stop test-octez-baker.service 2>/dev/null || true
systemctl --user stop test-octez-accuser.service 2>/dev/null || true

# Disable services
echo "[2/5] Disabling services..."
systemctl --user disable test-octez-node.service 2>/dev/null || true
systemctl --user disable test-octez-dal-node.service 2>/dev/null || true
systemctl --user disable test-octez-baker.service 2>/dev/null || true
systemctl --user disable test-octez-accuser.service 2>/dev/null || true

# Remove service files
echo "[3/5] Removing service files..."
rm -f "${HOME}/.config/systemd/user/test-octez-node.service"
rm -f "${HOME}/.config/systemd/user/test-octez-dal-node.service"
rm -f "${HOME}/.config/systemd/user/test-octez-baker.service"
rm -f "${HOME}/.config/systemd/user/test-octez-accuser.service"

# Reload systemd
echo "[4/5] Reloading systemd user daemon..."
systemctl --user daemon-reload

# Remove data directories
echo "[5/5] Removing data directories..."
rm -rf /tmp/octez-unmanaged-test
rm -rf "${HOME}/.config/octez-unmanaged-env"

echo ""
echo "✓ Cleanup complete!"
echo ""
