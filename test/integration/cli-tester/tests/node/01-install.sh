#!/bin/bash
# Test: Install a node instance (file generation only, no systemd)
set -euo pipefail
source /tests/lib.sh

echo "Test: Install node instance"

# Cleanup any previous test state
cleanup_instance "$TEST_INSTANCE" || true

# Install node (will fail at systemd step but should create files)
echo "Installing node '$TEST_INSTANCE'..."
om install-node \
    --instance "$TEST_INSTANCE" \
    --network tallinnnet \
    --snapshot \
    --snapshot-uri "$SANDBOX_URL/snapshot.rolling" \
    --rpc-addr "127.0.0.1:8732" \
    --service-user tezos \
    --no-enable 2>&1 || true

# Verify env file was created (path depends on whether running as root)
if [ "$(id -u)" -eq 0 ]; then
    ENV_FILE="/etc/octez/instances/$TEST_INSTANCE/node.env"
else
    ENV_FILE="$HOME/.config/octez/instances/$TEST_INSTANCE/node.env"
fi
if [ ! -f "$ENV_FILE" ]; then
    echo "ERROR: Env file not created: $ENV_FILE"
    echo "Searching for env files..."
    find /etc/octez /root/.config/octez ~/.config/octez -name "*.env" 2>/dev/null || true
    exit 1
fi
echo "Env file created: $ENV_FILE"

# Verify data directory exists (path depends on whether running as root)
if [ "$(id -u)" -eq 0 ]; then
    DATA_DIR="/var/lib/octez/$TEST_INSTANCE"
else
    DATA_DIR="$HOME/.local/share/octez/$TEST_INSTANCE"
fi
if [ ! -d "$DATA_DIR" ]; then
    echo "ERROR: Data directory not created: $DATA_DIR"
    echo "Searching for data directories..."
    find /var/lib/octez ~/.local/share/octez -type d 2>/dev/null || true
    exit 1
fi
echo "Data directory created: $DATA_DIR"

# Verify registry entry
if ! om list 2>&1 | grep -q "$TEST_INSTANCE"; then
    echo "ERROR: Instance not in registry"
    om list 2>&1 || true
    exit 1
fi
echo "Instance registered successfully"

echo "Node installation test passed"
