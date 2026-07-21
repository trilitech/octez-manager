#!/bin/bash
# Test: Install a node instance (file generation only, no systemd)
set -euo pipefail
source /tests/lib.sh

test_init "Install node instance"

# Register instance for auto cleanup (also does pre-cleanup)
register_instance "$TEST_INSTANCE"

RPC_PORT=$(alloc_port)

# Install node (will fail at systemd step but should create files)
echo "Installing node '$TEST_INSTANCE'..."
om install-node \
	--instance "$TEST_INSTANCE" \
	--network shadownet \
	--snapshot \
	--snapshot-no-check \
	--snapshot-uri "$SANDBOX_URL/snapshot.rolling" \
	--rpc-addr "127.0.0.1:$RPC_PORT" \
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

# Verify registry entry.
# Capture the output instead of piping into grep -q: with pipefail, grep -q
# exiting at the first match can SIGPIPE octez-manager (exit 141) and turn a
# successful match into a spurious failure.
LIST_OUTPUT=$(om list 2>&1)
if [[ "$LIST_OUTPUT" != *"$TEST_INSTANCE"* ]]; then
	echo "ERROR: Instance not in registry"
	echo "$LIST_OUTPUT"
	exit 1
fi
echo "Instance registered successfully"

echo "Node installation test passed"
