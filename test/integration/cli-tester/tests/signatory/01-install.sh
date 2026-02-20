#!/bin/bash
# Test: Install Signatory with file backend
set -euo pipefail
source /tests/lib.sh

test_init "Install signatory instance"

# Register instance for auto cleanup (also does pre-cleanup)
register_instance "$TEST_INSTANCE"

SIGNER_PORT=$(alloc_port)
METRICS_PORT=$(alloc_port)

# Install Signatory
echo "Installing signatory '$TEST_INSTANCE'..."
om install-signatory \
	--instance "$TEST_INSTANCE" \
	--backend file \
	--authorized-keys "tz1KqTpEZ7Yob7QbPE4Hy4Wo8fHG8LhKxZSx,tz2LBtbMMvvguWQupgEmtfjtXy77cHgdr5TE" \
	--address "127.0.0.1:$SIGNER_PORT" \
	--metrics-address "127.0.0.1:$METRICS_PORT" \
	--service-user tezos \
	--no-enable 2>&1 || true

# Verify data directory exists (path depends on whether running as root)
if [ "$(id -u)" -eq 0 ]; then
	DATA_DIR="/var/lib/octez/signatory/$TEST_INSTANCE"
else
	DATA_DIR="$HOME/.local/share/octez/signatory/$TEST_INSTANCE"
fi
if [ ! -d "$DATA_DIR" ]; then
	echo "ERROR: Data directory not created: $DATA_DIR"
	echo "Searching for data directories..."
	find /var/lib/octez ~/.local/share/octez -type d 2>/dev/null || true
	exit 1
fi
echo "Data directory created: $DATA_DIR"

# Verify config file exists
CONFIG_FILE="$DATA_DIR/signatory.yaml"
if [ ! -f "$CONFIG_FILE" ]; then
	echo "ERROR: Config file not created: $CONFIG_FILE"
	ls -la "$DATA_DIR" || true
	exit 1
fi
echo "Config file created: $CONFIG_FILE"

# Verify config contains authorized keys
if ! grep -q "tz1KqTpEZ7Yob7QbPE4Hy4Wo8fHG8LhKxZSx" "$CONFIG_FILE"; then
	echo "ERROR: Authorized key not found in config"
	cat "$CONFIG_FILE"
	exit 1
fi
echo "Authorized keys configured correctly"

# Verify config contains correct address
if ! grep -q "127.0.0.1:$SIGNER_PORT" "$CONFIG_FILE"; then
	echo "ERROR: HTTP address not found in config"
	cat "$CONFIG_FILE"
	exit 1
fi
echo "HTTP address configured correctly"

# Verify registry entry
if ! om list 2>&1 | grep -q "$TEST_INSTANCE"; then
	echo "ERROR: Instance not in registry"
	om list 2>&1 || true
	exit 1
fi
echo "Instance registered successfully"

# Verify instance shows as signatory role
if ! om list 2>&1 | grep "$TEST_INSTANCE" | grep -q "signatory"; then
	echo "ERROR: Instance not registered as signatory"
	om list 2>&1 || true
	exit 1
fi
echo "Instance role verified"

echo "Signatory installation test passed"
