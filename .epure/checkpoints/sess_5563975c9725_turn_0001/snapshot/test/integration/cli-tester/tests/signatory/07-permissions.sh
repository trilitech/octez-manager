#!/bin/bash
# Test: Signatory per-key permissions configuration
set -euo pipefail
source /tests/lib.sh

test_init "Signatory permissions test"

# Use unique instance names for this test
TEST_INSTANCE_1="test-sig-perms-1"
TEST_INSTANCE_2="test-sig-perms-2"
TEST_INSTANCE_3="test-sig-perms-3"

# Register instances for auto cleanup
register_instance "$TEST_INSTANCE_1"
register_instance "$TEST_INSTANCE_2"
register_instance "$TEST_INSTANCE_3"

SIGNER_PORT_1=$(alloc_port)
SIGNER_PORT_2=$(alloc_port)
SIGNER_PORT_3=$(alloc_port)

# Test 1: Install with custom permissions (consensus only)
echo "Test 1: Installing signatory with consensus-only permissions..."
om install-signatory \
	--instance "$TEST_INSTANCE_1" \
	--backend file \
	--authorized-keys "tz1KqTpEZ7Yob7QbPE4Hy4Wo8fHG8LhKxZSx:block,attestation,preattestation" \
	--address "127.0.0.1:$SIGNER_PORT_1" \
	--app-bin-dir /usr/local/bin \
	--service-user tezos \
	--no-enable 2>&1 || true

# Verify config has only specified permissions
DATA_DIR_1="$HOME/.local/share/octez/signatory/$TEST_INSTANCE_1"
if [ "$(id -u)" -eq 0 ]; then
	DATA_DIR_1="/var/lib/octez/signatory/$TEST_INSTANCE_1"
fi
CONFIG_FILE_1="$DATA_DIR_1/signatory.yaml"

if ! grep -q "block:" "$CONFIG_FILE_1"; then
	echo "ERROR: Missing 'block' permission"
	cat "$CONFIG_FILE_1"
	exit 1
fi

if ! grep -q "attestation:" "$CONFIG_FILE_1"; then
	echo "ERROR: Missing 'attestation' permission"
	cat "$CONFIG_FILE_1"
	exit 1
fi

if grep -A10 "tz1KqTpEZ7Yob7QbPE4Hy4Wo8fHG8LhKxZSx" "$CONFIG_FILE_1" | grep -q "generic:"; then
	echo "ERROR: Should not have 'generic' permission (consensus-only key)"
	cat "$CONFIG_FILE_1"
	exit 1
fi

echo "✓ Consensus-only permissions configured correctly"

# Test 2: Install with generic (manager) only
echo "Test 2: Installing signatory with manager-only permissions..."
om install-signatory \
	--instance "$TEST_INSTANCE_2" \
	--backend file \
	--authorized-keys "tz2LBtbMMvvguWQupgEmtfjtXy77cHgdr5TE:generic" \
	--address "127.0.0.1:$SIGNER_PORT_2" \
	--app-bin-dir /usr/local/bin \
	--service-user tezos \
	--no-enable 2>&1 || true

DATA_DIR_2="$HOME/.local/share/octez/signatory/$TEST_INSTANCE_2"
if [ "$(id -u)" -eq 0 ]; then
	DATA_DIR_2="/var/lib/octez/signatory/$TEST_INSTANCE_2"
fi
CONFIG_FILE_2="$DATA_DIR_2/signatory.yaml"

if ! grep -A5 "tz2LBtbMMvvguWQupgEmtfjtXy77cHgdr5TE" "$CONFIG_FILE_2" | grep -q "generic:"; then
	echo "ERROR: Missing 'generic' permission"
	cat "$CONFIG_FILE_2"
	exit 1
fi

if grep -A5 "tz2LBtbMMvvguWQupgEmtfjtXy77cHgdr5TE" "$CONFIG_FILE_2" | grep -q "block:"; then
	echo "ERROR: Should not have 'block' permission (manager-only key)"
	cat "$CONFIG_FILE_2"
	exit 1
fi

echo "✓ Manager-only permissions configured correctly"

# Test 3: Install with multiple keys with different permissions
echo "Test 3: Installing signatory with multiple keys and different permissions..."
om install-signatory \
	--instance "$TEST_INSTANCE_3" \
	--backend file \
	--authorized-keys "tz1KqTpEZ7Yob7QbPE4Hy4Wo8fHG8LhKxZSx:block,attestation tz2LBtbMMvvguWQupgEmtfjtXy77cHgdr5TE:generic" \
	--address "127.0.0.1:$SIGNER_PORT_3" \
	--app-bin-dir /usr/local/bin \
	--service-user tezos \
	--no-enable 2>&1 || true

DATA_DIR_3="$HOME/.local/share/octez/signatory/$TEST_INSTANCE_3"
if [ "$(id -u)" -eq 0 ]; then
	DATA_DIR_3="/var/lib/octez/signatory/$TEST_INSTANCE_3"
fi
CONFIG_FILE_3="$DATA_DIR_3/signatory.yaml"

# Verify first key has block and attestation
if ! grep -A5 "tz1KqTpEZ7Yob7QbPE4Hy4Wo8fHG8LhKxZSx" "$CONFIG_FILE_3" | grep -q "block:"; then
	echo "ERROR: First key missing 'block' permission"
	cat "$CONFIG_FILE_3"
	exit 1
fi

if ! grep -A5 "tz1KqTpEZ7Yob7QbPE4Hy4Wo8fHG8LhKxZSx" "$CONFIG_FILE_3" | grep -q "attestation:"; then
	echo "ERROR: First key missing 'attestation' permission"
	cat "$CONFIG_FILE_3"
	exit 1
fi

# Verify second key has generic
if ! grep -A5 "tz2LBtbMMvvguWQupgEmtfjtXy77cHgdr5TE" "$CONFIG_FILE_3" | grep -q "generic:"; then
	echo "ERROR: Second key missing 'generic' permission"
	cat "$CONFIG_FILE_3"
	exit 1
fi

# Verify second key doesn't have block
if grep -A5 "tz2LBtbMMvvguWQupgEmtfjtXy77cHgdr5TE" "$CONFIG_FILE_3" | grep -q "block:"; then
	echo "ERROR: Second key should not have 'block' permission"
	cat "$CONFIG_FILE_3"
	exit 1
fi

echo "✓ Multiple keys with different permissions configured correctly"

# Test 4: Default permissions (no explicit specification)
echo "Test 4: Verifying default permissions (backwards compatibility)..."
TEST_INSTANCE_4="test-sig-perms-default"
register_instance "$TEST_INSTANCE_4"
SIGNER_PORT_4=$(alloc_port)

om install-signatory \
	--instance "$TEST_INSTANCE_4" \
	--backend file \
	--authorized-keys "tz1KqTpEZ7Yob7QbPE4Hy4Wo8fHG8LhKxZSx" \
	--address "127.0.0.1:$SIGNER_PORT_4" \
	--app-bin-dir /usr/local/bin \
	--service-user tezos \
	--no-enable 2>&1 || true

DATA_DIR_4="$HOME/.local/share/octez/signatory/$TEST_INSTANCE_4"
if [ "$(id -u)" -eq 0 ]; then
	DATA_DIR_4="/var/lib/octez/signatory/$TEST_INSTANCE_4"
fi
CONFIG_FILE_4="$DATA_DIR_4/signatory.yaml"

# Verify all permissions are present (default behavior)
for perm in "block:" "attestation:" "preattestation:" "attestation_with_dal:" "generic:"; do
	if ! grep -A10 "tz1KqTpEZ7Yob7QbPE4Hy4Wo8fHG8LhKxZSx" "$CONFIG_FILE_4" | grep -q "$perm"; then
		echo "ERROR: Default permissions missing '$perm'"
		cat "$CONFIG_FILE_4"
		exit 1
	fi
done

echo "✓ Default permissions (all operations) configured correctly"

echo "All signatory permissions tests passed"
