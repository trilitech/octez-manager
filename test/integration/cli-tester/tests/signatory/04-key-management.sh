#!/bin/bash
set -euo pipefail
source /tests/lib.sh

echo "Test: Signatory Key Management"

# Initialize test harness (automatic cleanup on EXIT)
test_init

# Allocate unique port for this test
SIGNATORY_PORT=$(alloc_port)
SIGNATORY_INSTANCE="test-signatory-keys"

echo "==> Step 1: Install signatory with multiple authorized keys (different key types)"
om install-signatory \
	--instance "$SIGNATORY_INSTANCE" \
	--backend file \
	--address "127.0.0.1:$SIGNATORY_PORT" \
	--authorized-keys "tz1VSUr8wwNhLAzempoch5d6hLRiTh8Cjcjb,tz2TSvNTh2epDMhZHrw73nV9piBX7kLZ9K9m,tz3RDC3Jdn4j15J7bBHZd29EUee9gVB1CxD9,tz4EECtMxAuJ9UDLaisMwAnHTG8ZfdQahMWq" \
	--keys-dir "/var/lib/tezos/.tezos-signatory/keys-test" \
	--service-user tezos \
	--no-enable 2>&1

register_instance "$SIGNATORY_INSTANCE"

echo "==> Step 2: Verify signatory configuration file"
CONFIG_FILE="/var/lib/tezos/.tezos-signatory/${SIGNATORY_INSTANCE}/config.toml"

if [ ! -f "$CONFIG_FILE" ]; then
	echo "ERROR: Signatory config file not found: $CONFIG_FILE"
	exit 1
fi

echo "==> Step 3: Verify all key types are in authorized_keys"
# Check for tz1 (ed25519)
if ! grep -q "tz1VSUr8wwNhLAzempoch5d6hLRiTh8Cjcjb" "$CONFIG_FILE"; then
	echo "ERROR: tz1 key not found in config"
	cat "$CONFIG_FILE"
	exit 1
fi

# Check for tz2 (secp256k1)
if ! grep -q "tz2TSvNTh2epDMhZHrw73nV9piBX7kLZ9K9m" "$CONFIG_FILE"; then
	echo "ERROR: tz2 key not found in config"
	cat "$CONFIG_FILE"
	exit 1
fi

# Check for tz3 (p256)
if ! grep -q "tz3RDC3Jdn4j15J7bBHZd29EUee9gVB1CxD9" "$CONFIG_FILE"; then
	echo "ERROR: tz3 key not found in config"
	cat "$CONFIG_FILE"
	exit 1
fi

# Check for tz4 (bls)
if ! grep -q "tz4EECtMxAuJ9UDLaisMwAnHTG8ZfdQahMWq" "$CONFIG_FILE"; then
	echo "ERROR: tz4 key not found in config"
	cat "$CONFIG_FILE"
	exit 1
fi

echo "==> Step 4: Verify keys directory configuration"
if ! grep -q "keys-test" "$CONFIG_FILE"; then
	echo "ERROR: Custom keys directory not in config"
	cat "$CONFIG_FILE"
	exit 1
fi

echo "==> Step 5: Verify keys directory was created"
KEYS_DIR="/var/lib/tezos/.tezos-signatory/keys-test"
if [ ! -d "$KEYS_DIR" ]; then
	echo "ERROR: Keys directory not created: $KEYS_DIR"
	exit 1
fi

echo "==> Step 6: Verify keys directory permissions"
KEYS_DIR_OWNER=$(stat -c '%U' "$KEYS_DIR")
if [ "$KEYS_DIR_OWNER" != "tezos" ]; then
	echo "ERROR: Keys directory not owned by tezos user (owner: $KEYS_DIR_OWNER)"
	exit 1
fi

KEYS_DIR_PERMS=$(stat -c '%a' "$KEYS_DIR")
if [ "$KEYS_DIR_PERMS" != "700" ]; then
	echo "ERROR: Keys directory has incorrect permissions: $KEYS_DIR_PERMS (expected 700)"
	exit 1
fi

echo "==> Step 7: Verify signatory service unit"
SIGNATORY_UNIT="signatory@${SIGNATORY_INSTANCE}.service"
if ! systemctl list-unit-files | grep -q "$SIGNATORY_UNIT"; then
	echo "ERROR: Signatory service unit not found: $SIGNATORY_UNIT"
	exit 1
fi

echo "==> Step 8: Test signatory list command"
om list-signatories 2>&1 | tee /tmp/list-output.txt

if ! grep -q "$SIGNATORY_INSTANCE" /tmp/list-output.txt; then
	echo "ERROR: Signatory instance not in list output"
	cat /tmp/list-output.txt
	exit 1
fi

echo "==> Step 9: Test signatory info command"
om info-signatory --instance "$SIGNATORY_INSTANCE" 2>&1 | tee /tmp/info-output.txt

if ! grep -q "$SIGNATORY_INSTANCE" /tmp/info-output.txt; then
	echo "ERROR: Instance name not in info output"
	cat /tmp/info-output.txt
	exit 1
fi

# Verify at least some key info is displayed
if ! grep -q -E "tz[1-4]" /tmp/info-output.txt; then
	echo "ERROR: No key hashes found in info output"
	cat /tmp/info-output.txt
	exit 1
fi

echo "Test passed: Key management working correctly"
