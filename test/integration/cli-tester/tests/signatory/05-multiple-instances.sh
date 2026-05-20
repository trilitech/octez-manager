#!/bin/bash
set -euo pipefail
source /tests/lib.sh

echo "Test: Multiple Signatory Instances"

# Initialize test harness (automatic cleanup on EXIT)
test_init

# Allocate unique ports for this test
SIGNATORY1_PORT=$(alloc_port)
SIGNATORY2_PORT=$(alloc_port)
SIGNATORY3_PORT=$(alloc_port)
NODE_RPC_PORT=$(alloc_port)
NODE_NET_PORT=$(alloc_port)

SIGNATORY1_INSTANCE="test-signatory-multi-1"
SIGNATORY2_INSTANCE="test-signatory-multi-2"
SIGNATORY3_INSTANCE="test-signatory-multi-3"
NODE_INSTANCE="test-node-multi"
BAKER1_INSTANCE="test-baker-multi-1"
BAKER2_INSTANCE="test-baker-multi-2"

# Register all instances for cleanup
register_instance "$SIGNATORY1_INSTANCE"
register_instance "$SIGNATORY2_INSTANCE"
register_instance "$SIGNATORY3_INSTANCE"
register_instance "$NODE_INSTANCE"
register_instance "$BAKER1_INSTANCE"
register_instance "$BAKER2_INSTANCE"

echo "==> Step 1: Install three signatory instances with different configurations"

om install-signatory \
	--instance "$SIGNATORY1_INSTANCE" \
	--backend file \
	--address "127.0.0.1:$SIGNATORY1_PORT" \
	--authorized-keys "tz1VSUr8wwNhLAzempoch5d6hLRiTh8Cjcjb" \
	--watermark file \
	--app-bin-dir /usr/local/bin \
	--service-user tezos \
	--no-enable 2>&1

om install-signatory \
	--instance "$SIGNATORY2_INSTANCE" \
	--backend file \
	--address "127.0.0.1:$SIGNATORY2_PORT" \
	--authorized-keys "tz2TSvNTh2epDMhZHrw73nV9piBX7kLZ9K9m" \
	--watermark memory \
	--app-bin-dir /usr/local/bin \
	--service-user tezos \
	--no-enable 2>&1

om install-signatory \
	--instance "$SIGNATORY3_INSTANCE" \
	--backend file \
	--address "127.0.0.1:$SIGNATORY3_PORT" \
	--authorized-keys "tz3RDC3Jdn4j15J7bBHZd29EUee9gVB1CxD9" \
	--app-bin-dir /usr/local/bin \
	--service-user tezos \
	--no-enable 2>&1

echo "==> Step 2: Verify signatory service template was created"
# Check if the unit file exists (list-unit-files may not show it immediately after install)
UNIT_FILE="/etc/systemd/system/signatory@.service"
if [ ! -f "$UNIT_FILE" ]; then
	echo "ERROR: Signatory service template not found: $UNIT_FILE"
	echo "Checking /etc/systemd/system:"
	ls -la /etc/systemd/system/signatory* 2>&1 || echo "No signatory files found"
	exit 1
fi
echo "Signatory service template exists: $UNIT_FILE"

echo "==> Step 3: Verify list shows all signatory instances"
om list 2>&1 | tee /tmp/signatory-list.txt

for instance in "$SIGNATORY1_INSTANCE" "$SIGNATORY2_INSTANCE" "$SIGNATORY3_INSTANCE"; do
	if ! grep -q "$instance" /tmp/signatory-list.txt; then
		echo "ERROR: Instance $instance not in list output"
		cat /tmp/signatory-list.txt
		exit 1
	fi
	# Verify it's listed as signatory role
	if ! om list 2>&1 | grep "$instance" | grep -q "signatory"; then
		echo "ERROR: Instance $instance not listed as signatory role"
		om list 2>&1 | grep "$instance"
		exit 1
	fi
done

echo "==> Step 4: Verify each signatory has unique configuration"
CONFIG1="/var/lib/octez/signatory/${SIGNATORY1_INSTANCE}/signatory.yaml"
CONFIG2="/var/lib/octez/signatory/${SIGNATORY2_INSTANCE}/signatory.yaml"
CONFIG3="/var/lib/octez/signatory/${SIGNATORY3_INSTANCE}/signatory.yaml"

# Check each config has different authorized keys
if ! grep -q "tz1VSUr8wwNhLAzempoch5d6hLRiTh8Cjcjb" "$CONFIG1"; then
	echo "ERROR: Signatory 1 config missing tz1 key"
	exit 1
fi

if ! grep -q "tz2TSvNTh2epDMhZHrw73nV9piBX7kLZ9K9m" "$CONFIG2"; then
	echo "ERROR: Signatory 2 config missing tz2 key"
	exit 1
fi

if ! grep -q "tz3RDC3Jdn4j15J7bBHZd29EUee9gVB1CxD9" "$CONFIG3"; then
	echo "ERROR: Signatory 3 config missing tz3 key"
	exit 1
fi

# Check different watermark settings
if ! grep -q "file" "$CONFIG1"; then
	echo "ERROR: Signatory 1 should have file watermark"
	cat "$CONFIG1"
	exit 1
fi

if ! grep -q "driver: mem" "$CONFIG2"; then
	echo "ERROR: Signatory 2 should have mem watermark driver"
	cat "$CONFIG2"
	exit 1
fi

echo "==> Step 5: Install node instance"
om install-node \
	--instance "$NODE_INSTANCE" \
	--network shadownet \
	--snapshot \
	--snapshot-no-check \
	--snapshot-uri "$SANDBOX_URL/snapshot.rolling" \
	--rpc-addr "127.0.0.1:$NODE_RPC_PORT" \
	--net-addr "127.0.0.1:$NODE_NET_PORT" \
	--service-user tezos \
	--no-enable 2>&1

echo "==> Step 6: Install two bakers using different signatories"
om install-baker \
	--instance "$BAKER1_INSTANCE" \
	--node-instance "$NODE_INSTANCE" \
	--remote-signer-instance "$SIGNATORY1_INSTANCE" \
	--delegate "tz1VSUr8wwNhLAzempoch5d6hLRiTh8Cjcjb" \
	--liquidity-baking-vote pass \
	--service-user tezos \
	--no-enable 2>&1

om install-baker \
	--instance "$BAKER2_INSTANCE" \
	--node-instance "$NODE_INSTANCE" \
	--remote-signer-instance "$SIGNATORY2_INSTANCE" \
	--delegate "tz2TSvNTh2epDMhZHrw73nV9piBX7kLZ9K9m" \
	--liquidity-baking-vote pass \
	--service-user tezos \
	--no-enable 2>&1

echo "==> Step 7: Verify each baker depends on correct signatory"
systemctl daemon-reload

BAKER1_UNIT="octez-baker@${BAKER1_INSTANCE}.service"
BAKER2_UNIT="octez-baker@${BAKER2_INSTANCE}.service"

# Baker 1 -> Signatory 1
if ! systemctl list-dependencies "$BAKER1_UNIT" | grep -q "signatory@${SIGNATORY1_INSTANCE}.service"; then
	echo "ERROR: Baker1 not dependent on signatory1"
	systemctl list-dependencies "$BAKER1_UNIT"
	exit 1
fi

# Baker 2 -> Signatory 2
if ! systemctl list-dependencies "$BAKER2_UNIT" | grep -q "signatory@${SIGNATORY2_INSTANCE}.service"; then
	echo "ERROR: Baker2 not dependent on signatory2"
	systemctl list-dependencies "$BAKER2_UNIT"
	exit 1
fi

echo "==> Step 8: Verify cross-contamination - baker1 should NOT depend on signatory2 or signatory3"
if systemctl list-dependencies "$BAKER1_UNIT" | grep -q "signatory@${SIGNATORY2_INSTANCE}.service"; then
	echo "ERROR: Baker1 incorrectly depends on signatory2"
	systemctl list-dependencies "$BAKER1_UNIT"
	exit 1
fi

if systemctl list-dependencies "$BAKER1_UNIT" | grep -q "signatory@${SIGNATORY3_INSTANCE}.service"; then
	echo "ERROR: Baker1 incorrectly depends on signatory3"
	systemctl list-dependencies "$BAKER1_UNIT"
	exit 1
fi

echo "==> Step 9: Verify all systemd units use correct naming (signatory@, not octez-signatory@)"
for instance in "$SIGNATORY1_INSTANCE" "$SIGNATORY2_INSTANCE" "$SIGNATORY3_INSTANCE"; do
	DROPIN_FILES=$(find /etc/systemd/system -name "*.conf" -type f 2>/dev/null || true)

	if echo "$DROPIN_FILES" | xargs grep -l "signatory@${instance}" 2>/dev/null | xargs grep -q "octez-signatory@${instance}" 2>/dev/null; then
		echo "ERROR: Found incorrect octez-signatory@ reference for $instance"
		grep -r "octez-signatory@${instance}" /etc/systemd/system/ || true
		exit 1
	fi
done

echo "Test passed: Multiple signatory instances working correctly with correct naming"
