#!/bin/bash
set -euo pipefail
source /tests/lib.sh

echo "Test: Edit Existing Baker Signer Configuration"

# Initialize test harness (automatic cleanup on EXIT)
test_init

# Allocate unique ports for this test
SIGNATORY1_PORT=$(alloc_port)
SIGNATORY2_PORT=$(alloc_port)
NODE_RPC_PORT=$(alloc_port)
NODE_NET_PORT=$(alloc_port)
EXTERNAL_SIGNER_PORT=$(alloc_port)

SIGNATORY1_INSTANCE="test-signatory-edit-1"
SIGNATORY2_INSTANCE="test-signatory-edit-2"
NODE_INSTANCE="test-node-edit"
BAKER_INSTANCE="test-baker-edit-signer"

echo "==> Step 1: Install two signatory instances"
om install-signatory \
	--instance "$SIGNATORY1_INSTANCE" \
	--backend file \
	--address "127.0.0.1:$SIGNATORY1_PORT" \
	--authorized-keys "tz1VSUr8wwNhLAzempoch5d6hLRiTh8Cjcjb" \
	--service-user tezos \
	--no-enable 2>&1

register_instance "$SIGNATORY1_INSTANCE"

om install-signatory \
	--instance "$SIGNATORY2_INSTANCE" \
	--backend file \
	--address "127.0.0.1:$SIGNATORY2_PORT" \
	--authorized-keys "tz2TSvNTh2epDMhZHrw73nV9piBX7kLZ9K9m" \
	--service-user tezos \
	--no-enable 2>&1

register_instance "$SIGNATORY2_INSTANCE"

echo "==> Step 2: Install node instance"
om install-node \
	--instance "$NODE_INSTANCE" \
	--network ghostnet \
	--snapshot \
	--snapshot-no-check \
	--snapshot-uri "$SANDBOX_URL/snapshot.rolling" \
	--rpc-addr "127.0.0.1:$NODE_RPC_PORT" \
	--net-addr "127.0.0.1:$NODE_NET_PORT" \
	--service-user tezos \
	--no-enable 2>&1

register_instance "$NODE_INSTANCE"

echo "==> Step 3: Install baker with first signatory"
om install-baker \
	--instance "$BAKER_INSTANCE" \
	--node-instance "$NODE_INSTANCE" \
	--protocol alpha \
	--use-signatory "$SIGNATORY1_INSTANCE" \
	--baker-key "tz1VSUr8wwNhLAzempoch5d6hLRiTh8Cjcjb" \
	--service-user tezos \
	--no-enable 2>&1

register_instance "$BAKER_INSTANCE"

echo "==> Step 4: Verify initial signatory dependency"
BAKER_UNIT="octez-baker@${BAKER_INSTANCE}.service"
DROPIN_FILE="/etc/systemd/system/${BAKER_UNIT}.d/dependencies.conf"

systemctl daemon-reload

if ! systemctl list-dependencies "$BAKER_UNIT" | grep -q "signatory@${SIGNATORY1_INSTANCE}.service"; then
	echo "ERROR: Baker not initially dependent on signatory1"
	systemctl list-dependencies "$BAKER_UNIT"
	exit 1
fi

echo "==> Step 5: Edit baker to use second signatory"
om edit-baker \
	--instance "$BAKER_INSTANCE" \
	--use-signatory "$SIGNATORY2_INSTANCE" \
	--baker-key "tz2TSvNTh2epDMhZHrw73nV9piBX7kLZ9K9m" 2>&1

echo "==> Step 6: Verify signatory dependency was updated"
systemctl daemon-reload

if ! systemctl list-dependencies "$BAKER_UNIT" | grep -q "signatory@${SIGNATORY2_INSTANCE}.service"; then
	echo "ERROR: Baker not updated to depend on signatory2"
	systemctl list-dependencies "$BAKER_UNIT"
	exit 1
fi

# Should NOT depend on signatory1 anymore
if systemctl list-dependencies "$BAKER_UNIT" | grep -q "signatory@${SIGNATORY1_INSTANCE}.service"; then
	echo "ERROR: Baker still depends on old signatory1"
	systemctl list-dependencies "$BAKER_UNIT"
	exit 1
fi

echo "==> Step 7: Verify drop-in file was updated"
if ! grep -q "signatory@${SIGNATORY2_INSTANCE}.service" "$DROPIN_FILE"; then
	echo "ERROR: Drop-in not updated to reference signatory2"
	cat "$DROPIN_FILE"
	exit 1
fi

if grep -q "signatory@${SIGNATORY1_INSTANCE}.service" "$DROPIN_FILE"; then
	echo "ERROR: Drop-in still references old signatory1"
	cat "$DROPIN_FILE"
	exit 1
fi

echo "==> Step 8: Edit baker to use external signer URI instead"
EXTERNAL_SIGNER_URI="http://127.0.0.1:$EXTERNAL_SIGNER_PORT"

om edit-baker \
	--instance "$BAKER_INSTANCE" \
	--signer-uri "$EXTERNAL_SIGNER_URI" \
	--baker-key "tz1VSUr8wwNhLAzempoch5d6hLRiTh8Cjcjb" 2>&1

echo "==> Step 9: Verify signatory dependency was removed"
systemctl daemon-reload

# Should NOT depend on any signatory anymore
if systemctl list-dependencies "$BAKER_UNIT" | grep -q "signatory@"; then
	echo "ERROR: Baker still depends on signatory after switching to external URI"
	systemctl list-dependencies "$BAKER_UNIT"
	exit 1
fi

# Drop-in should no longer reference signatory
if [ -f "$DROPIN_FILE" ] && grep -q "signatory@" "$DROPIN_FILE"; then
	echo "ERROR: Drop-in still references signatory after switching to external URI"
	cat "$DROPIN_FILE"
	exit 1
fi

echo "==> Step 10: Verify baker config has external URI"
BAKER_CONFIG_FILE="/var/lib/tezos/.tezos-baker/${BAKER_INSTANCE}/config"

if ! grep -q "$EXTERNAL_SIGNER_URI" "$BAKER_CONFIG_FILE"; then
	echo "ERROR: Baker config does not have external signer URI"
	cat "$BAKER_CONFIG_FILE"
	exit 1
fi

echo "==> Step 11: Switch back to signatory to verify bidirectional editing"
om edit-baker \
	--instance "$BAKER_INSTANCE" \
	--use-signatory "$SIGNATORY1_INSTANCE" \
	--baker-key "tz1VSUr8wwNhLAzempoch5d6hLRiTh8Cjcjb" 2>&1

systemctl daemon-reload

if ! systemctl list-dependencies "$BAKER_UNIT" | grep -q "signatory@${SIGNATORY1_INSTANCE}.service"; then
	echo "ERROR: Baker not updated back to signatory1"
	systemctl list-dependencies "$BAKER_UNIT"
	exit 1
fi

echo "Test passed: Baker signer configuration editing working correctly"
