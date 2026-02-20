#!/bin/bash
set -euo pipefail
source /tests/lib.sh

echo "Test: External Signer URI Configuration"

# Initialize test harness (automatic cleanup on EXIT)
test_init

# Allocate unique ports for this test
NODE_RPC_PORT=$(alloc_port)
NODE_NET_PORT=$(alloc_port)
EXTERNAL_SIGNER_PORT=$(alloc_port)

NODE_INSTANCE="test-node-external-signer"
BAKER_INSTANCE="test-baker-external-signer"

# Register instances for cleanup
register_instance "$NODE_INSTANCE"
register_instance "$BAKER_INSTANCE"

echo "==> Step 1: Install node instance"
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

echo "==> Step 2: Start node to enable baker installation"
om start --instance "$NODE_INSTANCE" 2>&1

# Wait for node RPC to be ready
echo "Waiting for node RPC at 127.0.0.1:$NODE_RPC_PORT..."
for i in {1..30}; do
	if curl -s "http://127.0.0.1:$NODE_RPC_PORT/chains/main/blocks/head" >/dev/null 2>&1; then
		echo "Node RPC is ready"
		break
	fi
	if [ $i -eq 30 ]; then
		echo "ERROR: Node RPC did not become ready"
		exit 1
	fi
	sleep 1
done

echo "==> Step 3: Install baker with external signer URI (http://)"
EXTERNAL_SIGNER_URI="http://127.0.0.1:$EXTERNAL_SIGNER_PORT"

om install-baker \
	--instance "$BAKER_INSTANCE" \
	--node-instance "$NODE_INSTANCE" \
	--remote-signer-uri "$EXTERNAL_SIGNER_URI" \
	--delegate "tz1VSUr8wwNhLAzempoch5d6hLRiTh8Cjcjb" \
	--liquidity-baking-vote pass \
	--service-user tezos \
	--no-enable 2>&1

echo "==> Step 4: Verify baker installation succeeded"
# Baker is installed successfully if registry entry exists
if ! om list 2>&1 | grep -q "$BAKER_INSTANCE"; then
	echo "ERROR: Baker instance '$BAKER_INSTANCE' not found in om list"
	om list 2>&1
	exit 1
fi

echo "==> Step 5: Verify baker configuration includes signer URI"
BAKER_CONFIG_FILE="/var/lib/tezos/.tezos-baker/${BAKER_INSTANCE}/config"

if [ ! -f "$BAKER_CONFIG_FILE" ]; then
	echo "ERROR: Baker config file not found: $BAKER_CONFIG_FILE"
	exit 1
fi

if ! grep -q "$EXTERNAL_SIGNER_URI" "$BAKER_CONFIG_FILE"; then
	echo "ERROR: External signer URI not found in baker config"
	cat "$BAKER_CONFIG_FILE"
	exit 1
fi

echo "==> Step 6: Verify NO signatory dependency in systemd drop-in"
# When using external signer URI, there should be no signatory dependency
DROPIN_DIR="/etc/systemd/system/${BAKER_UNIT}.d"
DROPIN_FILE="${DROPIN_DIR}/dependencies.conf"

if [ -f "$DROPIN_FILE" ]; then
	# If drop-in exists, it should NOT reference any signatory service
	if grep -q "signatory@" "$DROPIN_FILE"; then
		echo "ERROR: Baker with external URI should not depend on signatory service"
		cat "$DROPIN_FILE"
		exit 1
	fi

	# Should still depend on the node though
	if ! grep -q "octez-node@${NODE_INSTANCE}.service" "$DROPIN_FILE"; then
		echo "ERROR: Baker should still depend on node"
		cat "$DROPIN_FILE"
		exit 1
	fi
fi

echo "==> Step 7: Verify systemd dependencies"
systemctl daemon-reload

# Should depend on node only, not signatory
if ! systemctl list-dependencies "$BAKER_UNIT" | grep -q "octez-node@${NODE_INSTANCE}.service"; then
	echo "ERROR: Baker not dependent on node"
	systemctl list-dependencies "$BAKER_UNIT"
	exit 1
fi

if systemctl list-dependencies "$BAKER_UNIT" | grep -q "signatory@"; then
	echo "ERROR: Baker with external URI should not depend on local signatory"
	systemctl list-dependencies "$BAKER_UNIT"
	exit 1
fi

echo "==> Step 8: Test baker info command shows signer URI"
om info-baker --instance "$BAKER_INSTANCE" 2>&1 | tee /tmp/baker-info.txt

if ! grep -q "$EXTERNAL_SIGNER_URI" /tmp/baker-info.txt && ! grep -q "$EXTERNAL_SIGNER_PORT" /tmp/baker-info.txt; then
	echo "WARNING: Signer URI not visible in baker info (may be expected)"
	cat /tmp/baker-info.txt
fi

echo "==> Step 9: Verify baker key is configured"
if ! grep -q "tz1VSUr8wwNhLAzempoch5d6hLRiTh8Cjcjb" "$BAKER_CONFIG_FILE"; then
	echo "ERROR: Baker key not found in config"
	cat "$BAKER_CONFIG_FILE"
	exit 1
fi

echo "Test passed: External signer URI configuration working correctly"
