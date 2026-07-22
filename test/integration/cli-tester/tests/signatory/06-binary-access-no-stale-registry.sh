#!/bin/bash
# Test: A failed signatory install must not leave a stale registry entry
# Regression test for https://github.com/trilitech/octez-manager/issues/987
# where installers wrote the service registry entry before the binary-access
# validation in Systemd.install_unit; a failed install left a ghost instance
# occupying the instance name and address. Covers the non-node installers
# (the node variant is covered by node/25-binary-access-validation.sh).
set -euo pipefail
source /tests/lib.sh

test_init "Failed signatory install leaves no stale registry entry"

INSTANCE="test-sig-bin-access"
RESTRICTED_DIR="/tmp/restricted-signatory-bin"

# Register instance and data dir for automatic cleanup (also pre-cleans leftovers)
register_instance "$INSTANCE"
register_data_dir "$RESTRICTED_DIR"

SIGNER_PORT=$(alloc_port)
METRICS_PORT=$(alloc_port)

install_signatory() {
	om install-signatory \
		--instance "$INSTANCE" \
		--backend file \
		--authorized-keys "tz1KqTpEZ7Yob7QbPE4Hy4Wo8fHG8LhKxZSx" \
		--address "127.0.0.1:$SIGNER_PORT" \
		--metrics-address "127.0.0.1:$METRICS_PORT" \
		--app-bin-dir "$1" \
		--service-user tezos \
		--no-enable 2>&1
}

# Step 1: Install from a directory the service user cannot traverse — must fail.
echo "Step 1: Installing with restricted binary directory (should fail)"
mkdir -p "$RESTRICTED_DIR"
cp /usr/local/bin/signatory "$RESTRICTED_DIR/"
chmod 755 "$RESTRICTED_DIR/signatory"
chmod 700 "$RESTRICTED_DIR" # Owner (root) only

if install_signatory "$RESTRICTED_DIR"; then
	echo "ERROR: Installation should have failed with restricted directory"
	exit 1
fi
echo "✓ Installation correctly rejected restricted directory"

# Step 2: The failed install must not have left a registry entry behind.
LIST_OUTPUT=$(om list 2>&1)
if [[ "$LIST_OUTPUT" == *"$INSTANCE"* ]]; then
	echo "ERROR: Failed install left a stale registry entry"
	echo "$LIST_OUTPUT"
	exit 1
fi
echo "✓ No stale registry entry after failed install"

# Step 3: Installing again with accessible binaries and the SAME instance
# name and ports must succeed (a stale entry would block the name/address).
echo "Step 3: Reinstalling with accessible binaries (should succeed)"
install_signatory /usr/local/bin

if ! instance_exists "$INSTANCE"; then
	echo "ERROR: Reinstall failed after a previously failed install"
	exit 1
fi
echo "✓ Reinstall succeeded with the same instance name and address"

echo "Test passed"
