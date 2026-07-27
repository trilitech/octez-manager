#!/bin/bash
# Test: 'instance purge --force-purge' on a baker whose env file is
# missing the OCTEZ_BAKER_BASE_DIR key must still complete cleanly.
#
# Regression test for https://github.com/trilitech/octez-manager/issues/1007
#
# WHY THIS IS RED ON MAIN: `purge_service` in src/installer/removal.ml
# (~lines 137-151) fetches the baker/accuser base directory with a
# partial `List.assoc "OCTEZ_BAKER_BASE_DIR" env`. If the env file is
# missing that key -- because `Node_env.read` failed (env falls back to
# `[]`) or because the key is simply absent from an otherwise-readable
# file (write_pairs drops empty values; a hand-edited file) -- the
# `List.assoc` raises an uncaught `Not_found` that crashes the CLI
# instead of purging. This is precisely the "clean up a broken
# instance" flow, so the crash defeats the one command meant to recover
# from a bad install state. Reproduced locally against this exact code
# path (XDG-scoped user-mode install, real binary): after deleting the
# OCTEZ_BAKER_BASE_DIR line, `purge --force-purge` printed
# "octez-manager: internal error, uncaught exception: Not_found" and
# exited 125, leaving the base-dir and per-instance env directory on
# disk uncleaned.
set -euo pipefail
source /tests/lib.sh

test_init "Purge with missing OCTEZ_BAKER_BASE_DIR env key (#1007)"

NODE_INSTANCE="test-purge-missing-basedir-node"
BAKER_INSTANCE="test-purge-missing-basedir-baker"
NODE_RPC="127.0.0.1:$(alloc_port)"
NODE_NET="0.0.0.0:$(alloc_port)"

register_instance "$NODE_INSTANCE"
register_instance "$BAKER_INSTANCE"

echo "Installing node..."
om install-node \
	--instance "$NODE_INSTANCE" \
	--network shadownet \
	--rpc-addr "$NODE_RPC" \
	--net-addr "$NODE_NET" \
	--service-user tezos \
	--no-enable 2>&1

echo "Installing baker depending on node..."
om install-baker \
	--instance "$BAKER_INSTANCE" \
	--node-instance "$NODE_INSTANCE" \
	--liquidity-baking-vote pass \
	--service-user tezos \
	--no-enable 2>&1

if ! instance_exists "$BAKER_INSTANCE"; then
	echo "ERROR: Baker instance not created"
	exit 1
fi

BAKER_ENV="/etc/octez/instances/$BAKER_INSTANCE/node.env"
if ! grep -q "^OCTEZ_BAKER_BASE_DIR=" "$BAKER_ENV"; then
	echo "ERROR: Baker env file does not have OCTEZ_BAKER_BASE_DIR before corruption"
	cat "$BAKER_ENV"
	exit 1
fi

# --- Corrupt the precondition: drop the base-dir key deterministically,
# simulating a hand-edited or partially-written env file (issue #1007).
echo "Removing OCTEZ_BAKER_BASE_DIR from baker env file..."
sed -i '/^OCTEZ_BAKER_BASE_DIR=/d' "$BAKER_ENV"

if grep -q "^OCTEZ_BAKER_BASE_DIR=" "$BAKER_ENV"; then
	echo "ERROR: Failed to remove OCTEZ_BAKER_BASE_DIR from env file"
	exit 1
fi
echo "Baker env file corrupted (OCTEZ_BAKER_BASE_DIR removed)"

# --- Purge must complete cleanly. With --force-purge, force_purge=true
# short-circuits the interactive prompt entirely in purge_service (the
# `prompt_yes_no` passed by cmd_instance.ml when non-interactive is
# `fun _ ~default:_ -> false`, but it is never even reached here), so
# this exercises the base-dir lookup itself, not prompt plumbing.
echo "Purging baker with corrupted env (should succeed cleanly)..."
set +e
output=$(om instance "$BAKER_INSTANCE" purge --force-purge 2>&1)
rc=$?
set -e

echo "$output"

# --- Assert the CORRECT behavior: purge completes cleanly.
if [ "$rc" -ne 0 ]; then
	echo "ERROR: purge --force-purge exited non-zero ($rc) on a baker with"
	echo "a missing OCTEZ_BAKER_BASE_DIR key -- this is issue #1007: an"
	echo "uncaught Not_found from List.assoc in purge_service crashes the"
	echo "CLI instead of purging cleanly."
	exit 1
fi

if [[ "$output" == *"Not_found"* ]]; then
	echo "ERROR: purge output contains an uncaught Not_found exception (#1007)"
	exit 1
fi

if [[ "$output" == *"uncaught exception"* ]]; then
	echo "ERROR: purge output contains an uncaught exception trace (#1007)"
	exit 1
fi

if [[ "$output" == *"Fatal error"* ]]; then
	echo "ERROR: purge output contains a Fatal error trace (#1007)"
	exit 1
fi

if [[ "$output" == *"internal error"* ]]; then
	echo "ERROR: purge reported an internal error instead of completing (#1007)"
	exit 1
fi

BAKER_REGISTRY_FILE="/etc/octez_manager/services/${BAKER_INSTANCE}.json"
if [ -f "$BAKER_REGISTRY_FILE" ]; then
	echo "ERROR: Baker registry file still present after purge: $BAKER_REGISTRY_FILE"
	exit 1
fi
echo "Baker purged cleanly, registry file removed"

echo "Purge-with-missing-base-dir test passed"
