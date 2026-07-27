#!/bin/bash
# Test: enable-on-boot state is persisted to the service registry at
# install time (both --no-enable and default/enabled), and survives a
# subsequent 'instance edit' round-trip without being silently
# re-enabled (regression test for #1001).
#
# Before the fix, every install wizard's edit-mode prefill (and the CLI
# edit path in cmd_instance.ml) hardcoded auto_enable = true, because
# the applied systemd enable state was never persisted anywhere. So
# editing an intentionally-disabled instance silently re-enabled it at
# boot on the next save. Service.t now has an enabled_on_boot field
# that installers record, and edit paths read it back instead of
# hardcoding true.
#
# Edit round-trip coverage: 'om instance <inst> edit' runs fully
# non-interactively here because stdin is not a tty in this harness --
# Cli_helpers.is_interactive () is false, so every prompt_input /
# prompt_with_completion / prompt_yes_no call in cmd_instance.ml
# returns None (or the non-interactive default) immediately, and every
# field falls back to Option.value ~default:<current value> ..., in
# particular:
#   auto_enable = Option.value ~default:true svc.Service.enabled_on_boot
# (src/cli/cmd_instance.ml, node role branch of the 'edit' handler).
# This means an unattended 'edit' exercises exactly the post-#1001-fix
# seeding logic: without the fix this line was hardcoded to
# 'auto_enable = true' and the edit below would re-enable the unit.
set -euo pipefail
source /tests/lib.sh

test_init "Enable-on-boot state persists across install and edit"

DISABLED_INSTANCE="test-boot-disabled"
ENABLED_INSTANCE="test-boot-enabled"
DISABLED_RPC="127.0.0.1:$(alloc_port)"
DISABLED_NET="0.0.0.0:$(alloc_port)"
ENABLED_RPC="127.0.0.1:$(alloc_port)"
ENABLED_NET="0.0.0.0:$(alloc_port)"

register_instance "$DISABLED_INSTANCE"
register_instance "$ENABLED_INSTANCE"

DISABLED_REGISTRY="/etc/octez_manager/services/${DISABLED_INSTANCE}.json"
ENABLED_REGISTRY="/etc/octez_manager/services/${ENABLED_INSTANCE}.json"

# --- Step 1: install a node with --no-enable (no snapshot needed --
# we never start it) and assert both the systemd unit and the
# registry record it as disabled.
echo "Installing node with --no-enable..."
om install-node \
	--instance "$DISABLED_INSTANCE" \
	--network shadownet \
	--rpc-addr "$DISABLED_RPC" \
	--net-addr "$DISABLED_NET" \
	--service-user tezos \
	--no-enable 2>&1

if service_is_enabled node "$DISABLED_INSTANCE"; then
	echo "ERROR: systemd unit should be disabled after --no-enable install"
	exit 1
fi

assert_file_exists "$DISABLED_REGISTRY" \
	"registry file should exist after install"

disabled_flag=$(jq -r '.enabled_on_boot' "$DISABLED_REGISTRY")
assert_eq "false" "$disabled_flag" \
	"registry enabled_on_boot should be false after --no-enable install"
echo "Disabled instance: unit disabled, registry enabled_on_boot=false"

# --- Step 2: edit the instance non-interactively (see header) and
# assert it is STILL disabled afterwards. This is the actual #1001
# regression: pre-fix, this edit silently re-enabled the unit.
echo "Editing disabled instance non-interactively..."
om instance "$DISABLED_INSTANCE" edit 2>&1

if service_is_enabled node "$DISABLED_INSTANCE"; then
	echo "ERROR: edit re-enabled a unit that was explicitly disabled (#1001 regression)"
	exit 1
fi

disabled_flag_after_edit=$(jq -r '.enabled_on_boot' "$DISABLED_REGISTRY")
assert_eq "false" "$disabled_flag_after_edit" \
	"registry enabled_on_boot should still be false after edit"
echo "Disabled instance stayed disabled through edit round-trip"

# --- Step 3: positive case -- install another instance WITHOUT
# --no-enable and assert the registry records enabled_on_boot: true.
# No --snapshot is passed either, so bootstrap is Genesis (no
# download); we only need the enable state, not a synced node, so we
# stop it again immediately after asserting.
echo "Installing node without --no-enable (default enabled)..."
om install-node \
	--instance "$ENABLED_INSTANCE" \
	--network shadownet \
	--rpc-addr "$ENABLED_RPC" \
	--net-addr "$ENABLED_NET" \
	--service-user tezos 2>&1

assert_file_exists "$ENABLED_REGISTRY" \
	"registry file should exist after install"

enabled_flag=$(jq -r '.enabled_on_boot' "$ENABLED_REGISTRY")
assert_eq "true" "$enabled_flag" \
	"registry enabled_on_boot should be true for default (enabled) install"
echo "Enabled instance: registry enabled_on_boot=true"

om instance "$ENABLED_INSTANCE" stop 2>&1 || true

echo "Enable-on-boot persistence test passed"
