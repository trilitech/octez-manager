#!/bin/bash
# Test: 'octez-manager instance' without arguments errors like its sibling
# command groups, and its --help documents the available actions.
# Regression test for https://github.com/trilitech/octez-manager/issues/969
# where the no-args invocation dumped the top-level man page with exit 0 and
# the help never listed the actions.
set -euo pipefail
source /tests/lib.sh

test_init "instance no-args error and action documentation"

# No arguments: must fail with a usage error, not print the top-level help
# with exit 0.
set +e
output=$(om instance 2>&1)
exit_code=$?
set -e

if [ "$exit_code" -eq 0 ]; then
	echo "ERROR: 'instance' with no args exited 0 (expected non-zero)"
	echo "Output: $output"
	exit 1
fi

assert_contains "$output" "INSTANCE required" \
	"expected a usage error mentioning the missing INSTANCE"

if [[ "$output" == *"Terminal UI for managing Octez services"* ]]; then
	echo "ERROR: 'instance' with no args printed the top-level help page"
	exit 1
fi
echo "No-args invocation errors correctly (exit $exit_code)"

# --help must document the available actions.
help_output=$(om instance --help 2>&1)
for action in start stop restart remove purge show logs export-logs edit set-env get-env; do
	assert_contains "$help_output" "$action" \
		"expected 'instance --help' to document the '$action' action"
done
echo "Help documents all actions"

echo "Test passed"
