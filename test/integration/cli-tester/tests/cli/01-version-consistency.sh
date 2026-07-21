#!/bin/bash
# Test: 'octez-manager version' and 'octez-manager --version' report the same version
# Regression test for https://github.com/trilitech/octez-manager/issues/967
# where 'version' printed a stale hardcoded "v0.3.0" while '--version'
# printed the real build version.
set -euo pipefail
source /tests/lib.sh

test_init "Version consistency between 'version' and '--version'"

flag_version=$(om --version)
# 'version' may append update-availability lines; the version is on line 1.
subcmd_version=$(om version | head -1)

echo "--version:          $flag_version"
echo "version subcommand: $subcmd_version"

if [ -z "$flag_version" ]; then
	echo "ERROR: --version printed nothing"
	exit 1
fi

# Both commands must produce byte-identical version output.
assert_eq "$flag_version" "$subcmd_version" \
	"'version' subcommand must print the same output as '--version'"

# The stale hardcoded version must never come back.
if [[ "$subcmd_version" == *"0.3.0"* ]] && [[ "$flag_version" != "0.3.0" ]]; then
	echo "ERROR: 'version' still reports the stale hardcoded v0.3.0"
	exit 1
fi

echo "Test passed"
