#!/bin/bash
# Test: Bash completion offers full:50 as a candidate for --snapshot-kind
# Verifies SC-007: the full:50 snapshot kind value survives the completion
# generator and is selectable via tab completion.
set -euo pipefail
source /tests/lib.sh

test_init "Bash completion: --snapshot-kind offers full:50"

COMPLETION_BASH="/tests/completion/fixtures/octez-manager.bash"

# shellcheck disable=SC1090
source "$COMPLETION_BASH"

# Simulate: octez-manager install-node --snapshot-kind <TAB>
# The prev/cur handler at the top of _octez_manager intercepts --snapshot-kind
# before any subcommand dispatch, so this works at COMP_CWORD=3.
COMP_WORDS=("octez-manager" "install-node" "--snapshot-kind" "")
COMP_CWORD=3
COMPREPLY=()
_octez_manager 2>/dev/null || true

result="${COMPREPLY[*]:-}"
assert_contains "$result" "full:50"  "--snapshot-kind <TAB> should offer 'full:50'"
assert_contains "$result" "rolling"  "--snapshot-kind <TAB> should offer 'rolling'"
assert_contains "$result" "archive"  "--snapshot-kind <TAB> should offer 'archive'"
assert_contains "$result" "full"     "--snapshot-kind <TAB> should offer 'full'"

echo "Test passed"
