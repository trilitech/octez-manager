#!/bin/bash
# Test: Bash completion offers subcommands for group commands
# Verifies that 'octez-manager baker <TAB>' offers baker subcommands,
# not just global flags — regression guard for the group-completion fix.
set -euo pipefail
source /tests/lib.sh

test_init "Bash completion: group commands offer subcommands"

COMPLETION_BASH="/tests/completion/fixtures/octez-manager.bash"

# shellcheck disable=SC1090
source "$COMPLETION_BASH"

# Helper: simulate bash completion for the given COMP_WORDS and return results
run_completion() {
  COMP_WORDS=("$@")
  COMP_CWORD=$(( ${#COMP_WORDS[@]} - 1 ))
  COMPREPLY=()
  _octez_manager 2>/dev/null || true
  echo "${COMPREPLY[*]:-}"
}

# baker subcommands at depth 2
result=$(run_completion "octez-manager" "baker" "")
assert_contains "$result" "list"     "baker <TAB> should offer 'list'"
assert_contains "$result" "status"   "baker <TAB> should offer 'status'"
assert_contains "$result" "register" "baker <TAB> should offer 'register'"

# rewards subcommands at depth 2
result=$(run_completion "octez-manager" "rewards" "")
assert_contains "$result" "generate" "rewards <TAB> should offer 'generate'"
assert_contains "$result" "pay"      "rewards <TAB> should offer 'pay'"

# group subcommands at depth 2
result=$(run_completion "octez-manager" "group" "")
assert_contains "$result" "create" "group <TAB> should offer 'create'"
assert_contains "$result" "list"   "group <TAB> should offer 'list'"

# binaries subcommands at depth 2
result=$(run_completion "octez-manager" "binaries" "")
assert_contains "$result" "list"     "binaries <TAB> should offer 'list'"
assert_contains "$result" "download" "binaries <TAB> should offer 'download'"

# rpc subcommands at depth 2 (US3: all 7 affected groups)
result=$(run_completion "octez-manager" "rpc" "")
assert_contains "$result" "get"       "rpc <TAB> should offer 'get'"
assert_contains "$result" "instances" "rpc <TAB> should offer 'instances'"

# sandbox subcommands at depth 2 (US3: all 7 affected groups)
result=$(run_completion "octez-manager" "sandbox" "")
assert_contains "$result" "create"  "sandbox <TAB> should offer 'create'"
assert_contains "$result" "destroy" "sandbox <TAB> should offer 'destroy'"

# top-level: both group commands and leaf commands appear
result=$(run_completion "octez-manager" "")
assert_contains "$result" "baker"        "top-level <TAB> should offer 'baker'"
assert_contains "$result" "install-node" "top-level <TAB> should offer 'install-node'"

# baker with -- prefix: only flags, not subcommands
result=$(run_completion "octez-manager" "baker" "--")
assert_contains "$result" "--help" "baker --<TAB> should offer '--help'"

echo "Test passed"
