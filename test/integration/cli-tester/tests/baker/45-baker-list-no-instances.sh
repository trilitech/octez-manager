#!/bin/bash
# Test: 'baker list' with no baker instances exits cleanly
# Verifies that 'octez-manager baker list' does not crash with
# "capability missing: service_manager" when no bakers are installed.
set -euo pipefail
source /tests/lib.sh

test_init "Baker list with no instances exits cleanly"

# Ensure no baker instances exist for this test.
# We use a cleanup guard to avoid stale state from a previous failed run.
GUARD_INSTANCE="test-baker-list-guard"
cleanup_instance "$GUARD_INSTANCE" || true

# 'baker list' must exit 0 and show a friendly message, not a stack trace.
output=$(om baker list 2>&1)
exit_code=$?

if [[ $exit_code -ne 0 ]]; then
  echo "ERROR: 'baker list' exited with code $exit_code (expected 0)"
  echo "Output: $output"
  exit 1
fi

if [[ "$output" == *"capability missing"* ]] || [[ "$output" == *"Failure"* ]]; then
  echo "ERROR: 'baker list' crashed with exception:"
  echo "$output"
  exit 1
fi

# JSON variant must also exit cleanly.
json_output=$(om baker list --json 2>&1)
json_exit=$?

if [[ $json_exit -ne 0 ]]; then
  echo "ERROR: 'baker list --json' exited with code $json_exit (expected 0)"
  echo "Output: $json_output"
  exit 1
fi

if [[ "$json_output" == *"capability missing"* ]] || [[ "$json_output" == *"Failure"* ]]; then
  echo "ERROR: 'baker list --json' crashed with exception:"
  echo "$json_output"
  exit 1
fi

echo "Baker list no-instances test passed"
