#!/bin/bash
# Completed-command assertion regression, outside service-test discovery.
set -euo pipefail
LIBRARY="${1:-}"
if [ ! -r "$LIBRARY" ] || ! bash -n "$LIBRARY"; then
	echo "readable, valid library argument required" >&2
	exit 2
fi
for tool in bash mktemp cat rm sleep; do command -v "$tool" >/dev/null || exit 2; done
if ! source "$LIBRARY"; then exit 2; fi
command -v external_service_detected >/dev/null || exit 2
scratch=$(mktemp -d) || exit 2
trap 'rm -rf -- "$scratch"' EXIT
fail() {
	echo "ASSERT FAILED: $*" >&2
	exit 1
}
needle='fixture.[literal]*'
om() {
	printf 'called\n' >>"$scratch/calls"
	printf '%s\n' "$needle"
	sleep 0.05
	local i
	for ((i = 0; i < 20000; i++)); do printf '%0128d\n' "$i"; done
	printf 'complete\n' >"$scratch/completed"
}
# This unchanged-name helper exists in the baseline; old pipefail/grep loses it.
if ! external_service_detected "$needle"; then fail "existing helper rejected a successful late producer"; fi
[ -f "$scratch/completed" ] || fail "producer did not complete"
[ "$(cat "$scratch/calls")" = called ] || fail "producer invoked more than once"
command -v assert_command_output >/dev/null || exit 2
: >"$scratch/calls"
rm -f "$scratch/completed"
assert_command_output 0 "$needle" late-output -- om list --external >/dev/null || fail "strict late producer rejected"
[ -f "$scratch/completed" ] || fail "strict producer did not complete"
[ "$(cat "$scratch/calls")" = called ] || fail "strict producer invoked more than once"
producer() {
	printf '%s\n' 'stdout original'
	printf '%s\n' 'stderr original' >&2
	return "${1:-0}"
}
assert_command_output 0 'stdout original' literal -- producer >/dev/null || fail "success rejected"
assert_command_output 7 'stdout original' expected-seven -- producer 7 >/dev/null || fail "explicit nonzero rejected"
if assert_command_output 0 'stdout original' wrong-status -- producer 7 >"$scratch/out" 2>"$scratch/error"; then fail "nonzero with match accepted"; fi
errors=$(cat "$scratch/error")
[[ "$errors" == *'Actual:   7'* && "$errors" == *'stdout original'* && "$errors" == *'stderr original'* ]] || fail "status/streams lost"
for text in missing 'stderr original'; do
	if assert_command_output 0 "$text" missing -- producer >/dev/null 2>&1; then fail "absent stdout accepted"; fi
done
literal() { printf '%s\n' 'literal.[name]*'; }
assert_command_output 0 'literal.[name]*' literal -- literal >/dev/null || fail "literal metacharacters changed"
if assert_command_output 0 'literalXname' literal -- literal >/dev/null 2>&1; then fail "literal interpreted as regex"; fi
# An assertion called under if must propagate its own failure despite disabled errexit.
if assert_command_output 0 missing conditional -- producer >/dev/null 2>&1; then fail "conditional failure swallowed"; fi
status=0
assert_command_output 0 '' unavailable -- "$scratch/absent" >/dev/null 2>&1 || status=$?
[ "$status" -ge 2 ] || fail "missing command not infrastructure"
status=0
assert_command_output 999999999999999999999 '' invalid -- producer >/dev/null 2>&1 || status=$?
[ "$status" -ge 2 ] || fail "invalid status not infrastructure"
absent="ci-output-no-registry-$$"
[ ! -e "/etc/octez_manager/services/$absent.json" ] || exit 2
om() {
	printf '%s\n' "$absent"
	return 7
}
if instance_exists "$absent"; then fail "failed list accepted without fallback record"; fi
if external_service_detected "$absent" 2>/dev/null; then fail "failed external list accepted"; fi
om() { printf '%s\n' "$absent" >&2; }
if instance_exists "$absent"; then fail "stderr warning accepted as instance row"; fi
if external_service_detected "$absent" 2>/dev/null; then fail "stderr warning accepted as external row"; fi
# A failed matching first attempt must retry; do not turn predicates into strict assertions.
om() {
	local count=0
	[ ! -f "$scratch/retry" ] || count=$(cat "$scratch/retry")
	printf '%s\n' "$((count + 1))" >"$scratch/retry"
	printf '%s\n' "$absent"
	[ "$count" -gt 0 ]
}
wait_for_external_service "$absent" 2 0 >/dev/null 2>&1 || fail "legacy retry did not recover"
[ "$(cat "$scratch/retry")" = 2 ] || fail "retry accepted a failed matching invocation"
systemctl() {
	if [ "$1" = daemon-reload ]; then return 0; fi
	local count=0
	[ ! -f "$scratch/systemctl-retry" ] || count=$(cat "$scratch/systemctl-retry")
	printf '%s\n' "$((count + 1))" >"$scratch/systemctl-retry"
	printf '%s\n' 'octez-node@.service'
	[ "$count" -gt 0 ]
}
service_exists node fixture 2 >/dev/null || fail "service retry did not recover"
[ "$(cat "$scratch/systemctl-retry")" = 2 ] || fail "service retry swallowed producer status"
echo "Completed command-output selftest passed"
