#!/bin/bash
# Test helper library

# Assertions
assert_eq() {
    local expected="$1"
    local actual="$2"
    local msg="${3:-assertion failed}"

    if [ "$expected" != "$actual" ]; then
        echo "ASSERT FAILED: $msg"
        echo "  Expected: $expected"
        echo "  Actual:   $actual"
        return 1
    fi
}

assert_contains() {
    local haystack="$1"
    local needle="$2"
    local msg="${3:-string not found}"

    if [[ "$haystack" != *"$needle"* ]]; then
        echo "ASSERT FAILED: $msg"
        echo "  Looking for: $needle"
        echo "  In: $haystack"
        return 1
    fi
}

assert_file_exists() {
    local path="$1"
    local msg="${2:-file should exist: $path}"

    if [ ! -f "$path" ]; then
        echo "ASSERT FAILED: $msg"
        return 1
    fi
}

assert_dir_exists() {
    local path="$1"
    local msg="${2:-directory should exist: $path}"

    if [ ! -d "$path" ]; then
        echo "ASSERT FAILED: $msg"
        return 1
    fi
}

# Instance helpers (use octez-manager registry instead of systemctl)
instance_exists() {
    local instance="$1"
    om list 2>/dev/null | grep -q "$instance"
}

# Legacy service helpers (mock - always succeed in test mode)
service_exists() {
    local role="$1"
    local instance="$2"
    instance_exists "$instance"
}

service_is_active() {
    # In test mode with mock systemctl, services aren't actually running
    # Return false so tests don't expect running services
    return 1
}

service_is_enabled() {
    # In test mode, return false
    return 1
}

wait_for_service_active() {
    # Skip waiting in test mode
    echo "Note: Skipping service wait in test mode (mock systemctl)"
    return 0
}

wait_for_service_stopped() {
    # Skip waiting in test mode
    return 0
}

# octez-manager helpers
om() {
    octez-manager "$@"
}

om_install_node() {
    local instance="${1:-$TEST_INSTANCE}"
    local extra_args="${2:-}"

    om install-node \
        --instance "$instance" \
        --network tallinnnet \
        --snapshot \
        --snapshot-uri "$SANDBOX_URL/snapshot.rolling" \
        --rpc-addr "127.0.0.1:8732" \
        --service-user tezos \
        --no-enable \
        $extra_args
}

om_instance() {
    local instance="${1:-$TEST_INSTANCE}"
    local action="$2"
    shift 2

    om instance "$instance" "$action" "$@"
}

# Cleanup
cleanup_instance() {
    local instance="${1:-$TEST_INSTANCE}"

    # Try to remove, ignore errors
    om instance "$instance" remove 2>/dev/null || true
    om instance "$instance" purge 2>/dev/null || true
}

# Debug helpers
show_service_status() {
    local role="$1"
    local instance="$2"
    echo "=== Service status: octez-${role}@${instance} ==="
    systemctl status "octez-${role}@${instance}.service" 2>&1 || true
    echo "==="
}

show_service_logs() {
    local role="$1"
    local instance="$2"
    local lines="${3:-20}"
    echo "=== Service logs: octez-${role}@${instance} (last $lines lines) ==="
    journalctl -u "octez-${role}@${instance}.service" -n "$lines" --no-pager 2>&1 || true
    echo "==="
}
