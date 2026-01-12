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

# Instance helpers
instance_exists() {
    local instance="$1"
    # Capture both stdout and stderr to ensure we see all output
    om list 2>&1 | grep -q "$instance"
}

# Inject pre-generated identity to skip PoW during node start
# Call this after install-node but before starting the node
inject_identity() {
    local instance="$1"
    local data_dir="${2:-/var/lib/octez/$instance}"
    local pregenerated="/etc/octez/pregenerated/identity.json"

    if [ -f "$pregenerated" ]; then
        cp "$pregenerated" "$data_dir/identity.json"
        chown tezos:tezos "$data_dir/identity.json"
        chmod 600 "$data_dir/identity.json"
    fi
}

# Service helpers (real systemd)
service_exists() {
    local role="$1"
    local instance="$2"
    # Template units are listed as role@.service, not role@instance.service
    systemctl list-unit-files "octez-${role}@.service" 2>/dev/null | grep -q "octez-${role}@"
}

service_is_active() {
    local role="$1"
    local instance="$2"
    systemctl is-active "octez-${role}@${instance}.service" >/dev/null 2>&1
}

service_is_enabled() {
    local role="$1"
    local instance="$2"
    systemctl is-enabled "octez-${role}@${instance}.service" >/dev/null 2>&1
}

wait_for_service_active() {
    local role="$1"
    local instance="$2"
    local max_wait="${3:-30}"
    local count=0

    echo "Waiting for octez-${role}@${instance} to be active..."
    while [ $count -lt $max_wait ]; do
        if service_is_active "$role" "$instance"; then
            echo "Service is active"
            return 0
        fi
        sleep 1
        count=$((count + 1))
    done

    echo "Service octez-${role}@${instance} did not become active after ${max_wait}s"
    show_service_status "$role" "$instance"
    return 1
}

wait_for_service_stopped() {
    local role="$1"
    local instance="$2"
    local max_wait="${3:-30}"
    local count=0

    while [ $count -lt $max_wait ]; do
        if ! service_is_active "$role" "$instance"; then
            return 0
        fi
        sleep 1
        count=$((count + 1))
    done

    echo "Service octez-${role}@${instance} did not stop"
    return 1
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
        --network shadownet \
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

    # Stop service if running, ignore errors
    om instance "$instance" stop 2>/dev/null || true
    # Remove and purge
    om instance "$instance" remove 2>/dev/null || true
    om instance "$instance" purge 2>/dev/null || true
}

# RPC helpers
get_node_level() {
    local rpc_addr="${1:-127.0.0.1:8732}"
    curl -sf "http://${rpc_addr}/chains/main/blocks/head/header" 2>/dev/null | jq -r '.level // empty'
}

wait_for_node_ready() {
    local rpc_addr="${1:-127.0.0.1:8732}"
    local max_wait="${2:-60}"
    local count=0

    echo "Waiting for node RPC at $rpc_addr..."
    while [ $count -lt $max_wait ]; do
        if curl -sf "http://${rpc_addr}/chains/main/blocks/head/header" >/dev/null 2>&1; then
            echo "Node RPC is ready"
            return 0
        fi
        sleep 2
        count=$((count + 2))
    done

    echo "Node RPC did not become ready after ${max_wait}s"
    return 1
}

wait_for_level_increase() {
    local initial_level="$1"
    local rpc_addr="${2:-127.0.0.1:8732}"
    local max_wait="${3:-120}"
    local count=0

    echo "Waiting for level to increase from $initial_level..."
    while [ $count -lt $max_wait ]; do
        local current_level=$(get_node_level "$rpc_addr")
        if [ -n "$current_level" ] && [ "$current_level" -gt "$initial_level" ]; then
            echo "Level increased: $initial_level -> $current_level"
            return 0
        fi
        sleep 5
        count=$((count + 5))
    done

    echo "Level did not increase after ${max_wait}s (still at $initial_level)"
    return 1
}

# Debug helpers
show_service_status() {
    local role="$1"
    local instance="$2"
    echo "=== Service status: octez-${role}@${instance} ==="
    systemctl status "octez-${role}@${instance}.service" --no-pager 2>&1 || true
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
