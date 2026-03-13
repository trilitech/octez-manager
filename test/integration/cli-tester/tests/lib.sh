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

# Ensure the tezos system user exists.
# Purge operations delete the service user, which breaks parallel tests
# that need --service-user tezos. Call this after any purge.
ensure_tezos_user() {
	if ! id tezos &>/dev/null; then
		useradd --system --no-create-home tezos 2>/dev/null || true
	fi
}

# Lock file used to serialize install and purge operations.
# Purge can delete the tezos service user; install needs it for chown.
# Without serialization, a parallel purge can delete the user between
# install's ensure_service_account and its chown call.
_OM_USER_LOCK="/tmp/om-service-user.lock"

# Instance helpers
instance_exists() {
	local instance="$1"
	# Primary check: ask octez-manager
	if om list 2>&1 | grep -q "$instance"; then
		return 0
	fi
	# Fallback: check the service registry file directly.
	# This handles cases where om list fails due to a corrupt
	# sibling file during parallel test execution.
	if [ -f "/etc/octez_manager/services/${instance}.json" ]; then
		return 0
	fi
	return 1
}

# Inject pre-generated identity to skip PoW during node start
# Call this after install-node but before starting the node
inject_identity() {
	local instance="$1"
	local data_dir="${2:-/var/lib/octez/$instance}"
	local pregenerated="/etc/octez/pregenerated/identity.json"

	if [ -f "$pregenerated" ]; then
		ensure_tezos_user
		cp "$pregenerated" "$data_dir/identity.json"
		chown tezos:tezos "$data_dir/identity.json"
		chmod 600 "$data_dir/identity.json"
	fi
}

# Service helpers (real systemd)
service_exists() {
	local role="$1"
	local instance="$2"
	local max_retries="${3:-20}" # 10 seconds max (20 × 0.5s)
	local retry=0

	# Force systemd to reload its configuration to ensure new services are visible
	systemctl daemon-reload 2>/dev/null || true

	# Retry loop to handle systemd cache propagation delay
	# After install, there can be a brief window where daemon-reload hasn't
	# fully propagated through systemd's internal state
	while [ $retry -lt $max_retries ]; do
		# Template units are listed as role@.service, not role@instance.service
		if systemctl list-unit-files "octez-${role}@.service" 2>/dev/null | grep -q "octez-${role}@"; then
			return 0
		fi

		# Short sleep and retry
		sleep 0.5
		retry=$((retry + 1))
	done

	# Failed after retries - provide diagnostics
	echo "WARNING: Service template octez-${role}@ not found after $((max_retries / 2)) seconds" >&2
	echo "Available octez services:" >&2
	systemctl list-unit-files "octez-*" 2>&1 | head -10 >&2 || true
	return 1
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
#
# Wraps octez-manager with a file lock for install and purge/remove
# operations. This prevents a race where a parallel purge deletes the
# tezos service user while an install needs it for chown.
om() {
	local _needs_lock=0
	case "${1:-}" in
	install-*) _needs_lock=1 ;;
	instance)
		local _arg
		for _arg in "$@"; do
			case "$_arg" in purge | remove) _needs_lock=1 && break ;; esac
		done
		;;
	esac

	if [ "$_needs_lock" -eq 1 ]; then
		(
			flock -w 60 200
			case "${1:-}" in install-*) ensure_tezos_user ;; esac
			octez-manager "$@"
			_om_rc=$?
			ensure_tezos_user
			exit "$_om_rc"
		) 200>"$_OM_USER_LOCK"
	else
		octez-manager "$@"
	fi
}

om_install_node() {
	local instance="${1:-$TEST_INSTANCE}"
	local extra_args="${2:-}"

	om install-node \
		--instance "$instance" \
		--network shadownet \
		--snapshot \
		--snapshot-no-check \
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
	# Remove and purge (om() holds the user lock and recreates
	# the tezos user after each purge/remove automatically)
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

# Import test helpers

# Create an external systemd service (unmanaged by octez-manager)
# Parameters:
#   $1: role (node, baker, accuser, dal-node)
#   $2: instance name
#   $3: data directory (empty string for accuser)
#   $4: rpc_addr (for node) or empty for dependent services
#   $5: network
#   $6: node_endpoint (for baker, accuser, dal-node)
#   $7: base_dir (for baker) or dal_rpc_addr (for dal-node)
#   $8: node_instance (optional, for baker/accuser/dal-node dependencies)
create_external_service() {
	local role="$1"
	local instance="$2"
	local data_dir="$3"
	local rpc_addr="${4:-127.0.0.1:8732}"
	local network="${5:-shadownet}"

	local unit_name="octez-${role}@${instance}.service"
	local unit_dir="/etc/systemd/system"
	local octez_bin_path="/usr/local/bin"

	mkdir -p "$unit_dir"
	if [ -n "$data_dir" ]; then
		mkdir -p "$data_dir"
		ensure_tezos_user
		chown -R tezos:tezos "$data_dir"
	else
		ensure_tezos_user
	fi

	case "$role" in
	node)
		local p2p_addr="127.0.0.1:$(alloc_port)"
		cat >"$unit_dir/$unit_name" <<SERVICE
[Unit]
Description=External Octez Node - $instance
After=network.target

[Service]
Type=simple
User=tezos
ExecStart=$octez_bin_path/octez-node run --data-dir $data_dir --network $network --rpc-addr $rpc_addr --net-addr $p2p_addr
Restart=on-failure
RestartSec=5

[Install]
WantedBy=multi-user.target
SERVICE
		;;
	baker)
		local node_endpoint="${6:-http://localhost:8732}"
		local base_dir="${7:-$data_dir}"
		local node_instance="${8:-$instance}"
		cat >"$unit_dir/$unit_name" <<SERVICE
[Unit]
Description=External Octez Baker - $instance
After=network.target octez-node@${node_instance}.service
Requires=octez-node@${node_instance}.service

[Service]
Type=simple
User=tezos
ExecStart=$octez_bin_path/octez-baker --endpoint $node_endpoint --base-dir $base_dir run with local node $data_dir --liquidity-baking-toggle-vote pass
Restart=on-failure
RestartSec=5

[Install]
WantedBy=multi-user.target
SERVICE
		;;
	accuser)
		local node_endpoint="${6:-http://localhost:8732}"
		local base_dir="${7:-$data_dir}"
		local node_instance="${8:-$instance}"
		cat >"$unit_dir/$unit_name" <<SERVICE
[Unit]
Description=External Octez Accuser - $instance
After=network.target octez-node@${node_instance}.service
Requires=octez-node@${node_instance}.service

[Service]
Type=simple
User=tezos
ExecStart=$octez_bin_path/octez-baker --endpoint $node_endpoint --base-dir $base_dir run accuser
Restart=on-failure
RestartSec=5

[Install]
WantedBy=multi-user.target
SERVICE
		;;
	dal-node)
		local node_endpoint="${6:-http://localhost:8732}"
		local dal_rpc_addr="${7:-127.0.0.1:10732}"
		local node_instance="${8:-$instance}"
		cat >"$unit_dir/$unit_name" <<SERVICE
[Unit]
Description=External Octez DAL Node - $instance
After=network.target octez-node@${node_instance}.service
Requires=octez-node@${node_instance}.service

[Service]
Type=simple
User=tezos
ExecStart=$octez_bin_path/octez-dal-node run --data-dir $data_dir --endpoint $node_endpoint --rpc-addr $dal_rpc_addr
Restart=on-failure
RestartSec=5

[Install]
WantedBy=multi-user.target
SERVICE
		;;
	esac

	systemctl daemon-reload
}

# Check if external service is detected
external_service_detected() {
	local service_name="$1"
	om list --external 2>&1 | grep -q "$service_name"
}

# Wait for external service to be detected (with retries)
# Usage: wait_for_external_service <instance> [max_attempts] [sleep_interval]
wait_for_external_service() {
	local service_name="$1"
	local max_attempts="${2:-10}"
	local interval="${3:-1}"
	local attempt=1
	while [ "$attempt" -le "$max_attempts" ]; do
		if external_service_detected "$service_name"; then
			return 0
		fi
		echo "  Waiting for external service detection (attempt $attempt/$max_attempts)..."
		sleep "$interval"
		attempt=$((attempt + 1))
	done
	echo "ERROR: External service '$service_name' not detected after $max_attempts attempts"
	return 1
}

# Verify service is now managed
service_is_managed() {
	local instance="$1"
	# Check that instance appears in list but NOT in external services section
	om list 2>&1 | grep -v "External Octez Services" | grep -q "$instance"
}

# Verify external service is disabled
external_service_disabled() {
	local role="$1"
	local instance="$2"
	! systemctl is-enabled "octez-${role}@${instance}.service" 2>/dev/null
}

# Start process without systemd (unmanaged)
start_unmanaged_process() {
	local binary="$1"
	shift
	local args="$@"
	local octez_bin_path="/usr/local/bin"

	runuser -s /bin/sh -c "$octez_bin_path/$binary $args" tezos &
	echo $!
}

# ==========================================================================
# Test isolation harness
#
# Provides automatic resource cleanup via trap handlers so tests never leak
# systemd services, data directories, or background processes — even on
# failure or early exit.
#
# Usage:
#   source /tests/lib.sh
#   test_init "My test description"
#
#   register_instance "my-node"
#   register_external_service "node" "ext-node"
#   register_data_dir "/var/lib/octez-external/ext-node"
#   register_process "$pid"
#
#   # ... test logic ...
#   # Cleanup happens automatically on exit (pass or fail)
# ==========================================================================

# Resource tracking arrays (initialized by test_init)
_HARNESS_INSTANCES=()
_HARNESS_EXTERNAL_SERVICES=()
_HARNESS_DATA_DIRS=()
_HARNESS_PROCESSES=()
_HARNESS_INITIALIZED=0
_HARNESS_TEST_NAME=""

# Port allocation state
_HARNESS_PORT_BASE=0
_HARNESS_PORT_FILE=""

# Initialize the test harness. Must be called before any register_* calls.
# Sets up a trap handler that cleans all registered resources on EXIT.
#
# Usage: test_init "Test description"
test_init() {
	local description="${1:-unnamed test}"
	_HARNESS_TEST_NAME="$description"
	_HARNESS_INITIALIZED=1
	_HARNESS_INSTANCES=()
	_HARNESS_EXTERNAL_SERVICES=()
	_HARNESS_DATA_DIRS=()
	_HARNESS_PROCESSES=()

	# Compute a stable port base from the test script path (dir/name).
	# Each test gets a range of 10 ports starting at its base.
	# Range: 19000-48990 (3000 possible test slots × 10 ports each)
	# Using the full relative path (e.g. "node/01-install") avoids
	# collisions between tests in different directories.
	local script_path
	script_path="${BASH_SOURCE[-1]}"
	# Extract "dir/name" from e.g. "/tests/node/01-install.sh"
	script_path="${script_path##*/tests/}"
	script_path="${script_path%.sh}"
	local hash
	hash=$(echo -n "$script_path" | cksum | awk '{print $1}')
	_HARNESS_PORT_BASE=$((19000 + (hash % 3000) * 10))
	_HARNESS_PORT_FILE=$(mktemp /tmp/harness-port-XXXXXX)
	echo "0" >"$_HARNESS_PORT_FILE"

	echo "Test: $description"

	# Install trap handler for cleanup on exit
	trap _harness_cleanup EXIT
}

# Allocate a unique port for this test. Returns a port number via stdout.
# Each test gets a deterministic base port derived from its filename,
# with sequential offsets for multiple ports within the same test.
#
# Usage: local port=$(alloc_port)
alloc_port() {
	if [ "$_HARNESS_INITIALIZED" -ne 1 ]; then
		echo "ERROR: alloc_port called before test_init" >&2
		return 1
	fi
	# Read and increment counter from file to survive $() subshells
	local counter
	counter=$(cat "$_HARNESS_PORT_FILE")
	echo $((counter + 1)) >"$_HARNESS_PORT_FILE"
	local port=$((_HARNESS_PORT_BASE + counter))
	if [ "$counter" -ge 10 ]; then
		echo "WARNING: test allocated more than 10 ports" >&2
	fi
	echo "$port"
}

# Register a managed instance for automatic cleanup.
# Also runs pre-cleanup to remove leftovers from previous failed runs.
#
# Usage: register_instance "my-node"
register_instance() {
	local instance="$1"
	if [ "$_HARNESS_INITIALIZED" -ne 1 ]; then
		echo "ERROR: register_instance called before test_init" >&2
		return 1
	fi
	_HARNESS_INSTANCES+=("$instance")
	# Pre-cleanup: remove leftovers from previous failed runs
	cleanup_instance "$instance" || true
}

# Register an external systemd service for automatic cleanup.
# Also runs pre-cleanup to stop/disable/remove leftovers.
#
# Usage: register_external_service "node" "ext-instance"
register_external_service() {
	local role="$1"
	local instance="$2"
	if [ "$_HARNESS_INITIALIZED" -ne 1 ]; then
		echo "ERROR: register_external_service called before test_init" >&2
		return 1
	fi
	_HARNESS_EXTERNAL_SERVICES+=("${role}:${instance}")
	# Pre-cleanup: remove leftovers from previous failed runs
	local unit="octez-${role}@${instance}.service"
	systemctl stop "$unit" 2>/dev/null || true
	systemctl disable "$unit" 2>/dev/null || true
	rm -f "/etc/systemd/system/$unit" || true
}

# Register a data directory for automatic cleanup.
# Also removes leftovers from previous failed runs.
#
# Usage: register_data_dir "/var/lib/octez-external/my-node"
register_data_dir() {
	local dir="$1"
	if [ "$_HARNESS_INITIALIZED" -ne 1 ]; then
		echo "ERROR: register_data_dir called before test_init" >&2
		return 1
	fi
	_HARNESS_DATA_DIRS+=("$dir")
	# Pre-cleanup
	rm -rf "$dir" || true
}

# Register a background process for automatic cleanup.
#
# Usage: local pid=$(start_unmanaged_process ...); register_process "$pid"
register_process() {
	local pid="$1"
	if [ "$_HARNESS_INITIALIZED" -ne 1 ]; then
		echo "ERROR: register_process called before test_init" >&2
		return 1
	fi
	_HARNESS_PROCESSES+=("$pid")
}

# Internal: cleanup handler called automatically on EXIT.
# Cleans resources in reverse order: processes, external services,
# managed instances, data directories. All errors are suppressed.
_harness_cleanup() {
	local exit_code=$?

	# Skip if harness was never initialized (backward compatibility)
	if [ "$_HARNESS_INITIALIZED" -ne 1 ]; then
		return "$exit_code"
	fi

	# Kill registered background processes
	for pid in "${_HARNESS_PROCESSES[@]}"; do
		if kill -0 "$pid" 2>/dev/null; then
			kill "$pid" 2>/dev/null || true
			wait "$pid" 2>/dev/null || true
		fi
	done

	# Stop and remove external systemd services
	local need_reload=0
	for entry in "${_HARNESS_EXTERNAL_SERVICES[@]}"; do
		local role="${entry%%:*}"
		local instance="${entry#*:}"
		local unit="octez-${role}@${instance}.service"
		systemctl stop "$unit" 2>/dev/null || true
		systemctl disable "$unit" 2>/dev/null || true
		if [ -f "/etc/systemd/system/$unit" ]; then
			rm -f "/etc/systemd/system/$unit" || true
			need_reload=1
		fi
	done
	if [ "$need_reload" -eq 1 ]; then
		systemctl daemon-reload 2>/dev/null || true
	fi

	# Cleanup managed instances (stop, remove, purge)
	for instance in "${_HARNESS_INSTANCES[@]}"; do
		cleanup_instance "$instance" || true
	done

	# Remove data directories
	for dir in "${_HARNESS_DATA_DIRS[@]}"; do
		rm -rf "$dir" || true
	done

	# Clean up port counter file
	rm -f "$_HARNESS_PORT_FILE" || true

	return "$exit_code"
}
