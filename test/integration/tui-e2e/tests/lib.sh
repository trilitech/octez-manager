#!/bin/bash
# TUI E2E Test Helper Library
# Provides tmux-based TUI automation, screen capture, and service verification

# Session configuration
TUI_SESSION="om-e2e"
TUI_LOG="/tmp/tui-session.log"
ERROR_DIR="/tmp/tui-errors"

# Initialize error directory
mkdir -p "$ERROR_DIR"

# ============================================================
# TUI Session Management
# ============================================================

tui_start() {
	local title="${1:-octez-manager}"
	export TERM=xterm-256color
	export BISECT_FILE=/coverage/bisect-tui

	echo "Starting TUI session..."

	# Start octez-manager in tmux session with logging
	tmux new-session -d -s "$TUI_SESSION" \
		"octez-manager 2>&1 | tee -a $TUI_LOG"

	sleep 2 # Wait for TUI to initialize

	if ! tmux has-session -t "$TUI_SESSION" 2>/dev/null; then
		echo "ERROR: TUI session failed to start"
		cat "$TUI_LOG" 2>/dev/null || echo "No log file"
		return 1
	fi

	echo "TUI session started successfully"
}

tui_stop() {
	echo "Stopping TUI session..."
	tmux send-keys -t "$TUI_SESSION" 'q' 2>/dev/null || true
	sleep 0.5
	tmux kill-session -t "$TUI_SESSION" 2>/dev/null || true
}

tui_send() {
	local keys="$1"
	tmux send-keys -t "$TUI_SESSION" "$keys"
}

tui_send_literal() {
	local text="$1"
	tmux send-keys -t "$TUI_SESSION" -l "$text"
}

tui_capture() {
	tmux capture-pane -t "$TUI_SESSION" -p
}

tui_capture_to_file() {
	local file="$1"
	tui_capture >"$file"
}

# ============================================================
# Waiting and Verification
# ============================================================

tui_wait_for() {
	local text="$1"
	local timeout="${2:-10}"
	local elapsed=0

	while [ $elapsed -lt $timeout ]; do
		if tui_capture | grep -q "$text"; then
			return 0
		fi
		sleep 0.5
		elapsed=$((elapsed + 1))
	done

	echo "ERROR: Timeout waiting for: $text"
	echo "=== Screen capture ==="
	tui_capture
	echo "======================"

	# Save error screenshot
	local timestamp=$(date +%Y%m%d-%H%M%S)
	tui_capture_to_file "$ERROR_DIR/error-${timestamp}.txt"

	return 1
}

tui_wait_not() {
	local text="$1"
	local timeout="${2:-10}"
	local elapsed=0

	while [ $elapsed -lt $timeout ]; do
		if ! tui_capture | grep -q "$text"; then
			return 0
		fi
		sleep 0.5
		elapsed=$((elapsed + 1))
	done

	echo "ERROR: Timeout waiting for text to disappear: $text"
	tui_capture
	return 1
}

# ============================================================
# Navigation Helpers
# ============================================================

nav_to_install() {
	echo "Navigating to install menu..."
	tui_send 'i'
	tui_wait_for "Install" 5
}

nav_to_instances() {
	echo "Navigating to instances page..."
	# Ensure we're at main menu
	tui_send 'Escape'
	sleep 0.3

	# Look for "Instances" in menu
	if ! tui_wait_for "Instances" 3; then
		# Try pressing Escape again
		tui_send 'Escape'
		sleep 0.3
	fi

	# Navigate to it (usually first item or Enter from main)
	tui_send 'Enter'
	tui_wait_for "Services\|Instance" 3
}

nav_back() {
	tui_send 'Escape'
	sleep 0.3
}

# ============================================================
# Form Helpers
# ============================================================

form_fill_text() {
	local value="$1"
	# Clear existing content
	tui_send 'C-a' # Ctrl+A to select all
	sleep 0.1
	tui_send 'C-k' # Ctrl+K to delete
	sleep 0.1
	# Type new value
	tui_send_literal "$value"
	sleep 0.1
}

form_next() {
	tui_send 'Tab'
	sleep 0.2
}

form_select_choice() {
	local navigation="$1" # e.g., "Down Down" to go down twice
	tui_send "$navigation"
	sleep 0.1
	tui_send 'Enter' # Confirm selection
	sleep 0.1
}

form_toggle_yes() {
	tui_send 'y'
	sleep 0.1
}

form_toggle_no() {
	tui_send 'n'
	sleep 0.1
}

form_submit() {
	echo "Submitting form..."
	# Navigate to Confirm button (usually Tab Tab from last field)
	tui_send 'Tab' 'Tab'
	sleep 0.3
	tui_send 'Enter'
	sleep 0.5
}

# ============================================================
# Service Verification
# ============================================================

verify_service_exists() {
	local instance="$1"
	local role="$2"

	if systemctl --user list-units --all | grep -q "octez-${role}@${instance}"; then
		return 0
	else
		echo "ERROR: Service octez-${role}@${instance} not found in systemd"
		systemctl --user list-units --all | grep octez || echo "No octez services"
		return 1
	fi
}

verify_service_running() {
	local instance="$1"
	local role="$2"

	if systemctl --user is-active "octez-${role}@${instance}" >/dev/null 2>&1; then
		return 0
	else
		echo "ERROR: Service octez-${role}@${instance} not running"
		systemctl --user status "octez-${role}@${instance}" --no-pager || true
		return 1
	fi
}

verify_in_instances() {
	local instance="$1"

	echo "Verifying '$instance' appears in instances page..."
	nav_to_instances

	if ! tui_wait_for "$instance" 5; then
		echo "ERROR: Instance '$instance' not found in TUI"
		echo "=== Screen content ==="
		tui_capture
		echo "====================="
		return 1
	fi

	echo "✓ Instance '$instance' found in instances page"
}

# ============================================================
# Cleanup Helpers
# ============================================================

cleanup_instance() {
	local instance="$1"
	echo "Cleaning up instance: $instance"
	om remove -y --instance "$instance" 2>/dev/null || true
}

cleanup_all_test_instances() {
	echo "Cleaning up all test instances..."
	for instance in $(systemctl --user list-units 'octez-*@tui-*' --no-legend | awk '{print $1}' | sed 's/.*@//; s/\.service//'); do
		cleanup_instance "$instance"
	done
}

# ============================================================
# Error Handling
# ============================================================

on_error() {
	echo ""
	echo "=========================================="
	echo "ERROR: Test failed"
	echo "=========================================="

	echo ""
	echo "=== Screen capture ==="
	tui_capture || echo "Could not capture screen"

	echo ""
	echo "=== TUI log (last 50 lines) ==="
	tail -50 "$TUI_LOG" 2>/dev/null || echo "No log file"

	echo ""
	echo "=== Systemd services ==="
	systemctl --user list-units 'octez-*' --no-pager || true

	echo ""
	echo "=== Coverage files ==="
	ls -la /coverage/ 2>/dev/null || echo "No coverage directory"

	# Save error artifacts
	local timestamp=$(date +%Y%m%d-%H%M%S)
	tui_capture_to_file "$ERROR_DIR/final-screen-${timestamp}.txt" 2>/dev/null || true
	cp "$TUI_LOG" "$ERROR_DIR/tui-log-${timestamp}.log" 2>/dev/null || true

	echo ""
	echo "Error artifacts saved to: $ERROR_DIR"
	ls -la "$ERROR_DIR/" || true

	# Stop TUI
	tui_stop
}

# Set error trap
trap on_error ERR

# ============================================================
# Utility Functions
# ============================================================

wait_for_service_state() {
	local instance="$1"
	local role="$2"
	local expected_state="$3" # active, inactive, etc.
	local timeout="${4:-30}"
	local elapsed=0

	while [ $elapsed -lt $timeout ]; do
		local state=$(systemctl --user is-active "octez-${role}@${instance}" 2>/dev/null || echo "unknown")
		if [ "$state" = "$expected_state" ]; then
			return 0
		fi
		sleep 1
		elapsed=$((elapsed + 1))
	done

	echo "ERROR: Service did not reach state '$expected_state' within ${timeout}s"
	systemctl --user status "octez-${role}@${instance}" --no-pager || true
	return 1
}

debug_print_screen() {
	echo "=== Current screen ==="
	tui_capture
	echo "====================="
}
