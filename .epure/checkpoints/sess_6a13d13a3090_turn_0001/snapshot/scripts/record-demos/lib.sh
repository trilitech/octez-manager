#!/usr/bin/env bash
# Shared helpers for demo GIF recording
# Usage: source lib.sh

set -euo pipefail

OM="${OM:-/home/valentin/work/octez-manager/_build/default/src/main.exe}"
AGG="${AGG:-/tmp/agg}"
CAST_DIR="/tmp/demo-casts"
GIF_DIR="/tmp/demo-gifs"
COLS=120
ROWS=35
SPEED=1.5
FONT_SIZE=14

mkdir -p "$CAST_DIR" "$GIF_DIR"

session_name() {
	echo "demo-$$-$1"
}

# Start recording session
# $1 = name (used for session, cast file, gif file)
start_recording() {
	local name="$1"
	local sess
	sess=$(session_name "$name")
	local cast="$CAST_DIR/$name.cast"

	tmux kill-session -t "$sess" 2>/dev/null || true
	tmux new-session -d -s "$sess" -x "$COLS" -y "$ROWS"
	tmux send-keys -t "$sess" "asciinema rec '$cast' --cols $COLS --rows $ROWS --overwrite -c '$OM'" Enter
	sleep 3 # wait for TUI to render
}

# Send keys with delay
k() {
	local sess="$1"
	shift
	tmux send-keys -t "$sess" "$@"
	sleep 0.3
}

# Longer pause for visual effect
pause() {
	sleep "${1:-1}"
}

# Capture screen (for debugging)
cap() {
	local sess="$1"
	tmux capture-pane -t "$sess" -p
}

# Stop recording and convert to GIF
# $1 = name
stop_and_convert() {
	local name="$1"
	local sess
	sess=$(session_name "$name")
	local cast="$CAST_DIR/$name.cast"
	local gif="$GIF_DIR/$name.gif"

	# Quit the TUI
	k "$sess" q
	sleep 1
	# Exit the asciinema shell
	tmux send-keys -t "$sess" "exit" Enter
	sleep 1
	tmux kill-session -t "$sess" 2>/dev/null || true

	# Convert
	"$AGG" "$cast" "$gif" --font-size "$FONT_SIZE" --speed "$SPEED" 2>&1 | tail -1
	echo "Created: $gif ($(du -h "$gif" | cut -f1))"
}

# Navigate to top of instances list
go_top() {
	local sess="$1"
	for i in {1..20}; do k "$sess" Up; done
}

# Navigate down N times
go_down() {
	local sess="$1"
	local n="$2"
	for ((i = 0; i < n; i++)); do k "$sess" Down; done
}
