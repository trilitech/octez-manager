#!/usr/bin/env bash
# Record rpc_browser_wide.gif: RPC browser at wide terminal (200 cols)
set -euo pipefail
DIR="$(cd "$(dirname "$0")" && pwd)"
source "$DIR/lib.sh"

# Override terminal size for wide recording
COLS=200

NAME="rpc_browser_wide"
SESS="demo-$$-$NAME"
CAST="$CAST_DIR/$NAME.cast"
GIF="$GIF_DIR/$NAME.gif"

tmux kill-session -t "$SESS" 2>/dev/null || true
tmux new-session -d -s "$SESS" -x "$COLS" -y "$ROWS"
tmux send-keys -t "$SESS" "asciinema rec '$CAST' --cols $COLS --rows $ROWS --overwrite -c '$OM'" Enter
sleep 3

pause 1

# Switch to RPCs tab
k "$SESS" "4"
pause 2

# Quick access: 1 = /version
k "$SESS" "1"
pause 2

# Go back
k "$SESS" Escape
pause 1

# Quick access: 2 = /chains/main/blocks/head
k "$SESS" "2"
pause 2

# Go back
k "$SESS" Escape
pause 1

# Quick access: 3 = /chains/main/is_bootstrapped
k "$SESS" "3"
pause 2

# Quit
k "$SESS" q
sleep 1
tmux send-keys -t "$SESS" "exit" Enter
sleep 1
tmux kill-session -t "$SESS" 2>/dev/null || true

# Convert
"$AGG" "$CAST" "$GIF" --font-size "$FONT_SIZE" --speed "$SPEED" 2>&1 | tail -1
echo "Created: $GIF ($(du -h "$GIF" | cut -f1))"
