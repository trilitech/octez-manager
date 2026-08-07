#!/usr/bin/env bash
# Record rpc_browser.gif: open RPC browser and query endpoints
set -euo pipefail
DIR="$(cd "$(dirname "$0")" && pwd)"
source "$DIR/lib.sh"

NAME="rpc_browser"
start_recording "$NAME"
SESS=$(session_name "$NAME")

pause 1

# Switch to RPCs tab
k "$SESS" "4"
pause 2

# Use quick access: 1 = /version
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

stop_and_convert "$NAME"
