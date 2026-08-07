#!/usr/bin/env bash
# Record the hero overview GIF: navigate tabs, show dashboard
set -euo pipefail
DIR="$(cd "$(dirname "$0")" && pwd)"
source "$DIR/lib.sh"

NAME="octez-manager"
start_recording "$NAME"
SESS=$(session_name "$NAME")

pause 1.5

# Browse instances - move through services
go_down "$SESS" 3
pause 0.8
go_top "$SESS"
pause 0.5

# Switch to Wallets tab
k "$SESS" "2"
pause 2

# Switch to Binaries tab
k "$SESS" "3"
pause 2

# Switch to RPCs
k "$SESS" "4"
pause 1.5

# Diagnostics
k "$SESS" "5"
pause 2

# Topology
k "$SESS" "6"
pause 2

# Back to Instances
k "$SESS" "1"
pause 1.5

stop_and_convert "$NAME"
