#!/usr/bin/env bash
# Record edit_baker.gif: open edit form for existing baker and browse fields
set -euo pipefail
DIR="$(cd "$(dirname "$0")" && pwd)"
source "$DIR/lib.sh"

NAME="edit_baker"
start_recording "$NAME"
SESS=$(session_name "$NAME")

pause 1

# Navigate to test-octez-baker (1 down from top)
go_down "$SESS" 1
pause 0.5

# Open action menu
k "$SESS" Enter
pause 1

# Select Edit (Wallet=0, Details=1, Edit=2 → 2 downs)
go_down "$SESS" 2
pause 0.3
k "$SESS" Enter
pause 2

# Browse form fields — scroll down through them
go_down "$SESS" 3
pause 0.5
go_down "$SESS" 3
pause 0.5
go_down "$SESS" 3
pause 0.5

# Scroll back up
go_top "$SESS"
pause 1

# Cancel without saving
k "$SESS" Escape
pause 1

stop_and_convert "$NAME"
