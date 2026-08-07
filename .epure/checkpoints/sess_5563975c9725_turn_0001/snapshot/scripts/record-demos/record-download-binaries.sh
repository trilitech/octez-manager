#!/usr/bin/env bash
# Record download_binaries.gif: open binaries tab and show versions
set -euo pipefail
DIR="$(cd "$(dirname "$0")" && pwd)"
source "$DIR/lib.sh"

NAME="download_binaries"
start_recording "$NAME"
SESS=$(session_name "$NAME")

pause 1

# Switch to Binaries tab
k "$SESS" "3"
pause 2

# Navigate through the list
go_down "$SESS" 3
pause 0.5
go_down "$SESS" 3
pause 0.5
go_top "$SESS"
pause 1

stop_and_convert "$NAME"
