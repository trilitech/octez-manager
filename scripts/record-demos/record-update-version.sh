#!/usr/bin/env bash
# Record update_version.gif: update version on test-octez-accuser
# (accuser is lightest service to restart)
set -euo pipefail
DIR="$(cd "$(dirname "$0")" && pwd)"
source "$DIR/lib.sh"

NAME="update_version"
start_recording "$NAME"
SESS=$(session_name "$NAME")

pause 1

# Navigate to test-octez-accuser (2 downs from top)
go_down "$SESS" 2
pause 0.5

# Open action menu
k "$SESS" Enter
pause 1

# Navigate to "Update Version" in the action menu
# Menu: Details(0), Edit(1), Start(2), Stop(3), Restart(4), Update Version(5)
go_down "$SESS" 5
pause 0.3
k "$SESS" Enter # Open version selector
pause 2

# Version list: v24.3(0), v24.2(1), v24.1(2), v24.0(3)
# Select v24.1 (2 downs from v24.3)
go_down "$SESS" 2
pause 0.5
k "$SESS" Enter # Confirm version
pause 5         # Wait for update

stop_and_convert "$NAME"

# Restore: update back to v24.0
"$OM" instance test-octez-accuser update-version v24.0 2>/dev/null || true
echo "Restored test-octez-accuser to v24.0"
