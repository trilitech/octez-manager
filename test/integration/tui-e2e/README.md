# TUI End-to-End Integration Tests

End-to-end integration tests that use tmux to automate the TUI and verify real systemd service management.

## Overview

These tests drive the octez-manager TUI to install and manage real Octez services (node, baker, accuser, DAL node), verifying the full stack from TUI interaction through systemd service creation and execution.

**Key features:**
- Real systemd integration (services actually start/stop)
- tmux-based TUI automation
- Coverage collection from TUI interactions
- Hybrid workflows (CLI→TUI and TUI→CLI)
- Screen capture on failures for debugging

## Architecture

```
tui-e2e/
├── Dockerfile              # Debian + systemd + tmux
├── docker-compose.yml      # Orchestration (shares sandbox)
├── run-tests.sh            # Test runner with sharding
├── shards.json             # Test distribution
├── tests/
│   ├── lib.sh              # TUI automation helpers
│   ├── 01-*.sh             # Shard 1 tests
│   ├── 02-*.sh             # Shard 2 tests
│   └── 03-*.sh             # Shard 3 tests
└── README.md               # This file
```

## Running Tests Locally

### Prerequisites
- Docker and docker-compose installed
- octez-manager built (instrumented for coverage)

### Run All Tests
```bash
# From repository root
cd test/integration

# Start sandbox
docker compose up -d sandbox

# Build and run TUI E2E tests
cd tui-e2e
docker compose build
docker compose up tui-tester

# View logs
docker compose logs -f tui-tester
```

### Run Specific Shard
```bash
SHARD=1 TOTAL_SHARDS=3 docker compose up tui-tester
```

### Run Single Test
```bash
docker compose exec tui-tester bash /tests/01-tui-startup.sh
```

## Writing New Tests

### Test Template
```bash
#!/bin/bash
set -euo pipefail
source /tests/lib.sh

echo "Test: Description of what this tests"

INSTANCE="unique-instance-name"

# Cleanup
cleanup_instance "$INSTANCE" || true

# Start TUI
tui_start

# Test logic here
nav_to_install
# ... interact with TUI ...

# Verify results
verify_service_exists "$INSTANCE" "node"
verify_in_instances "$INSTANCE"

# Cleanup
tui_stop
cleanup_instance "$INSTANCE"

echo "✓ Test passed"
```

### Available Helper Functions

**Session Management:**
- `tui_start` - Start tmux session with octez-manager
- `tui_stop` - Stop tmux session
- `tui_send <keys>` - Send keys to TUI
- `tui_capture` - Get current screen content
- `tui_wait_for <text> [timeout]` - Wait for text to appear

**Navigation:**
- `nav_to_install` - Go to install menu
- `nav_to_instances` - Go to instances page
- `nav_back` - Press Escape

**Form Interaction:**
- `form_fill_text <value>` - Fill current field
- `form_next` - Tab to next field
- `form_select_choice <navigation>` - Select from dropdown
- `form_toggle_yes` / `form_toggle_no` - Toggle boolean
- `form_submit` - Submit the form

**Verification:**
- `verify_service_exists <instance> <role>` - Check systemd unit exists
- `verify_service_running <instance> <role>` - Check service is active
- `verify_in_instances <instance>` - Check appears in TUI instances page

**Cleanup:**
- `cleanup_instance <instance>` - Remove service
- `cleanup_all_test_instances` - Remove all test instances

## Debugging Failed Tests

### View Error Screenshots
Failed tests automatically save screen captures to `/tmp/tui-errors/`:
```bash
docker compose exec tui-tester ls -la /tmp/tui-errors/
docker compose exec tui-tester cat /tmp/tui-errors/error-*.txt
```

### View TUI Logs
```bash
docker compose exec tui-tester cat /tmp/tui-session.log
```

### Interactive Debugging
```bash
# Attach to running container
docker compose exec tui-tester bash

# Start TUI manually
source /tests/lib.sh
tui_start

# Interact manually
tmux attach -t om-e2e

# Detach: Ctrl+B, then D
```

### Check Service Status
```bash
docker compose exec tui-tester systemctl --user list-units 'octez-*'
docker compose exec tui-tester journalctl --user -u octez-node@INSTANCE -n 50
```

## Test Independence

**Critical:** Each test MUST be completely independent:
- Use unique instance names
- Cleanup before AND after
- Don't rely on other tests running first
- Tests run in parallel shards

## Coverage Collection

Tests run with instrumented binary and `BISECT_FILE` set. Coverage data is collected to `/coverage/` and merged with CLI integration test coverage.

## Sharding

Tests are distributed across 3 shards for parallel execution. Update `shards.json` when adding tests:

```json
{
  "shards": {
    "1": {
      "tests": ["01-new-test.sh"],
      "estimated_minutes": 5
    }
  }
}
```

Balance shards by estimated runtime.

## Common Issues

### "TUI session failed to start"
- Check if `octez-manager` binary is executable
- Verify `TERM` environment variable is set
- Check logs: `cat /tmp/tui-session.log`

### "Timeout waiting for text"
- Increase timeout value in `tui_wait_for`
- Check if text pattern is correct (partial match)
- View screen: `debug_print_screen`

### "Service not found in systemd"
- Verify instance name is correct
- Check systemd user session: `systemctl --user status`
- May need `loginctl enable-linger tezos`

### "Coverage files not generated"
- Verify `BISECT_FILE` environment is set
- Check binary is instrumented: `file /usr/local/bin/octez-manager`
- Look for `.coverage` files in `/coverage/`

## CI Integration

Tests run automatically in `.github/workflows/coverage.yml`:
- Triggered on every PR (unless `skip-coverage` label)
- Runs in parallel with CLI integration tests
- Coverage merged into final report
- Artifacts saved on failure

## Future Enhancements

- [ ] DAL node installation tests
- [ ] Baker installation tests
- [ ] Accuser installation tests
- [ ] Service action tests (start/stop/restart)
- [ ] Page navigation tests
- [ ] Error handling tests
- [ ] Configuration edit tests
- [ ] Multi-instance scenarios
