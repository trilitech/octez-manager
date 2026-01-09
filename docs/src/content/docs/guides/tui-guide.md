---
title: Using the TUI
description: Navigate and use the Octez Manager terminal interface
---

# Using the TUI

Octez Manager provides a full-featured Terminal User Interface (TUI) for managing your Tezos infrastructure.

## Launching the TUI

```bash
# User mode
octez-manager

# System mode (as root)
sudo octez-manager
```

## Main Dashboard

The main screen shows all your instances with their status:

```
┌─ Instances ─────────────────────────────────────────┐
│  mainnet-node     node      ● running    mainnet   │
│  my-baker         baker     ● running    mainnet   │
│  ghostnet-node    node      ○ stopped    ghostnet  │
└─────────────────────────────────────────────────────┘
```

Status indicators:
- `●` Green: Running
- `○` Gray: Stopped
- `!` Red: Error/Failed

## Navigation

| Key | Action |
|-----|--------|
| `↑`/`↓` or `j`/`k` | Move selection |
| `Enter` | View instance details |
| `Tab` | Switch panels |
| `q` | Quit / Go back |
| `?` | Show help |

## Instance Actions

Select an instance and use these keys:

| Key | Action |
|-----|--------|
| `s` | Start/Stop service |
| `r` | Restart service |
| `l` | View logs |
| `e` | Edit configuration |
| `d` | Delete instance |

## Installing Services

Press `i` to open the install menu:

```
┌─ Install ───────────┐
│  Node              │
│  Baker             │
│  Accuser           │
│  DAL Node          │
│  Signer            │
└────────────────────┘
```

Navigate and press `Enter` to start the installation wizard.

## Log Viewer

The log viewer (`l`) shows real-time logs:

```
┌─ Logs: mainnet-node ─────────────────────────────────┐
│ Source: journald . r: refresh . t: toggle . /: search│
│                                                       │
│ Jan 09 12:00:01 validator: block BLabc... validated  │
│ Jan 09 12:00:02 p2p: 50 connections established      │
│ Jan 09 12:00:03 mempool: 12 pending operations       │
└───────────────────────────────────────────────────────┘
```

Log viewer keys:
| Key | Action |
|-----|--------|
| `f` | Toggle follow mode |
| `w` | Toggle line wrap |
| `t` | Toggle source (journald/daily logs) |
| `/` | Search |
| `n`/`N` | Next/previous match |
| `g`/`G` | Go to top/bottom |
| `Esc` | Exit log viewer |

## Form Navigation

When filling out forms:

| Key | Action |
|-----|--------|
| `Tab` / `↓` | Next field |
| `Shift+Tab` / `↑` | Previous field |
| `Enter` | Submit form / Select option |
| `Space` | Toggle checkbox |
| `Esc` | Cancel |

## Tips

### Quick Actions

- From the main screen, press the first letter of an action for quick access
- Use `?` anywhere to see context-specific help

### Multiple Instances

- Use meaningful instance names (e.g., `mainnet-baker`, `ghostnet-node`)
- The TUI shows all instances grouped by type

### Monitoring

- Keep the TUI open to monitor services
- Logs auto-refresh when in follow mode
- Instance status updates automatically
