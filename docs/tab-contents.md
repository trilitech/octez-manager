# Tab Contents Specification

**Date:** 2025-12-10
**Context:** Defining what each top-level tab would contain in octez-manager

## Current Pages Analysis

**Registered pages:**
1. `instances.ml` - Main dashboard (ACTIVE)
2. `instance_details.ml` - Detail view for single instance
3. `install_node_form.ml` - Install wizard for nodes
4. `install_baker_form.ml` - Install wizard for bakers

**Unregistered pages:**
5. `snapshots.ml` - Snapshot browser (EXISTS but not registered)

**Navigation flow:**
```
Instances (main)
    ├─> Instance Details → back to Instances
    ├─> Install Node Form → back to Instances
    └─> Install Baker Form → back to Instances
```

---

## Tab Strategy: Three Tabs Only

After reviewing the codebase, I recommend **THREE tabs** instead of four:

```
[Dashboard] [Snapshots] [Settings]
```

**Why only three?**
- "Resources" is really part of "Dashboard" - it's instance monitoring
- Keep it simple - tabs should be major functional areas
- Terminal width constraint (each tab ~12 chars = 40 chars for 3 tabs)

---

## Tab 1: Dashboard 🏠

**Keyboard shortcut:** `1` or `Home`

### Content

#### Main View: Instance List (current instances.ml)
```
┌────────────────────────────────────────────────────────────────────────┐
│ [Dashboard] [Snapshots] [Settings]                        ● USER       │
│ 3 instances | Filter: all | Last refresh: 2s ago                       │
├────────────────────────────────────────────────────────────────────────┤
│                                                                         │
│ ➤ [ Install new instance ]                                             │
│                                                                         │
│   ● mainnet-node   node      rolling    mainnet      [enabled]        │
│                    ✓ synced · L5847234 · proto:PsQueb · 4s ago        │
│                                                                         │
│   ● archive-node   node      archive    mainnet      [enabled]        │
│                    ⚠ catching up · L4523122 · proto:PsQueb · 2s ago   │
│                                                                         │
│   ○ test-baker     baker     inherited   inherited    [disabled]       │
│                    RPC not available; use logs                         │
│                                                                         │
├────────────────────────────────────────────────────────────────────────┤
│ 1-3: tabs  r: resources  ↑/↓: move  Enter: actions  f: filter  c: new │
└────────────────────────────────────────────────────────────────────────┘
```

#### Sub-view: Resource Monitor (press `r` or toggle with `d`)
```
┌────────────────────────────────────────────────────────────────────────┐
│ [Dashboard] [Snapshots] [Settings]                        ● USER       │
│ Resource Monitor | 3 instances | Auto-refresh: 30s                     │
├────────────────────────────────────────────────────────────────────────┤
│                                                                         │
│ mainnet-node                                                           │
│   CPU:    35% ████████░░░░░░░░░░ │ ⣿⣿⣿⣦⣀⠀⣀⣤⣶⣿⣿ (stable)        │
│   Memory: 4.2G ████████░░░░░░░░░ │ ⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿ (stable)       │
│   Disk:   89G ████████████████░░ │ ⠀⠀⢀⣀⣤⣴⣶⣾⣿⣿⣿⣿ (growing ↗)    │
│   I/O:    2.1 MB/s read, 450 KB/s write                                │
│                                                                         │
│ archive-node                                                           │
│   CPU:    68% ██████████████░░░░ │ ⣿⣿⣿⣿⣿⣿⣦⣤⣀⠀⣀⣤ (variable)      │
│   Memory: 8.9G ████████████████░ │ ⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿ (stable)       │
│   Disk:   340G ███████████████████ │ ⠀⠀⠀⢀⣀⣤⣴⣶⣾⣿⣿ (growing ↗↗)  │
│   I/O:    8.4 MB/s read, 3.2 MB/s write                                │
│                                                                         │
│ test-baker                                                             │
│   Status: Stopped (no resource usage)                                  │
│                                                                         │
├────────────────────────────────────────────────────────────────────────┤
│ i: instances  r: resources  ↑/↓: scroll  Esc: back                    │
└────────────────────────────────────────────────────────────────────────┘
```

### Features

**Instance List (Main):**
- [x] List all managed services (nodes, bakers, accusers, etc.)
- [x] Filter by role: all, node, baker, accuser, signer, dal-node, sr-node
- [x] Real-time status indicators (green/yellow/red)
- [x] RPC metrics: sync status, head level, protocol, chain ID
- [x] Service enablement status
- [x] Quick actions: start, stop, restart, remove
- [x] Create new instance button (top of list)
- [x] Auto-refresh every 5 seconds

**Resource Monitor (Sub-view):**
- [ ] Per-service CPU usage with sparkline trend
- [ ] Per-service memory usage with sparkline trend
- [ ] Per-service disk usage with sparkline trend
- [ ] Disk I/O rates (read/write)
- [ ] Trend indicators: stable ═══, increasing ↗, decreasing ↘
- [ ] Auto-refresh every 30 seconds
- [ ] Toggle between instance list and resource view

### Keyboard Shortcuts

| Key | Action |
|-----|--------|
| `1` or `Home` | Switch to Dashboard tab |
| `↑`/`↓` or `k`/`j` | Navigate instances |
| `Enter` | Open action menu for selected instance |
| `c` | Create new instance (opens install wizard) |
| `f` | Cycle through role filters (all → node → baker → ...) |
| `r` or `d` | Toggle resource monitor view |
| `i` | Return to instance list (when in resource view) |
| `Space` | Expand/collapse instance details (future) |
| `b` | Bulk actions (future) |

### Navigation Flow

```
Dashboard
    ├─> Enter on instance → Instance Details page
    │                          └─> Actions menu (start/stop/restart/remove/logs)
    │                          └─> Esc → Back to Dashboard
    │
    ├─> 'c' or Enter on "Install" → Install wizard flow
    │                                 ├─> Node installation form
    │                                 ├─> Baker installation form
    │                                 └─> Esc → Back to Dashboard
    │
    └─> 'r' → Resource Monitor view
                └─> 'i' or Esc → Back to instance list
```

---

## Tab 2: Snapshots 📦

**Keyboard shortcut:** `2`

### Content

```
┌────────────────────────────────────────────────────────────────────────┐
│ [Dashboard] [Snapshots] [Settings]                        ● USER       │
│ Network: mainnet | Source: tzinit.org | Updated: 2h ago                │
├────────────────────────────────────────────────────────────────────────┤
│                                                                         │
│ Available Snapshots                                                    │
│                                                                         │
│ ➤ mainnet-rolling                                                      │
│   Size: 12.3 GB | Block: L5847234 | Updated: 2h ago                   │
│   URL: https://snapshots.tzinit.org/mainnet/rolling/...               │
│                                                                         │
│   mainnet-full                                                         │
│   Size: 234 GB | Block: L5847234 | Updated: 2h ago                    │
│   URL: https://snapshots.tzinit.org/mainnet/full/...                  │
│                                                                         │
│   mainnet-archive                                                      │
│   Size: 1.2 TB | Block: L5847234 | Updated: 2h ago                    │
│   URL: https://snapshots.tzinit.org/mainnet/archive/...               │
│                                                                         │
│ Imported Snapshots                                                     │
│                                                                         │
│   local-mainnet-rolling.snapshot                                       │
│   Size: 12.1 GB | Imported: 2025-12-08 | Used by: mainnet-node        │
│                                                                         │
├────────────────────────────────────────────────────────────────────────┤
│ n: network  Enter: import  d: download  i: import from file  Esc: back│
└────────────────────────────────────────────────────────────────────────┘
```

### Features

**Available Snapshots:**
- [ ] Browse snapshots from tzinit.org (or other sources)
- [ ] Filter by network: mainnet, ghostnet, weeklynet, etc.
- [ ] Show snapshot metadata: size, block height, history mode, timestamp
- [ ] Direct download URLs
- [ ] Import snapshot to managed instance
- [ ] Preview snapshot details

**Imported/Local Snapshots:**
- [ ] List locally imported snapshots
- [ ] Show which instances use which snapshots
- [ ] Delete unused snapshots
- [ ] Manage snapshot storage

### Keyboard Shortcuts

| Key | Action |
|-----|--------|
| `2` | Switch to Snapshots tab |
| `↑`/`↓` or `k`/`j` | Navigate snapshot list |
| `n` | Select network (mainnet, ghostnet, weeklynet, etc.) |
| `Enter` | Import selected snapshot (choose target instance) |
| `d` | Download snapshot to disk (without importing) |
| `i` | Import from local file |
| `Delete` | Delete local snapshot (if not in use) |
| `Space` | Preview snapshot details |

### Navigation Flow

```
Snapshots
    ├─> 'n' → Network selector modal
    │          └─> Select network → Refresh snapshot list
    │
    ├─> Enter on snapshot → Import flow
    │                        ├─> Select target instance
    │                        ├─> Show progress bar
    │                        └─> Success/Error message
    │
    ├─> 'd' → Download flow
    │          ├─> Choose destination directory (file browser)
    │          ├─> Show progress bar
    │          └─> Success/Error message
    │
    └─> 'i' → Import from file
               ├─> File browser to select .snapshot file
               ├─> Select target instance
               └─> Import and show progress
```

### Implementation Status

**Current state:**
- ✅ Page exists (`snapshots.ml`)
- ❌ Not registered in `manager_app.ml`
- ⚠️ Network selection TODO (line 99)
- ⚠️ Import flow TODO (line 106)

**Required work:**
1. Register page in manager_app
2. Implement network selection (context or state)
3. Implement import flow (call backend, show progress)
4. Add local snapshot management
5. Add file browser integration for import from file

---

## Tab 3: Settings ⚙️

**Keyboard shortcut:** `3`

### Content

```
┌────────────────────────────────────────────────────────────────────────┐
│ [Dashboard] [Snapshots] [Settings]                        ● USER       │
│ Configuration                                                          │
├────────────────────────────────────────────────────────────────────────┤
│                                                                         │
│ ➤ General                                                              │
│                                                                         │
│   Default Paths                                                        │
│     Data directory:     /home/mathias/.local/share/octez              │
│     Binary directory:   /usr/local/bin                                │
│     Log directory:      /var/log/octez                                │
│                                                                         │
│   Service Defaults                                                     │
│     Service user:       octez                                         │
│     Enable on boot:     Yes                                           │
│     Start immediately:  No                                            │
│     Logging mode:       journald                                      │
│                                                                         │
│   UI Settings                                                          │
│     Auto-refresh:       5 seconds                                     │
│     Resource polling:   30 seconds                                    │
│     Theme:              Default (purple/blue)                         │
│     Unicode borders:    Enabled                                       │
│                                                                         │
│   Network Defaults                                                     │
│     Default network:    mainnet                                       │
│     Default history:    rolling                                       │
│     Default RPC port:   8732                                          │
│     Default P2P port:   9732                                          │
│                                                                         │
│   Advanced                                                             │
│     RPC timeout:        30 seconds                                    │
│     RPC max retries:    3                                             │
│     Debug logging:      Disabled                                      │
│     Metrics polling:    Prometheus (30s)                              │
│                                                                         │
├────────────────────────────────────────────────────────────────────────┤
│ Enter: edit  r: reset to defaults  s: save  Esc: back                 │
└────────────────────────────────────────────────────────────────────────┘
```

### Features

**Configuration Management:**
- [ ] Default paths for new instances
- [ ] Default service settings (user, enable on boot, logging)
- [ ] UI preferences (refresh rates, theme, borders)
- [ ] Network defaults (network, history mode, ports)
- [ ] Advanced settings (timeouts, retries, debug mode)
- [ ] Save configuration to file (~/.config/octez-manager/config.json)
- [ ] Reset to defaults
- [ ] Validate settings before saving

**Settings Categories:**
1. **General** - Basic app behavior
2. **Default Paths** - Where things are created
3. **Service Defaults** - Template for new instances
4. **UI Settings** - Appearance and refresh rates
5. **Network Defaults** - Default ports and networks
6. **Advanced** - Debug, timeouts, experimental features

### Keyboard Shortcuts

| Key | Action |
|-----|--------|
| `3` | Switch to Settings tab |
| `↑`/`↓` or `k`/`j` | Navigate settings |
| `Enter` | Edit selected setting |
| `r` | Reset all to defaults |
| `s` | Save configuration |
| `Esc` | Discard changes and return (with confirmation) |

### Navigation Flow

```
Settings
    ├─> Enter on setting → Edit modal
    │                      ├─> Textbox for paths/strings
    │                      ├─> Select widget for choices (Yes/No, theme, etc.)
    │                      ├─> Validated textbox for numbers (ports, seconds)
    │                      └─> File browser for directories
    │
    ├─> 's' → Save configuration
    │          ├─> Write to ~/.config/octez-manager/config.json
    │          └─> Show success toast
    │
    └─> 'r' → Reset to defaults
               ├─> Confirmation modal
               └─> Reset all settings
```

### Configuration File Format

**Location:** `~/.config/octez-manager/config.json`

```json
{
  "version": "1.0",
  "defaults": {
    "paths": {
      "data_dir": "/home/mathias/.local/share/octez",
      "binary_dir": "/usr/local/bin",
      "log_dir": "/var/log/octez"
    },
    "service": {
      "user": "octez",
      "enable_on_boot": true,
      "start_immediately": false,
      "logging_mode": "journald"
    },
    "network": {
      "default_network": "mainnet",
      "default_history_mode": "rolling",
      "default_rpc_port": 8732,
      "default_p2p_port": 9732
    },
    "ui": {
      "auto_refresh_interval": 5,
      "resource_poll_interval": 30,
      "theme": "default",
      "unicode_borders": true
    },
    "advanced": {
      "rpc_timeout": 30,
      "rpc_max_retries": 3,
      "debug_logging": false,
      "metrics_source": "prometheus",
      "metrics_poll_interval": 30
    }
  }
}
```

### Implementation Status

**Current state:**
- ❌ Settings page does not exist
- ❌ Configuration file not implemented
- ⚠️ Some defaults are hardcoded in various files

**Required work:**
1. Create `settings.ml` page
2. Create configuration module (`config.ml`)
3. Define config schema (JSON or TOML)
4. Load config on startup
5. Apply defaults when creating new instances
6. Settings editor UI with validation
7. Config file I/O (load/save)

---

## Alternative: Two Tabs Only

If Settings feels premature, start with **TWO tabs**:

```
[Dashboard] [Snapshots]
```

**Rationale:**
- Settings can be accessed via menu (`m` key) or command-line flags initially
- Dashboard includes both instances and resources
- Snapshots is a distinct functional area
- Simpler to implement and maintain

**Settings via Menu:**
```
Menu (press 'm')
  ├─> Instances (Dashboard)
  ├─> Snapshots
  ├─> Settings
  ├─> About
  └─> Quit
```

---

## Implementation Phases

### Phase 1: Two-Tab Layout (1 week)
1. ✅ Keep Dashboard as-is (instances list)
2. ✅ Register Snapshots page
3. ✅ Complete Snapshots network selection
4. ✅ Complete Snapshots import flow
5. ✅ Add tab bar widget
6. ✅ Wire up `1`/`2` keyboard shortcuts
7. ✅ Test navigation

### Phase 2: Resource Monitor (1 week)
8. ✅ Create resource monitoring module
9. ✅ Add resource view to Dashboard
10. ✅ Toggle between instances and resources with `r`
11. ✅ Add sparklines and trend indicators
12. ✅ Test polling and performance

### Phase 3: Settings Tab (1 week)
13. ✅ Create Settings page
14. ✅ Define configuration schema
15. ✅ Implement config file I/O
16. ✅ Add Settings to tab bar (becomes 3-tab layout)
17. ✅ Apply defaults from config when creating instances
18. ✅ Test configuration persistence

---

## Keyboard Shortcut Summary

### Global Shortcuts (work on any tab)

| Key | Action |
|-----|--------|
| `1` | Dashboard tab |
| `2` | Snapshots tab |
| `3` | Settings tab (Phase 3) |
| `m` | Menu (opens modal) |
| `Esc` | Back / Cancel |
| `Ctrl+C` or `q` | Quit (with confirmation) |
| `?` or `h` | Help |

### Dashboard Shortcuts

| Key | Action |
|-----|--------|
| `↑`/`↓`, `k`/`j` | Navigate instances |
| `Enter` | Open action menu |
| `c` | Create new instance |
| `f` | Cycle filter (all/node/baker/...) |
| `r` or `d` | Toggle resource monitor |
| `i` | Back to instance list (from resources) |
| `Space` | Expand details (future) |

### Snapshots Shortcuts

| Key | Action |
|-----|--------|
| `↑`/`↓`, `k`/`j` | Navigate snapshots |
| `n` | Select network |
| `Enter` | Import snapshot |
| `d` | Download snapshot |
| `i` | Import from file |

### Settings Shortcuts

| Key | Action |
|-----|--------|
| `↑`/`↓`, `k`/`j` | Navigate settings |
| `Enter` | Edit setting |
| `s` | Save configuration |
| `r` | Reset to defaults |

---

## Visual Summary

### Three-Tab Layout (Final)

```
┌────────────────────────────────────────────────────────────────────────┐
│ [Dashboard] [Snapshots] [Settings]                        ● USER       │
│ ...tab-specific header...                                              │
├────────────────────────────────────────────────────────────────────────┤
│                                                                         │
│                                                                         │
│                     TAB CONTENT AREA                                   │
│                                                                         │
│                                                                         │
├────────────────────────────────────────────────────────────────────────┤
│ 1-3: tabs  ...tab-specific shortcuts...  Esc: back/quit               │
└────────────────────────────────────────────────────────────────────────┘
```

### Tab Navigation Flow

```
            Tab 1: Dashboard                Tab 2: Snapshots           Tab 3: Settings
                   |                               |                         |
         ┌─────────┴─────────┐             ┌──────┴──────┐           ┌──────┴──────┐
         |                   |             |             |           |             |
    Instances List    Resources View   Available   Local/Imported   General   Advanced
         |                   |          Snapshots    Snapshots      Settings  Settings
         |                   |             |             |              |         |
    Instance Details    (view only)   Import Flow  Manage Snapshots  Edit    Reset/Save
         |                                                             Value
    Action Menu
    (start/stop/remove/logs)
```

---

## Recommendation

**Start with Phase 1: Two-Tab Layout**
- `[Dashboard] [Snapshots]`
- Keep Settings in menu for now
- Focus on completing Snapshots functionality
- Add resource monitoring as sub-view in Dashboard

**Then Phase 2: Add Resource Monitor**
- Toggle with `r` key in Dashboard
- Provides operational visibility without new tab

**Finally Phase 3: Add Settings Tab** (if needed)
- `[Dashboard] [Snapshots] [Settings]`
- Once config persistence is important
- When default values become cumbersome to manage

**Benefits of phased approach:**
- Deliver value incrementally
- Test tab navigation with 2 tabs first
- Add Settings only when configuration grows complex
- Keep cognitive load low for users

---

## Next Steps

1. **Review this document** - Does the tab content make sense?
2. **Decide on 2-tab or 3-tab** start
3. **Prioritize Snapshots completion** - network selection + import flow
4. **Design Tab_bar widget** (or use simple string formatting)
5. **Implement keyboard shortcuts** (1/2/3 keys)
6. **Test navigation flow**

---

**Document Version:** 1.0
**Last Updated:** 2025-12-10
**Status:** Design/Planning
