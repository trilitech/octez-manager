# Installation Progress Modal - UI Mockup

This document provides a visual representation of the installation progress modal.

## Modal Layout

```
┌─────────────────────────────────────────────────────────────────────┐
│                     Installing Node Instance                        │
├─────────────────────────────────────────────────────────────────────┤
│                                                                      │
│   ○ Setup directories and config                                    │
│   ◐ Download snapshot                                               │
│   ○ Import snapshot                                                  │
│   ○ Configure service                                                │
│                                                                      │
│   Download snapshot                                                  │
│   [████████████████░░░░░░░░░░░░░░░░] 45%                           │
│                                                                      │
│                                                                      │
│   l: show logs  Esc: cancel (background)                           │
└─────────────────────────────────────────────────────────────────────┘
```

## Modal States

### Initial State (Setup in Progress)

```
┌─────────────────────────────────────────────────────────────────────┐
│                     Installing Node Instance                        │
├─────────────────────────────────────────────────────────────────────┤
│                                                                      │
│   ◐ Setup directories and config                                    │
│   ○ Download snapshot                                                │
│   ○ Import snapshot                                                  │
│   ○ Configure service                                                │
│                                                                      │
│   Setup directories and config                                       │
│   [████░░░░░░░░░░░░░░░░░░░░░░░░░░] 25%                             │
│                                                                      │
│                                                                      │
│   l: show logs  Esc: cancel (background)                           │
└─────────────────────────────────────────────────────────────────────┘
```

### Download in Progress (75%)

```
┌─────────────────────────────────────────────────────────────────────┐
│                     Installing Node Instance                        │
├─────────────────────────────────────────────────────────────────────┤
│                                                                      │
│   ✓ Setup directories and config                                    │
│   ◐ Download snapshot                                               │
│   ○ Import snapshot                                                  │
│   ○ Configure service                                                │
│                                                                      │
│   Download snapshot                                                  │
│   [██████████████████████░░░░░] 75%                                 │
│                                                                      │
│                                                                      │
│   l: show logs  Esc: cancel (background)                           │
└─────────────────────────────────────────────────────────────────────┘
```

### Import in Progress

```
┌─────────────────────────────────────────────────────────────────────┐
│                     Installing Node Instance                        │
├─────────────────────────────────────────────────────────────────────┤
│                                                                      │
│   ✓ Setup directories and config                                    │
│   ✓ Download snapshot                                               │
│   ◐ Import snapshot                                                 │
│   ○ Configure service                                                │
│                                                                      │
│   Import snapshot                                                    │
│   [██████████████████████████████████░░░] 75%                       │
│                                                                      │
│                                                                      │
│   l: show logs  Esc: cancel (background)                           │
└─────────────────────────────────────────────────────────────────────┘
```

### With Logs Enabled

```
┌─────────────────────────────────────────────────────────────────────┐
│                     Installing Node Instance                        │
├─────────────────────────────────────────────────────────────────────┤
│                                                                      │
│   ✓ Setup directories and config                                    │
│   ✓ Download snapshot                                               │
│   ◐ Import snapshot                                                 │
│   ○ Configure service                                                │
│                                                                      │
│   Import snapshot                                                    │
│   [██████████████████████████████████░░░] 75%                       │
│                                                                      │
│   Recent logs:                                                       │
│     importing data from snapshot /path/to/snapshot                   │
│     chain TEZOS_SEOULNET_2025-07-11T08:00:00Z                       │
│     block hash BLEqXQY5eDJUKn8q2Zpq31mcCyd6zkzcrffkET3T...          │
│                                                                      │
│   l: hide logs  Esc: cancel (background)                           │
└─────────────────────────────────────────────────────────────────────┘
```

### Without Snapshot (Simplified)

```
┌─────────────────────────────────────────────────────────────────────┐
│                     Installing Node Instance                        │
├─────────────────────────────────────────────────────────────────────┤
│                                                                      │
│   ◐ Setup directories and config                                    │
│   ○ Configure service                                                │
│                                                                      │
│   Setup directories and config                                       │
│   [████████████████████████████░░░░] 50%                            │
│                                                                      │
│                                                                      │
│   l: show logs  Esc: cancel (background)                           │
└─────────────────────────────────────────────────────────────────────┘
```

## Legend

- `○` - Pending step (not started)
- `◐` - In progress step (currently executing)
- `✓` - Completed step (finished successfully)
- `✗` - Failed step (error occurred) [not shown in normal flow]

## Color Scheme

- Pending steps: Default/dim color
- In progress: Orange/yellow (fg 33)
- Complete: Green (fg 22)
- Failed: Red (fg 160)
- Progress bar: Filled portion in color, empty portion dim

## Interactive Controls

- **'l' or 'L'**: Toggle log output display
- **Esc**: Cancel modal (installation continues in background)

## Behavior

1. Modal opens immediately when user clicks "Confirm & Install"
2. First step marked as in progress automatically
3. Progress callbacks from installer update step status
4. Download progress updates progress bar within download step
5. Progress bar label shows current step name
6. Modal auto-closes on successful completion
7. On error, modal closes and error modal appears
8. Installation runs in background task - UI remains responsive
