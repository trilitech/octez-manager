---
title: RPC Browser
description: Interactively explore and query Tezos RPC endpoints
---

The RPC Browser lets you explore and query RPC endpoints on any managed or external node. It provides instant navigation through the endpoint tree with JSON syntax highlighting and response metrics.

## Opening RPC Browser

From the main screen, press `r` to open the RPC Browser.

Alternatively, select a node instance, press `Enter`, and choose **Browse RPC** from the action menu.

## Quick Access Shortcuts

When at the root of the endpoint tree, number keys jump directly to common endpoints:

| Key | Endpoint | Description |
|-----|----------|-------------|
| `1` | `/version` | Node version info |
| `2` | `/chains/main/blocks/head` | Latest block |
| `3` | `/chains/main/is_bootstrapped` | Bootstrap status |
| `4` | `/network/connections` | Connected peers |
| `5` | `/config/network` | Network configuration |

## Navigating Endpoints

The browser shows three types of entries:

| Type | Indicator | Description |
|------|-----------|-------------|
| `[SUB]` | Subdirectory | Navigate into with `Enter` |
| `[GET]` | GET endpoint | Execute request with `Enter` |
| `[DYN:type]` | Dynamic segment | Prompts for value (e.g., block hash) |

### Navigation Keys

| Key | Action |
|-----|--------|
| `↑`/`↓` or `j`/`k` | Move cursor |
| `Enter` | Select entry or execute GET |
| `Backspace` or `u` | Go up one level |
| `Esc` | Back to previous page |
| `Tab` | Cycle between instances |
| `r` | Refresh entries |

## Executing Requests

When you select a `[GET]` endpoint, the browser executes the request and displays the JSON response with syntax highlighting.

The header shows:
- Request URL
- Response time in milliseconds
- Response size in bytes/KB

### Result View Keys

| Key | Action |
|-----|--------|
| `↑`/`↓` or `j`/`k` | Scroll line by line |
| `PgUp`/`PgDn` | Scroll by page |
| `g` | Jump to top |
| `G` | Jump to bottom |
| `Esc` | Return to endpoint list |

## Dynamic Segments

Some endpoints require dynamic values like chain IDs or block hashes. The browser uses smart defaults:

| Segment | Default |
|---------|---------|
| `chain_id` | `main` |
| `block_id` | `head` |
| `block_hash` | `head` |

When prompted, press `Enter` to accept the default or type a custom value.

## Switching Instances

Press `Tab` to cycle through available node instances. The current instance is shown in the header with its network.

## Example: Checking Node Sync Status

1. Press `r` to open RPC Browser
2. Press `3` for quick access to `/chains/main/is_bootstrapped`
3. View the response showing sync status

Or navigate manually:
1. Select `chains` → `Enter`
2. Type `main` when prompted for chain_id → `Enter`
3. Select `is_bootstrapped` → `Enter`

## Tips

- Quick access shortcuts work only at the root level
- The browser remembers your position when switching instances
- Response times help identify slow endpoints
- Use `Tab` to compare responses across different nodes
