---
title: RPC Browser
description: Interactively explore and query Tezos RPC endpoints
---

The RPC Browser lets you explore and query RPC endpoints on any managed or external node. It provides instant navigation through the endpoint tree with JSON syntax highlighting and response metrics.

![RPC Browser](/gifs/rpc_browser.gif)

## Opening RPC Browser

From the Instances page, press `r` to open the RPC Browser.

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
| `/` | Search in response |
| `n` / `N` | Next / previous search match |
| `Space` | Toggle fold at cursor (for JSON objects/arrays) |
| `f` | Fold all JSON sections |
| `F` | Unfold all JSON sections |
| `s` | Save response to file |
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

## Streaming Endpoints

Some RPC endpoints stream continuous data instead of returning a single response. These include:

- `/monitor/heads/main` — Stream new block headers as they're baked
- `/monitor/bootstrapped` — Stream bootstrap progress
- `/monitor/validated_blocks` — Stream validated blocks
- `/monitor/applied_operations` — Stream applied operations

### How Streaming Works

1. **Automatic Detection** — The browser detects streaming endpoints automatically based on the path
2. **Live Updates** — Data appears in real-time as it streams from the node
3. **JSON Highlighting** — Each streamed JSON object is syntax-highlighted and separated
4. **Continuous** — Streaming continues until you leave the page or close the pager

### Example: Monitoring Block Headers

1. Press `r` to open RPC Browser
2. Navigate to `monitor` → `heads` → accept `main` for chain_id
3. Watch as new block headers appear in real-time

The pager header shows **"Streaming..."** while data is being received.

### Stopping a Stream

- Press `Esc` to return to the endpoint list (stops the stream)
- Press `x` to close the current pager (if multiple pagers exist)

## Multi-Pager Mode

The RPC browser supports up to 10 simultaneous result panes (numbered 0-9), useful for comparing responses across instances or monitoring multiple endpoints at once.

![Multi-Pager Mode](/gifs/rpc_browser_wide.gif)

### Creating Pagers

Press `S` (capital S) to **split** and create a new pager. The new pager appears next to existing ones, and you can execute a different query in each.

### Pager Management Keys

| Key | Action |
|-----|--------|
| `S` | Create new pager (split) — up to 10 total |
| `Tab` | Cycle focus through pagers |
| `Ctrl-x` then `0-9` | Jump directly to pager N (chord: hold Ctrl, press x, release, press number) |
| `x` | Close the focused pager (cannot close the last one) |
| `@` or `t` | Change target instance for the focused pager |
| `s` | Save focused pager's response to a JSON file |

### Targeting Different Instances

Each pager can query a different node instance:

1. Focus a pager with `Tab` or `Ctrl-x N`
2. Press `@` or `t` to open the instance selector
3. Choose a different node (local instance or public node)
4. Execute a query — it goes to that pager's target

This lets you compare the same endpoint across multiple nodes side-by-side.

### Layout Modes

The browser automatically adjusts layout based on terminal width:

| Terminal Width | Layout |
|----------------|--------|
| < 140 columns | Stacked — use `←`/`→` to switch between browser and pagers |
| ≥ 140 columns | Side-by-side — browser tree on left, pagers on right |

In side-by-side mode:
- Use `←`/`→` to switch focus between the browser panel and pager panel
- Multiple pagers stack vertically or in a grid depending on count

### Example: Comparing Block Heights Across Nodes

1. Press `r` to open RPC Browser
2. Press `2` to query `/chains/main/blocks/head` on first node
3. Press `S` to create a second pager
4. Press `@` and select a different node instance
5. Press `2` again to query the same endpoint
6. Compare the `level` field in both responses

### Example: Monitoring Multiple Streams

1. Navigate to `/monitor/heads/main` and press `Enter`
2. Press `S` to create a second pager
3. Press `@` to select a different node
4. Navigate to the same monitor endpoint
5. Watch both streams update in real-time

## Tips

- Quick access shortcuts work only at the root level
- The browser remembers your position when switching instances
- Response times help identify slow endpoints
- Use `Tab` to compare responses across different nodes
- Use multi-pager mode (`S`) to compare the same endpoint across different nodes
