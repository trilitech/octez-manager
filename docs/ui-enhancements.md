# Octez Manager UI Enhancement Plan

**Date:** 2025-12-10
**Status:** Planning/Design Document

## Table of Contents

1. [Executive Summary](#executive-summary)
2. [Current State Analysis](#current-state-analysis)
3. [Widget Additions](#widget-additions)
4. [Metrics & Charts](#metrics--charts)
5. [Performance Considerations](#performance-considerations)
6. [Implementation Plan](#implementation-plan)
7. [Technical Details](#technical-details)

---

## Executive Summary

This document outlines planned UI/UX improvements for octez-manager with a focus on:
- **Adding 4 high-value miaou widgets** without adding complexity
- **Smart metrics visualization** tailored to Tezos node behavior
- **Performance-conscious implementation** using Prometheus metrics and head monitors
- **Operational visibility** for resource monitoring

**Key Principle:** Avoid spamming nodes with RPC calls; use streaming monitors and Prometheus metrics where possible.

---

## Current State Analysis

### Current RPC Polling Strategy

From `src/ui/rpc_scheduler.ml`:

```ocaml
(* Polling intervals *)
let boot_pending_interval = 6.0   (* Poll every 6s when not synced *)
let boot_ok_interval = 10.0       (* Poll every 10s when synced *)
let min_spacing = 1.0             (* Minimum 1s between any RPC calls *)
```

**Current implementation:**
- Uses **head monitor** (WebSocket streaming) for real-time block updates (lines 108-166)
- Falls back to **polling** only for bootstrap status and metadata
- Enforces global spacing to prevent RPC stampeding
- Has in-flight cap of 4 concurrent requests

**Good practices already in place:**
✅ Streaming head monitor reduces RPC load
✅ Adaptive polling (slower when synced)
✅ Global rate limiting with min_spacing
✅ Concurrent request capping

### Current Metrics Collected (RPC-based)

From `src/ui/rpc_metrics.ml`:

```ocaml
type rpc_metrics = {
  chain_id : string option;
  head_level : int option;
  bootstrapped : bool option;
  last_rpc_refresh : float option;
  node_version : string option;
  data_size : string option;
  proto : string option;
  last_error : string option;
}
```

**Optimization opportunity:** Many of these metrics are available via Prometheus `/metrics` endpoint.

---

## Widget Additions

### Priority 1: Essential Widgets (Low Complexity, High Value)

#### 1. **Progress_widget** ⭐⭐⭐⭐⭐

**File:** Already exists in miaou: `miaou_widgets/progress_widget.ml`

**Use Cases:**
- Snapshot download progress
- Node initialization/import progress
- Package installation progress

**Current problem:**
- Spinner shows activity but no completion estimate
- Users uncertain if operation takes 30 seconds or 30 minutes

**Implementation:**
```ocaml
(* In context.ml, add *)
let pending_progress = ref None

let set_progress ~label ~value =
  pending_progress := Some (Progress_widget.open_inline ~label ~progress:value ())

(* During snapshot download *)
set_progress ~label:"Downloading snapshot" ~value:0.65
```

**Complexity:** LOW - Swap existing spinner infrastructure
**LOC estimate:** ~50 lines
**Benefits:**
- Reduces user abandonment
- Professional feel
- Clear time expectations

---

#### 2. **File_browser_widget** ⭐⭐⭐⭐⭐

**File:** Already exists in miaou: `miaou_widgets/file_browser_widget.ml`

**Use Cases:**
- Data directory selection: `/home/mathias/.tezos-node`
- App binary directory: `/opt/octez/bin`
- Log file path selection

**Current problem:**
- Users manually type paths (error-prone)
- No visual feedback on write permissions
- Can't easily discover existing installations

**Implementation:**
```ocaml
(* Replace textbox modals in install_node_form.ml *)
let data_dir_browser = File_browser_widget.open_centered
  ~title:"Select data directory"
  ~only_directories:true
  ~show_writability:true ()
```

**Key features:**
- Visual browsing eliminates typos
- Instant writability feedback
- `mkdir_and_cd` support for creating directories on-the-fly
- Path editing mode for quick adjustments

**Complexity:** MEDIUM - Requires modal flow integration
**LOC estimate:** ~150 lines
**Benefits:**
- Eliminates path typos (major error source)
- Discovery of existing nodes
- Visual permission validation

---

#### 3. **Description_list** ⭐⭐⭐⭐

**File:** Already exists in miaou: `miaou_widgets/description_list.ml`

**Use Case:**
- Instance details page (currently 345 lines of manual formatting)

**Current approach:**
```ocaml
(* Manual formatting in instance_details.ml *)
let output = [
  "Instance name: " ^ name;
  "Role: " ^ role;
  "Network: " ^ network;
  (* ... *)
]
```

**New approach:**
```ocaml
let details = Description_list.create
  ~title:"Instance Configuration"
  ~items:[
    ("Instance name", name);
    ("Role", role);
    ("Network", network);
    ("History mode", history_mode);
    ("Data directory", data_dir);
  ] ()
```

**Complexity:** LOW - Drop-in replacement
**LOC estimate:** Reduces 345 lines → ~100 lines
**Benefits:**
- Consistent alignment
- Better scannability
- Easier maintenance

---

#### 4. **Pane_layout** ⭐⭐⭐

**File:** Already exists in miaou: `miaou_widgets/pane_layout.ml`

**Use Case:**
- Contextual help panels in install forms

**Current problem:**
- Help hints appear in footer or require modal switches
- Form fills entire screen; help is separate

**Implementation:**
```ocaml
(* Split screen: Form (60%) | Help (40%) *)
let render_form_with_help model focus =
  let form_content = render_table model.table focus in
  let help_content = match model.selected_field with
    | InstanceName -> "Choose a unique name..."
    | Network -> "Network determines which chain..."
    | HistoryMode -> "Rolling: ~10GB, Full: ~100GB, Archive: ~500GB..."
    | _ -> ""
  in
  Pane_layout.create ~left_ratio:0.6
    ~left:form_content
    ~right:help_content
```

**Complexity:** MEDIUM - Requires layout redesign
**LOC estimate:** ~100 lines
**Benefits:**
- No modal switching for help
- Contextual guidance always visible
- Reduces cognitive load

---

### Widgets NOT Recommended

❌ **Tree_widget** - Minimal implementation (32 lines), no clear use case
❌ **Additional layout widgets** - Redundant with current approach

---

## Metrics & Charts

### Key Insight: Tezos Behavior is Bimodal

**Bootstrap Mode:**
- Fast block ingestion (~1 block/second)
- Progress tracking useful
- Show velocity and ETA

**Synced Mode:**
- Normal operation (~1 block every 6 seconds)
- Progress tracking NOT useful
- Show staleness and chain health instead

### Metric Collection Strategy: Prometheus First

**Octez Metrics Endpoint:** `/metrics` on RPC port (or dedicated port 9932)

**Format:** OpenMetrics/Prometheus compatible
**Documentation:** https://octez.tezos.com/docs/user/node-monitoring.html

**Available Metrics (partial list):**
- `octez_version` - Node version info
- `octez_distributed_db_requester_table_length` - DB metrics
- P2P layer metrics
- Storage/store metrics
- Prevalidator metrics
- Chain validator metrics
- Block validator metrics

**⚠️ Performance Warning from Docs:**
> "Most of the metrics are computed when scraped from the node. As there is no rate limiter, you should consider scraping wisely and adding a proxy for a public endpoint, to limit the impact on performance."

**Strategy:**
1. **Use head monitor** (already implemented) for real-time block updates
2. **Use Prometheus metrics** for node internals (storage size, p2p stats, etc.)
3. **Poll Prometheus** at slower rate than RPC (e.g., 30s for resource stats)
4. **Avoid redundant RPC calls** for data available in metrics

---

### Priority 1: Smart Status Indicators ⭐⭐⭐⭐⭐

**Complexity:** LOW (~150 lines)

#### A. Block Staleness Indicator

**Purpose:** "Is my node stuck or waiting for next block?"

**Visual:**
```
Last Block: 12s ago  ✓ [████░░░░░░] 12s/60s

Status: Normal operation (expecting ~6s/block)
```

**Color coding:**
- 🟢 **Green** (<30s): Healthy - syncing fast or waiting for next block
- 🟡 **Yellow** (30s-2min): Possibly stuck, but could be slow network
- 🔴 **Red** (>2min): Definitely stuck - needs attention

**Implementation:**
```ocaml
(* Add to rpc_metrics.ml *)
type rpc_metrics = {
  (* existing fields *)
  last_block_time : float option;  (* Unix timestamp of last block *)
  recent_block_times : float list; (* Last 10 block reception times *)
}

let render_staleness last_block_time =
  match last_block_time with
  | None -> "No data"
  | Some t ->
      let elapsed = Unix.time () -. t in
      let (color, icon) =
        if elapsed < 30. then (green, "✓")
        else if elapsed < 120. then (yellow, "⚠")
        else (red, "✗")
      in
      sprintf "%s %.0fs ago" (color icon) elapsed
```

**Data source:** Head monitor already provides this via streaming

---

#### B. Chain Health / Hiccup Detection

**Purpose:** "Are blocks coming regularly or is the chain having issues?"

**Metric:** Block time variance over last 10 blocks

**Visual:**
```
Chain Health: ✓ Stable
Block times:  6s 7s 6s 5s 6s 6s 8s 6s 6s 6s
              ═══════════════════════════ ✓

vs.

Chain Health: ⚠ Irregular
Block times:  6s 25s 3s 45s 6s 12s 6s 89s 6s 6s
              ══░░░░══░░░░░░══░░══░░░░░░░░══ ⚠
```

**Implementation:**
```ocaml
let render_chain_health block_times =
  match block_times with
  | [] -> "No data"
  | times ->
      let variance = calculate_variance times in
      if variance < 3. then green "Stable ═══"
      else if variance < 10. then yellow "Irregular ══░"
      else red "Unstable ░░░"

let calculate_variance times =
  let mean = List.fold_left (+.) 0. times /. float (List.length times) in
  let sum_sq_diff = List.fold_left
    (fun acc t -> acc +. ((t -. mean) ** 2.)) 0. times in
  sqrt (sum_sq_diff /. float (List.length times))
```

**Data source:** Track block reception timestamps from head monitor

---

#### C. Bootstrap Phase Detection

**Purpose:** Different UX for catching up vs synced

**Bootstrap mode:**
```
━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━
Mode: ⚡ Bootstrap (catching up)
  Velocity: ~1 block/s ↗
  Behind by: ~450,000 blocks
  ETA: ~5 days

Recent velocity:
  ⣿⣿⣿⣿⣿⣦⣤⣀⠀⣀⣤⣶⣿⣿⣿⣿⣿⣿
━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━
```

**Synced mode:**
```
━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━
Mode: ✓ Synced (following head)
  Last block: 4s ago
  Expected: ~6s/block
  Chain: Stable ═══════════ ✓
━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━
```

**Detection:**
```ocaml
let render_node_status metrics =
  match metrics.bootstrapped with
  | Some false -> render_bootstrap_mode metrics
  | Some true -> render_synced_mode metrics
  | None -> "Unknown"
```

**Data source:** `bootstrapped` field already collected via RPC

---

### Priority 2: Resource Monitoring Dashboard ⭐⭐⭐⭐⭐

**Complexity:** MEDIUM (~400 lines)

**Purpose:** Per-service operational visibility

**Visual:**
```
Resource Usage

Instance            CPU    Memory      Disk I/O    Disk Size
━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━
mainnet-node        35%    4.2GB       ██░░ 2MB/s  89GB/100GB
  CPU:    ████████░░░░░░░░░░ 35%
  Memory: █████████░░░░░░░░░ 42%
  Disk:   ████████████████░░ 89%  [⚠ High]

mainnet-baker       12%    512MB       ░░░░ 10KB/s  125MB
  CPU:    ███░░░░░░░░░░░░░░░ 12%
  Memory: █░░░░░░░░░░░░░░░░░  5%
  Disk:   ░░░░░░░░░░░░░░░░░░  0%

Trends (last 5 min):
  Node CPU:    ⣿⣿⣿⣦⣀⠀⣀⣤⣶⣿⣿⣿⣿
  Node Memory: ⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿  (stable)
  Node Disk:   ⠀⠀⢀⣀⣤⣴⣶⣾⣿⣿⣿⣿⣿  (growing)
```

#### Data Sources (Multiple Options)

**Option 1: Systemd cgroup metrics** (Recommended)
```bash
systemctl show octez-node-mainnet --property=CPUUsageNSec
systemctl show octez-node-mainnet --property=MemoryCurrent
```

**Option 2: /proc filesystem**
```bash
cat /proc/$(systemctl show -p MainPID --value octez-node-mainnet)/stat
cat /proc/$(systemctl show -p MainPID --value octez-node-mainnet)/status
```

**Option 3: Tezos Prometheus metrics**
```bash
curl http://localhost:9732/metrics | grep octez_store
```

**Polling strategy:**
- **CPU/Memory:** Poll every 30s (reasonable for trends)
- **Disk size:** Poll every 5 minutes (slow-changing)
- **Disk I/O:** From `/proc/[pid]/io` every 30s

**Storage:**
- Keep last 60 samples (5 minutes @ 5s interval)
- Ring buffer for memory efficiency
- Calculate trends: `current - value_5min_ago`

#### Implementation

**New module:** `src/ui/resource_monitor.ml`

```ocaml
type process_stats = {
  cpu_percent : float;
  memory_bytes : int64;
  disk_io_rate_read : int64;  (* bytes/sec *)
  disk_io_rate_write : int64; (* bytes/sec *)
  disk_usage : int64;          (* total bytes *)
}

type trend = Stable | Increasing | Decreasing

let fetch_stats service_name : process_stats option =
  (* Implementation using systemctl or /proc *)
  ...

let calculate_trend samples =
  match samples with
  | [] | [_] -> Stable
  | first :: _ as lst ->
      let last = List.hd (List.rev lst) in
      let diff_percent = (last -. first) /. first *. 100. in
      if abs_float diff_percent < 5. then Stable
      else if diff_percent > 0. then Increasing
      else Decreasing
```

**New page:** `src/ui/pages/resource_monitor.ml`

```ocaml
type model = {
  table : Table_widget.t;
  history : (string, float list) Hashtbl.t; (* instance -> cpu samples *)
}

let render model focus size =
  let header = ["Resource Monitor"; ""] in
  let footer = ["[Esc] Back  [r] Refresh"] in

  Vsection.render ~header ~footer ~child:(fun _ ->
    Table_widget.render model.table ~focus
  ) size
```

---

### Priority 3: Bootstrap Velocity Chart ⭐⭐⭐

**Complexity:** LOW (~100 lines)

**Purpose:** ETA during initial sync only

**Show only when:** `bootstrapped = false`

**Visual:**
```
Bootstrap Progress

Current:  L 5,847,234 / 7,500,000  ████████████████░░░░  78%
Rate:     ~150 blocks/sec
ETA:      ~3h 12m

Velocity (last 5 min):
  ⣿⣿⣿⣿⣿⣦⣤⣀⠀⣀⣤⣶⣿⣿⣿⣿⣿⣿
```

**Implementation:**
```ocaml
let calculate_eta current_level target_level recent_samples =
  let blocks_remaining = target_level - current_level in
  match recent_samples with
  | [] | [_] -> None
  | first :: _ as lst ->
      let last = List.hd (List.rev lst) in
      let time_elapsed = 300. in (* 5 minutes *)
      let blocks_processed = last - first in
      let blocks_per_sec = float blocks_processed /. time_elapsed in
      let seconds_remaining = float blocks_remaining /. blocks_per_sec in
      Some seconds_remaining

let render_eta_human seconds =
  let hours = int_of_float (seconds /. 3600.) in
  let minutes = int_of_float ((seconds -. float (hours * 3600)) /. 60.) in
  Printf.sprintf "~%dh %dm" hours minutes
```

---

## Performance Considerations

### Current Baseline (Good Practices)

✅ **Head monitor streaming** (WebSocket) for real-time updates
✅ **Adaptive polling intervals** (6s bootstrapping, 10s synced)
✅ **Global rate limiting** (min 1s spacing between RPC calls)
✅ **Concurrent request cap** (max 4 in-flight)

### Enhancements for New Features

#### 1. Use Prometheus Metrics Where Possible

**Replace RPC calls with `/metrics` scraping for:**
- `data_size` → `octez_store_*` metrics
- `node_version` → `octez_version` metric (static, cache it)
- Storage stats → `octez_distributed_db_*` metrics

**Implementation:**
```ocaml
(* New module: src/ui/prometheus_client.ml *)
let scrape_metrics svc =
  let url = Printf.sprintf "http://%s:%d/metrics"
    svc.rpc_host svc.rpc_port in
  (* Parse Prometheus text format *)
  (* Return key-value map *)
  ...

(* Poll at 30s interval instead of 6-10s for RPC *)
let metrics_poll_interval = 30.0
```

**Benefits:**
- Reduces RPC load on node
- Richer metrics available
- Single HTTP call for many metrics

**Trade-offs:**
- Prometheus endpoint computes metrics on-the-fly (per docs warning)
- Need to poll wisely (30s recommended, not <10s)

---

#### 2. Resource Monitoring Polling Strategy

**Systemd/proc polling:**
- Poll every **30 seconds** for CPU/Memory (reasonable for trend display)
- Poll every **5 minutes** for disk size (slow-changing metric)
- **No RPC calls** for resource stats

**Storage:**
- Ring buffer: 60 samples = 5 minutes of history
- Memory overhead: ~60 * 8 bytes * 4 metrics = ~2KB per instance

---

#### 3. Block Staleness/Chain Health

**No additional RPC calls needed:**
- Already have `last_block_time` from head monitor
- Track reception timestamps in-memory
- Zero network overhead

---

#### 4. Enhanced Metrics Type

**Add to `rpc_metrics.ml`:**
```ocaml
type rpc_metrics = {
  (* Existing fields *)
  chain_id : string option;
  head_level : int option;
  bootstrapped : bool option;
  last_rpc_refresh : float option;
  node_version : string option;
  data_size : string option;
  proto : string option;
  last_error : string option;

  (* NEW: Zero-cost additions from head monitor *)
  last_block_time : float option;        (* Timestamp of last block *)
  recent_block_times : float list;       (* Last 10 inter-block times *)
  recent_block_levels : int list;        (* Last 60 levels for velocity *)
}
```

**Data flow:**
```
Head Monitor (WebSocket)
    ↓ [Real-time, pushed]
Update rpc_metrics
    ↓ [Every block, ~6s]
Calculate derived metrics
    ↓ [In-memory, zero cost]
Render UI
```

---

### Performance Budget

| Feature | RPC Calls | Prometheus Calls | Systemd Calls | Frequency |
|---------|-----------|------------------|---------------|-----------|
| **Current (baseline)** |
| Head monitor | 0 (streaming) | 0 | 0 | Real-time |
| Bootstrap status | 1 | 0 | 0 | 6s / 10s |
| Metadata (version, chain_id) | 1-3 | 0 | 0 | 10s |
| **New features** |
| Block staleness | 0 | 0 | 0 | Real-time (derived) |
| Chain health | 0 | 0 | 0 | Real-time (derived) |
| Resource monitor | 0 | 0 | 3 | 30s |
| Storage metrics | 0 | 1 | 0 | 30s |

**Total overhead:**
- RPC: No increase (reuse head monitor data)
- Prometheus: +1 call per 30s (vs current 0)
- Systemd: +3 calls per 30s (negligible local overhead)

**Network impact:** Minimal - one additional HTTP call per 30s per node

---

## Implementation Plan

### Phase 1: Quick Wins (1-2 days)

#### Day 1: Morning
- [x] Add `Description_list` to instance details page
  - Replace manual formatting
  - **Expected LOC change:** 345 → ~100 lines
  - **Risk:** Low

#### Day 1: Afternoon
- [ ] Add `Progress_widget` for snapshot downloads
  - Hook into existing download progress callbacks
  - Replace spinner in install forms
  - **Expected LOC change:** +50 lines
  - **Risk:** Low

#### Day 2: Morning
- [ ] Enhance `rpc_metrics` type with staleness tracking
  - Add `last_block_time`, `recent_block_times`, `recent_block_levels`
  - Update head monitor callback to populate new fields
  - **Expected LOC change:** +30 lines in `rpc_scheduler.ml`
  - **Risk:** Low

#### Day 2: Afternoon
- [ ] Add block staleness indicator to instances page
  - Render function for staleness badge
  - Color coding: green/yellow/red
  - **Expected LOC change:** +50 lines
  - **Risk:** Low

**Phase 1 Total:** ~130 LOC, 2 days, Low risk

---

### Phase 2: UX Enhancement (3-5 days)

#### Day 3-4: File Browser Integration
- [ ] Add `File_browser_widget` for path selection in install forms
  - Replace data directory textbox
  - Replace binary directory textbox
  - Add writability checks
  - **Expected LOC change:** +150 lines
  - **Risk:** Medium (modal flow changes)

#### Day 5: Chain Health & Bootstrap Detection
- [ ] Add chain health indicator
  - Calculate block time variance
  - Render health badge
  - **Expected LOC change:** +40 lines
  - **Risk:** Low

- [ ] Add bootstrap mode detection
  - Different rendering for bootstrap vs synced
  - Velocity chart during bootstrap
  - **Expected LOC change:** +100 lines
  - **Risk:** Low

**Phase 2 Total:** ~290 LOC, 3 days, Medium risk

---

### Phase 3: Resource Monitoring (5-7 days)

#### Day 6-7: Resource Monitor Module
- [ ] Create `resource_monitor.ml`
  - Systemd stats fetching
  - Sample history storage (ring buffer)
  - Trend calculation
  - **Expected LOC change:** +300 lines
  - **Risk:** Medium (new module)

#### Day 8-9: Resource Monitor Page
- [ ] Create `resource_monitor.ml` page
  - Table widget for instances
  - Bar charts for CPU/Memory/Disk
  - Sparklines for trends
  - **Expected LOC change:** +200 lines
  - **Risk:** Low (reuses existing widgets)

#### Day 10: Prometheus Client (Optional)
- [ ] Add Prometheus metrics scraping
  - Text format parser
  - Integrate with existing RPC scheduler
  - Replace `data_size` with prometheus metrics
  - **Expected LOC change:** +150 lines
  - **Risk:** Medium (new dependency, parsing)

**Phase 3 Total:** ~650 LOC, 5 days, Medium risk

---

### Phase 4: Polish (2-3 days)

#### Day 11-12: Contextual Help Panels
- [ ] Add `Pane_layout` to install forms
  - Split form and help
  - Contextual help text per field
  - **Expected LOC change:** +100 lines + help content
  - **Risk:** Medium (layout redesign)

#### Day 13: Chart Widget Foundation (Optional)
- [ ] Create `miaou_widgets/chart_widget.ml`
  - Sparkline function (braille)
  - Bar chart function (block chars)
  - Line plot function (braille)
  - **Expected LOC change:** +200 lines
  - **Risk:** Low (new widget, no existing code changes)

**Phase 4 Total:** ~300 LOC, 3 days, Medium risk

---

### Total Effort Estimate

**Total LOC:** ~1,370 lines
**Total Time:** 13 days (2.5 weeks)
**Risk Level:** Low-Medium

**Phases prioritized by value:**
1. Phase 1 (Quick wins) - Immediate value, low risk
2. Phase 3 (Resource monitoring) - High operational value
3. Phase 2 (UX enhancement) - Polish, medium value
4. Phase 4 (Optional) - Nice-to-have

---

## Technical Details

### New Modules

#### `src/ui/resource_monitor.ml` (new)
```ocaml
(** Resource monitoring for systemd services *)

type process_stats = {
  cpu_percent : float;
  memory_bytes : int64;
  disk_io_read : int64;
  disk_io_write : int64;
  disk_usage : int64;
}

type sample_history = {
  cpu_samples : float array;
  memory_samples : int64 array;
  cursor : int;
  size : int;
}

val fetch_stats : Service.t -> process_stats option
(** Fetch current resource usage from systemd *)

val init_history : unit -> sample_history
(** Initialize ring buffer for 60 samples *)

val add_sample : sample_history -> process_stats -> unit
(** Add sample to ring buffer *)

val calculate_trend : float array -> trend
(** Calculate Stable/Increasing/Decreasing from samples *)
```

---

#### `src/ui/prometheus_client.ml` (new, optional)
```ocaml
(** Prometheus metrics scraper *)

type metric = {
  name : string;
  labels : (string * string) list;
  value : float;
  timestamp : float option;
}

val scrape : rpc_addr:string -> rpc_port:int -> metric list option
(** Scrape /metrics endpoint and parse text format *)

val find_metric : metric list -> name:string -> metric option
(** Find metric by name *)

val find_labeled : metric list -> name:string -> labels:(string * string) list -> metric option
(** Find metric by name and labels *)
```

**Text format parser (simplified):**
```ocaml
let parse_line line =
  (* Handle comments *)
  if String.starts_with ~prefix:"#" line then None
  else
    (* Parse: metric_name{label="value"} 123.45 *)
    match String.split_on_char ' ' line with
    | [name_and_labels; value] ->
        let (name, labels) = parse_name_and_labels name_and_labels in
        Some {name; labels; value = float_of_string value; timestamp = None}
    | _ -> None
```

---

### Enhanced Data Types

#### `src/ui/rpc_metrics.ml` (modified)
```ocaml
type rpc_metrics = {
  (* Existing fields *)
  chain_id : string option;
  head_level : int option;
  bootstrapped : bool option;
  last_rpc_refresh : float option;
  node_version : string option;
  data_size : string option;
  proto : string option;
  last_error : string option;

  (* NEW: Block staleness tracking *)
  last_block_time : float option;
  (** Unix timestamp of last block reception *)

  (* NEW: Chain health tracking *)
  recent_block_times : float list;
  (** Last 10 inter-block times (in seconds) for variance calculation *)

  (* NEW: Bootstrap velocity tracking *)
  recent_block_levels : int list;
  (** Last 60 block levels (5 min @ 5s/block) for velocity calculation *)
}
```

---

### Chart Rendering Functions

#### Braille Sparkline
```ocaml
(** Render sparkline using braille characters *)
let sparkline (data : float list) : string =
  let braille_chars = [|
    '⠀'; '⡀'; '⡄'; '⡆'; '⡇'; '⣇'; '⣧'; '⣷'; '⣿'
  |] in
  let normalized = normalize_to_range data ~min:0. ~max:8. in
  String.concat "" (List.map (fun v ->
    String.make 1 braille_chars.(int_of_float v)
  ) normalized)
```

#### Block Bar Chart
```ocaml
(** Render horizontal bar *)
let bar ~label ~value ~max_value ~width : string =
  let filled = int_of_float (float width *. value /. max_value) in
  let empty = width - filled in
  Printf.sprintf "%s %s%s %.1f%%"
    label
    (String.make filled '█')
    (String.make empty '░')
    (value /. max_value *. 100.)
```

---

### Systemd Stats Fetching

```ocaml
(** Fetch CPU usage from systemd *)
let fetch_cpu_usage service_name =
  let cmd = Printf.sprintf
    "systemctl show %s --property=CPUUsageNSec --value"
    service_name in
  match Sys.command cmd with
  | 0 ->
      (* Parse nanoseconds, convert to percentage *)
      (* Need to track delta and time elapsed *)
      Some cpu_percent
  | _ -> None

(** Fetch memory usage from systemd *)
let fetch_memory service_name =
  let cmd = Printf.sprintf
    "systemctl show %s --property=MemoryCurrent --value"
    service_name in
  (* Returns bytes *)
  ...

(** Fetch disk usage *)
let fetch_disk_usage data_dir =
  let cmd = Printf.sprintf "du -sb %s" data_dir in
  (* Returns bytes *)
  ...
```

---

## Testing Strategy

### Unit Tests

#### Staleness Calculation
```ocaml
let test_staleness_green () =
  let now = Unix.time () in
  let last_block = now -. 15. in
  let result = classify_staleness ~now ~last_block in
  assert (result = Green)

let test_staleness_yellow () =
  let now = Unix.time () in
  let last_block = now -. 60. in
  let result = classify_staleness ~now ~last_block in
  assert (result = Yellow)
```

#### Chain Health Variance
```ocaml
let test_chain_health_stable () =
  let block_times = [6.; 7.; 6.; 5.; 6.; 6.; 8.; 6.; 6.; 6.] in
  let variance = calculate_variance block_times in
  assert (variance < 3.)

let test_chain_health_unstable () =
  let block_times = [6.; 25.; 3.; 45.; 6.; 12.; 6.; 89.; 6.; 6.] in
  let variance = calculate_variance block_times in
  assert (variance > 10.)
```

#### ETA Calculation
```ocaml
let test_eta_calculation () =
  let current = 5_000_000 in
  let target = 7_500_000 in
  let recent_levels = [4_999_700; 5_000_000] in (* 300 blocks in 5 min *)
  let eta = calculate_eta current target recent_levels in
  (* 300 blocks / 300 sec = 1 block/sec *)
  (* 2,500,000 blocks / 1 block/sec = 2,500,000 sec = ~28 days *)
  assert (Option.is_some eta)
```

### Integration Tests

#### Resource Monitor Polling
```ocaml
let test_resource_monitor_fetch () =
  let service = make_test_service "test-node" in
  let stats = Resource_monitor.fetch_stats service in
  assert (Option.is_some stats);
  match stats with
  | Some s ->
      assert (s.cpu_percent >= 0. && s.cpu_percent <= 100.);
      assert (s.memory_bytes > 0L)
  | None -> assert false
```

---

## Appendix: Miaou Widgets Summary

### Currently Used (7 widgets)
1. `Widgets` (core) - Text styling, colors, box drawing
2. `Pager_widget` - Multi-line text viewer
3. `Table_widget` - Tabular data display
4. `Select_widget` - Single-selection dropdown
5. `Textbox_widget` - Text input
6. `Validated_textbox_widget` - Text input with validation
7. `Vsection` - Vertical layout section

### Recommended Additions (4 widgets)
8. `Progress_widget` - Progress bars
9. `File_browser_widget` - File/directory picker
10. `Description_list` - Key-value display
11. `Pane_layout` - Two-pane layouts

### Total: 11 widgets (manageable, focused)

---

## References

1. **Octez Node Monitoring:** https://octez.tezos.com/docs/user/node-monitoring.html
2. **Prometheus Text Format:** https://prometheus.io/docs/instrumenting/exposition_formats/
3. **Systemd Resource Control:** https://www.freedesktop.org/software/systemd/man/systemd.resource-control.html
4. **Miaou Documentation:** (local) `/home/mathias/dev/tezos/octez_setup/src/miaou/`

---

**Document Version:** 1.0
**Last Updated:** 2025-12-10
**Author:** Generated via Claude Code
**Status:** Ready for Implementation
