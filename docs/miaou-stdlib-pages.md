# Miaou Standard Library: Reusable Pages

**Date:** 2025-12-10
**Context:** Pre-built, parameterized pages for common TUI application needs

---

## Motivation

**Every TUI app needs:**
1. ✅ Settings/configuration management
2. ✅ Log viewing
3. ✅ Help/about page
4. ✅ Command execution with output

**Current situation:** Everyone reimplements these.

**Proposal:** Miaou provides reusable, parameterized page implementations.

---

## 1. Settings/Config Page

### **API Design**

```ocaml
(* miaou/miaou_stdlib/settings_page.mli *)

(** Configuration value types *)
type value =
  | String of string
  | Int of int
  | Bool of bool
  | Float of float
  | Choice of string * string list  (* selected, options *)
  | Path of string  (* file/directory path *)

(** Configuration item *)
type item = {
  key : string;           (* JSON key *)
  label : string;         (* Display label *)
  description : string;   (* Help text *)
  value_type : value;     (* Type and default *)
  validator : value -> (unit, string) result option;  (* Optional validation *)
}

(** Settings page configuration *)
module type CONFIG = sig
  (** Where to read/write config *)
  val config_path : string

  (** Configuration schema *)
  val items : item list

  (** Visual customization *)
  val title : string
  val theme : Theme.t option  (* Optional color theme *)

  (** Callbacks *)
  val on_save : (string * value) list -> unit
  val on_reset : unit -> unit
end

(** Create settings page from config *)
module Make (Config : CONFIG) : Miaou.Core.Tui_page.PAGE_SIG
```

### **Usage Example**

```ocaml
(* octez-manager: src/ui/pages/settings.ml *)

module Config : Settings_page.CONFIG = struct
  let config_path = "~/.config/octez-manager/config.json"

  let items = [
    { key = "default_network";
      label = "Default Network";
      description = "Network used when creating new nodes";
      value_type = Choice ("mainnet", ["mainnet"; "ghostnet"; "weeklynet"]);
      validator = None };

    { key = "default_data_dir";
      label = "Default Data Directory";
      description = "Base directory for node data";
      value_type = Path "/home/user/.local/share/octez";
      validator = Some (fun v ->
        match v with
        | Path p when Sys.file_exists p && Sys.is_directory p -> Ok ()
        | Path p -> Error (sprintf "Directory '%s' doesn't exist" p)
        | _ -> Error "Invalid type") };

    { key = "auto_refresh_interval";
      label = "Auto-refresh Interval (seconds)";
      description = "How often to refresh instance status";
      value_type = Int 5;
      validator = Some (fun v ->
        match v with
        | Int n when n >= 1 && n <= 60 -> Ok ()
        | Int n -> Error "Must be between 1 and 60"
        | _ -> Error "Invalid type") };

    { key = "enable_on_boot_default";
      label = "Enable on Boot (default)";
      description = "Whether new services are enabled on boot by default";
      value_type = Bool true;
      validator = None };
  ]

  let title = "Settings"
  let theme = None  (* Use default theme *)

  let on_save configs =
    (* Called when user saves *)
    Context.toast_success "Settings saved!"

  let on_reset () =
    (* Called when user resets to defaults *)
    Context.toast_info "Settings reset to defaults"
end

module Settings_page = Settings_page.Make (Config)
```

### **Visual Layout**

```
┌────────────────────────────────────────────────────────────┐
│ Settings                                                   │
├────────────────────────────────────────────────────────────┤
│                                                            │
│ Default Network                                            │
│   ➤ mainnet                                                │
│   Network used when creating new nodes                     │
│                                                            │
│ Default Data Directory                                     │
│   /home/user/.local/share/octez                            │
│   Base directory for node data                             │
│                                                            │
│ Auto-refresh Interval (seconds)                            │
│   [     5    ]                                             │
│   How often to refresh instance status                     │
│                                                            │
│ Enable on Boot (default)                                   │
│   [✓] Yes                                                  │
│   Whether new services are enabled on boot                 │
│                                                            │
├────────────────────────────────────────────────────────────┤
│ Enter: edit  s: save  r: reset  Esc: back                 │
└────────────────────────────────────────────────────────────┘
```

### **Features**

- ✅ Automatic JSON serialization/deserialization
- ✅ Type-safe value editing (string, int, bool, choice, path)
- ✅ Inline validation with error messages
- ✅ File browser for Path values
- ✅ Select widget for Choice values
- ✅ Save/reset/cancel operations
- ✅ Help text per setting
- ✅ Dirty state tracking (unsaved changes warning)

---

## 2. Log Viewer Page

### **API Design**

```ocaml
(* miaou/miaou_stdlib/log_viewer_page.mli *)

(** Log level *)
type level = Debug | Info | Warn | Error

(** Log entry *)
type entry = {
  timestamp : float;      (* Unix timestamp *)
  level : level;
  source : string;        (* e.g., "instances", "rpc_client" *)
  message : string;
}

(** Log source *)
type source =
  | File of string              (* Read from file *)
  | Stream of (unit -> entry option)  (* Pull from stream *)
  | Eio_stream of entry Eio.Stream.t  (* Eio stream *)

(** Log viewer configuration *)
module type CONFIG = sig
  (** Log source *)
  val source : source

  (** Maximum entries to keep in memory *)
  val max_entries : int  (* default: 500 *)

  (** Visual customization *)
  val title : string
  val theme : Theme.t option

  (** Log formatting *)
  val format_entry : entry -> string

  (** Initial filter (can be changed by user) *)
  val initial_filter : level option  (* None = show all *)
end

(** Create log viewer page *)
module Make (Config : CONFIG) : Miaou.Core.Tui_page.PAGE_SIG
```

### **Usage Example**

```ocaml
(* octez-manager: src/ui/pages/logs.ml *)

module Config : Log_viewer_page.CONFIG = struct
  let source =
    Log_viewer_page.File "~/.local/share/octez-manager/app.log"

  let max_entries = 1000

  let title = "Logs"
  let theme = None

  let format_entry entry =
    let level_str = match entry.level with
      | Debug -> Widgets.dim "DEBUG"
      | Info -> Widgets.blue "INFO "
      | Warn -> Widgets.yellow "WARN "
      | Error -> Widgets.red "ERROR"
    in
    let time_str =
      let tm = Unix.localtime entry.timestamp in
      Printf.sprintf "%02d:%02d:%02d"
        tm.tm_hour tm.tm_min tm.tm_sec
    in
    let source_str = Widgets.dim (Printf.sprintf "[%s]" entry.source) in
    Printf.sprintf "%s %s %s %s"
      time_str level_str source_str entry.message

  let initial_filter = None  (* Show all levels *)
end

module Logs_page = Log_viewer_page.Make (Config)
```

### **Visual Layout**

```
┌────────────────────────────────────────────────────────────┐
│ Logs | Filter: All | 1000 entries | Follow: ON             │
├────────────────────────────────────────────────────────────┤
│                                                            │
│ 14:23:45 INFO  [instances] Loaded 3 services              │
│ 14:23:46 DEBUG [rpc] GET http://localhost:8732/chains/... │
│ 14:23:46 INFO  [rpc] mainnet-node: synced, L5847234       │
│ 14:23:47 WARN  [rpc] test-baker: connection timeout       │
│ 14:23:50 INFO  [instances] Refreshing status              │
│ 14:23:51 ERROR [rpc] Failed to connect to mainnet-node    │
│ 14:23:51 DEBUG [rpc] Retrying in 5 seconds                │
│ ...                                                        │
│ ▼                                                          │
├────────────────────────────────────────────────────────────┤
│ f: filter  /: search  F: follow  c: clear  Esc: back      │
└────────────────────────────────────────────────────────────┘
```

### **Features**

- ✅ Tail-like behavior (follow mode)
- ✅ Filter by level (Debug/Info/Warn/Error)
- ✅ Search within logs
- ✅ Auto-scroll in follow mode
- ✅ Manual scroll (disables follow)
- ✅ Color-coded levels
- ✅ Timestamps
- ✅ Ring buffer (max entries)
- ✅ Clear logs
- ✅ Copy to clipboard (if terminal supports)

---

## 3. Integrated Logging System

### **API Design**

```ocaml
(* miaou/miaou_stdlib/logger.mli *)

(** Logger module that integrates with log viewer *)
module type LOGGER = sig
  (** Log levels *)
  type level = Debug | Info | Warn | Error

  (** Initialize logger with output path *)
  val init : ?level:level -> string -> unit

  (** Log functions *)
  val debug : ?source:string -> string -> unit
  val info : ?source:string -> string -> unit
  val warn : ?source:string -> string -> unit
  val error : ?source:string -> string -> unit

  (** Printf-style logging *)
  val debugf : ?source:string -> ('a, unit, string, unit) format4 -> 'a
  val infof : ?source:string -> ('a, unit, string, unit) format4 -> 'a
  val warnf : ?source:string -> ('a, unit, string, unit) format4 -> 'a
  val errorf : ?source:string -> ('a, unit, string, unit) format4 -> 'a

  (** Stream for live viewing *)
  val stream : unit -> Log_viewer_page.entry Eio.Stream.t
end

(** Create logger *)
val make : unit -> (module LOGGER)
```

### **Usage Example**

```ocaml
(* octez-manager: src/logger.ml *)

module Log = (val Miaou_stdlib.Logger.make () : Miaou_stdlib.Logger.LOGGER)

let () =
  Log.init ~level:Info "~/.local/share/octez-manager/app.log"

(* Elsewhere in code: *)
Log.infof ~source:"instances" "Loaded %d services" count;
Log.warnf ~source:"rpc" "Connection timeout for %s" instance_name;
Log.error ~source:"install" "Failed to create systemd service";

(* In log viewer config: *)
module Config = struct
  let source = Log_viewer_page.Eio_stream (Log.stream ())
  ...
end
```

---

## 4. Command Execution with Logging

### **API Design**

```ocaml
(* miaou/miaou_stdlib/command_runner.mli *)

(** Command execution result *)
type result = {
  exit_code : int;
  stdout : string;
  stderr : string;
  duration : float;
}

(** Command runner configuration *)
module type CONFIG = sig
  (** Logger to use *)
  module Log : Logger.LOGGER

  (** Log source name *)
  val source : string

  (** Whether to log commands before execution *)
  val log_commands : bool

  (** Whether to log stdout/stderr *)
  val log_output : bool

  (** Log level for commands *)
  val command_level : Logger.level

  (** Log level for output *)
  val output_level : Logger.level
end

(** Command runner *)
module type RUNNER = sig
  (** Run command, return result *)
  val run : string -> string list -> result

  (** Run command, log everything *)
  val run_logged : string -> string list -> result

  (** Run command in background, return handle *)
  val run_async : string -> string list -> (unit -> result option)
end

(** Create command runner *)
module Make (Config : CONFIG) : RUNNER
```

### **Usage Example**

```ocaml
(* octez-manager: src/command_runner.ml *)

module Config : Command_runner.CONFIG = struct
  module Log = Logger.Log
  let source = "exec"
  let log_commands = true
  let log_output = true
  let command_level = Info
  let output_level = Debug
end

module Runner = Command_runner.Make (Config)

(* Run command with automatic logging *)
let create_systemd_service name =
  let result = Runner.run_logged "systemctl"
    ["enable"; "--now"; name] in

  match result.exit_code with
  | 0 ->
      Log.infof "Successfully enabled service %s" name;
      Ok ()
  | code ->
      Log.errorf "Failed to enable service %s (exit %d)" name code;
      Error result.stderr

(* Logs automatically show: *)
(* INFO  [exec] Running: systemctl enable --now octez-node-mainnet *)
(* DEBUG [exec] stdout: Created symlink ... *)
(* INFO  [exec] Command completed in 0.234s (exit code: 0) *)
```

---

## 5. Help/About Page

### **API Design**

```ocaml
(* miaou/miaou_stdlib/help_page.mli *)

(** Help section *)
type section = {
  title : string;
  content : string;  (* Markdown-formatted *)
}

(** Help page configuration *)
module type CONFIG = sig
  val app_name : string
  val version : string
  val description : string

  (** Help sections *)
  val sections : section list

  (** Key bindings *)
  val key_bindings : (string * string) list

  (** Authors/contributors *)
  val authors : string list

  (** Links *)
  val links : (string * string) list  (* (label, url) *)
end

(** Create help page *)
module Make (Config : CONFIG) : Miaou.Core.Tui_page.PAGE_SIG
```

### **Usage Example**

```ocaml
(* octez-manager: src/ui/pages/help.ml *)

module Config : Help_page.CONFIG = struct
  let app_name = "octez-manager"
  let version = "0.1.0"
  let description = "Minimal TUI for managing Octez services"

  let sections = [
    { title = "Overview";
      content = "Octez Manager helps you install and manage Octez nodes, \
                 bakers, and other services using systemd." };

    { title = "Getting Started";
      content = "1. Press 'c' to create a new instance\n\
                 2. Follow the installation wizard\n\
                 3. Start/stop services from the main view" };
  ]

  let key_bindings = [
    ("↑/↓", "Navigate");
    ("Enter", "Open actions");
    ("c", "Create instance");
    ("f", "Filter");
    ("s", "Settings");
    ("?", "Help");
  ]

  let authors = [
    "Nomadic Labs";
    "Contributors: ...";
  ]

  let links = [
    ("GitHub", "https://github.com/nomadic-labs/octez-manager");
    ("Documentation", "https://tezos.gitlab.io/");
  ]
end

module Help_page = Help_page.Make (Config)
```

---

## 6. Implementation in Miaou

### **Directory Structure**

```
miaou/
  miaou_stdlib/
    dune
    settings_page.ml
    settings_page.mli
    log_viewer_page.ml
    log_viewer_page.mli
    logger.ml
    logger.mli
    command_runner.ml
    command_runner.mli
    help_page.ml
    help_page.mli
    theme.ml              (* Color themes *)
    theme.mli
```

### **Dune Configuration**

```ocaml
(library
 (name miaou_stdlib)
 (public_name miaou.stdlib)
 (libraries
  miaou.core
  miaou.widgets.display
  miaou.widgets.input
  miaou.widgets.layout
  yojson
  eio))
```

---

## 7. Benefits

### **For Library Users (octez-manager, etc.)**

✅ **Instant settings UI** - No need to build forms, validation, JSON I/O
✅ **Built-in logging** - Integrated log viewer + logger module
✅ **Command execution** - Automatic logging of shell commands
✅ **Consistent UX** - All apps using miaou_stdlib look familiar
✅ **Less code** - Don't reimplement common patterns

### **For Miaou**

✅ **Showcase features** - Demonstrates what's possible
✅ **Battle-tested** - Real usage improves library
✅ **Adoption** - Easier to get started with miaou
✅ **Documentation** - Examples serve as docs

---

## 8. Advanced: Composable Pages

### **Tab Container Page**

```ocaml
(* miaou/miaou_stdlib/tab_container_page.mli *)

(** Tab definition *)
type tab = {
  key : string;              (* Keyboard shortcut *)
  label : string;            (* Display label *)
  page : (module PAGE_SIG);  (* Page module *)
}

module type CONFIG = sig
  val tabs : tab list
  val initial_tab : int  (* Index of default tab *)
end

module Make (Config : CONFIG) : PAGE_SIG
```

### **Usage**

```ocaml
(* Compose multiple stdlib pages into tabs *)
module Config : Tab_container_page.CONFIG = struct
  let tabs = [
    { key = "1"; label = "Instances"; page = (module Instances_page) };
    { key = "2"; label = "Logs"; page = (module Logs_page) };
    { key = "3"; label = "Settings"; page = (module Settings_page) };
  ]

  let initial_tab = 0
end

module App = Tab_container_page.Make (Config)
```

---

## 9. Migration Path for Octez-Manager

### **Phase 1: Add Logging**

```ocaml
(* Use miaou_stdlib logger *)
module Log = (val Miaou_stdlib.Logger.make ())

(* Log throughout codebase *)
Log.infof ~source:"rpc" "Fetching head for %s" instance;
```

### **Phase 2: Add Log Viewer**

```ocaml
(* Create logs page using stdlib *)
module Logs_page = Log_viewer_page.Make (struct
  let source = Eio_stream (Log.stream ())
  ...
end)

(* Register *)
Registry.register_factory (module Logs_page.Make)
```

### **Phase 3: Replace Settings with Stdlib Version**

```ocaml
(* Delete custom settings implementation *)
(* Use stdlib settings page *)
module Settings_page = Settings_page.Make (struct
  let config_path = "~/.config/octez-manager/config.json"
  let items = [...]
  ...
end)
```

### **Result:**

- ✅ ~500 lines of code deleted (settings + logging)
- ✅ Better logging infrastructure
- ✅ Consistent UX
- ✅ Easier to maintain

---

## 10. Comparison: Before vs After

### **Without miaou_stdlib:**

```ocaml
(* octez-manager has to implement: *)
- Settings page: 300+ lines
- Log viewing: 200+ lines
- JSON config I/O: 100+ lines
- Command execution with logging: 150+ lines
- Help page: 100+ lines

Total: ~850 lines of boilerplate
```

### **With miaou_stdlib:**

```ocaml
(* octez-manager just configures: *)
module Settings = Settings_page.Make (Config)  (* 50 lines of config *)
module Logs = Log_viewer_page.Make (Config)    (* 30 lines *)
module Help = Help_page.Make (Config)          (* 40 lines *)

Total: ~120 lines of config
Savings: ~730 lines!
```

---

## Summary

### **Proposed Stdlib Pages:**

1. ✅ **Settings/Config Page** - JSON-backed configuration UI
2. ✅ **Log Viewer Page** - Tail-like log viewing
3. ✅ **Logger Module** - Integrated logging system
4. ✅ **Command Runner** - Shell execution with auto-logging
5. ✅ **Help/About Page** - Documentation display
6. ✅ **Tab Container** - Compose pages into tabs

### **Benefits:**

- 🚀 **Rapid prototyping** - Working app in minutes
- 📦 **Reusable components** - Don't reinvent the wheel
- 🎨 **Consistent UX** - All apps look familiar
- 📚 **Documentation** - Examples serve as docs
- 🐛 **Battle-tested** - Shared code = shared quality

### **Implementation Effort:**

- Settings page: ~400 lines
- Log viewer: ~300 lines
- Logger: ~200 lines
- Command runner: ~150 lines
- Help page: ~200 lines
- Tab container: ~150 lines

**Total: ~1400 lines** for entire stdlib

**Savings per app: ~700+ lines** of boilerplate

---

**Document Version:** 1.0
**Last Updated:** 2025-12-10
**Status:** Design Proposal - Ready for Feedback
