# Miaou Global Keys API Design

**Date:** 2025-12-10
**Context:** Clean API for registering and validating global keyboard shortcuts

---

## Design Overview

### **Two-Phase Registration**

1. **Application startup:** Register global keys (once)
2. **Page registration:** Validate page keys against globals (build-time or module-load)

### **Optional PPX:** Compile-time validation for both

---

## API Design

### **Phase 1: Register Global Keys**

```ocaml
(* src/main.ml or src/app.ml *)

let () =
  (* Register global shortcuts before any pages *)
  Miaou.Core.Global_keys.register [
    ("s", "Settings", fun () -> Context.navigate "settings");
    ("?", "Help", fun () -> Modal_helpers.show_help ());
    ("h", "Help", fun () -> Modal_helpers.show_help ());
    ("m", "Menu", fun () -> Modal_helpers.show_menu ());
    ("q", "Quit", fun () -> Context.quit_with_confirmation ());
  ];

  (* Now register pages - they'll be validated *)
  register_pages ()
```

### **Phase 2: Register Page with Key Declaration**

```ocaml
(* src/ui/pages/instances.ml *)

let name = "instances"

(** Keys this page handles *)
let handled_keys = [
  ("c", "Create instance");
  ("f", "Filter");
  ("r", "Toggle resources");
  ("s", "Start service");  (* ❌ Will fail: conflicts with global "s" *)
]

let register () =
  Miaou.Core.Registry.register
    (module struct
      type state = ...
      type msg = ...

      let name = name
      let handled_keys = handled_keys  (* NEW: required field *)
      let init = init
      let update = update
      let view = view
      let handle_key = handle_key
      ...
    end)
```

---

## Implementation in Miaou

### **1. Global Keys Registry**

```ocaml
(* miaou/miaou_core/global_keys.ml *)

module Global_keys = struct
  type handler = unit -> unit

  type registration = {
    key : string;
    description : string;
    handler : handler;
  }

  let registry : registration list ref = ref []

  let register keys =
    registry := keys @ !registry

  let is_registered key =
    List.exists (fun r -> r.key = key) !registry

  let handle key =
    match List.find_opt (fun r -> r.key = key) !registry with
    | Some r -> r.handler (); true
    | None -> false

  let all () = !registry

  let reserved_keys () =
    List.map (fun r -> r.key) !registry
end
```

### **2. Enhanced Page Signature**

```ocaml
(* miaou/miaou_core/tui_page.mli *)

module type PAGE_SIG = sig
  type state
  type msg

  val name : string

  (** NEW: Declare which keys this page handles.
      List of (key, description) tuples.
      Will be validated against Global_keys at registration. *)
  val handled_keys : (string * string) list

  val init : unit -> state
  val update : state -> msg -> state
  val view : state -> focus:bool -> size:LTerm_geom.size -> string
  val handle_key : state -> string -> size:LTerm_geom.size -> state
  ...
end
```

### **3. Registry with Validation**

```ocaml
(* miaou/miaou_core/registry.ml *)

let validate_keys page_name page_keys =
  let reserved = Global_keys.reserved_keys () in
  let conflicts =
    List.filter (fun (key, _desc) ->
      List.mem key reserved
    ) page_keys
  in
  match conflicts with
  | [] -> Ok ()
  | conflicts ->
      let keys = List.map fst conflicts in
      Error (
        Printf.sprintf
          "Page '%s' tries to use reserved keys: %s\nReserved by: %s"
          page_name
          (String.concat ", " keys)
          (String.concat ", "
             (List.map (fun k ->
               let r = List.find (fun r -> r.Global_keys.key = k)
                         (Global_keys.all ()) in
               Printf.sprintf "'%s' (%s)" k r.description
             ) keys))
      )

let register (module P : PAGE_SIG) =
  (* Validate keys *)
  match validate_keys P.name P.handled_keys with
  | Error msg ->
      (* Fail at module load time *)
      failwith msg
  | Ok () ->
      (* Continue with normal registration *)
      let page = make_page (module P) in
      add_to_registry P.name page
```

### **4. Driver Integration**

```ocaml
(* miaou/miaou_driver_term/lambda_term_driver.ml *)

let run_page page =
  ...
  let rec event_loop state =
    ...
    let key_str = parse_lterm_event event in

    (* Check global keys first (if no modal) *)
    let state' =
      if not (Modal_manager.has_active ()) then
        if Global_keys.handle key_str then
          (* Global key handled, don't pass to page *)
          state
        else
          (* Not global, pass to page *)
          page.handle_key state key_str ~size
      else
        (* Modal active, pass to modal *)
        Modal_manager.handle_key key_str;
        state
    in
    event_loop state'
  ...
```

---

## Usage Example

### **Application Setup**

```ocaml
(* octez-manager/src/main.ml *)

let register_global_shortcuts () =
  Miaou.Core.Global_keys.register [
    ("s", "Open Settings", fun () ->
      Context.navigate "settings");

    ("?", "Show Help", fun () ->
      Modal_helpers.show_help_modal ());

    ("h", "Show Help (alternate)", fun () ->
      Modal_helpers.show_help_modal ());

    ("m", "Open Menu", fun () ->
      Modal_helpers.show_menu_modal ());

    ("q", "Quit", fun () ->
      Context.quit_with_confirmation ());
  ]

let run () =
  (* 1. Register globals FIRST *)
  register_global_shortcuts ();

  (* 2. Register pages (will be validated) *)
  Instances.register ();
  Instance_details.register ();
  Install_node_form.register ();
  Install_baker_form.register ();
  Settings.register ();

  (* 3. Run app *)
  Manager_app.run ()
```

### **Page Implementation**

```ocaml
(* octez-manager/src/ui/pages/instances.ml *)

let name = "instances"

(** Keys handled by this page *)
let handled_keys = [
  ("c", "Create new instance");
  ("f", "Cycle filter");
  ("r", "Toggle resource view");
  ("Tab", "Toggle view mode");
  ("Up", "Move selection up");
  ("Down", "Move selection down");
  ("Enter", "Open actions menu");
  ("Esc", "Back");
  (* NOT 's' - reserved for Settings *)
]

let handle_key state key ~size =
  if Modal_manager.has_active () then (
    Modal_manager.handle_key key;
    check_navigation state
  ) else
    (* Global keys already handled by driver, won't reach here *)
    match key with
    | "c" -> create_instance state
    | "f" -> cycle_filter state
    | "r" | "Tab" -> toggle_view_mode state
    | "Up" -> move_selection state (-1)
    | "Down" -> move_selection state 1
    | "Enter" -> handle_enter state
    | "Esc" -> {state with next_page = Some "__BACK__"}
    | _ -> state

let register () =
  Miaou.Core.Registry.register
    (module struct
      type state = state
      type msg = msg

      let name = name
      let handled_keys = handled_keys  (* Validated here *)
      let init = init
      let update = update
      let view = view
      let handle_key = handle_key
      ...
    end)
```

---

## Error Messages

### **Conflict Detection**

```bash
$ dune build

Fatal error: exception Failure("Page 'instances' tries to use reserved keys: s
Reserved by: 's' (Open Settings)")

Build failed!
```

### **Helpful Messages**

```ocaml
let validate_keys page_name page_keys =
  ...
  Error (
    Printf.sprintf
      "Page '%s' conflicts with global shortcuts:\n\n\
       Page uses:     %s\n\
       Reserved for:  %s\n\n\
       Hint: Use different keys or remove from handled_keys list.\n\
       Available keys: c, d, e, f, i, j, k, l, n, o, p, r, t, u, v, w, x, y, z"
      page_name
      (String.concat ", " (List.map (fun (k, d) -> Printf.sprintf "'%s' (%s)" k d) conflicts))
      (String.concat ", " (List.map ... globals))
  )
```

---

## PPX Extension (Optional)

### **Compile-Time Validation**

```ocaml
(* ppx_miaou/ppx_miaou_keys.ml *)

(** PPX to validate handled_keys at compile time *)

(* Reads global keys from a generated file or environment *)
let reserved_keys = ["s"; "?"; "h"; "m"; "q"]  (* from codegen *)

let validate_handled_keys ~loc keys =
  match keys with
  | [%expr [ [%e? _]; ... ] ] ->
      (* Extract key strings from AST *)
      let key_strs = extract_strings keys in
      let conflicts = List.filter (fun k -> List.mem k reserved_keys) key_strs in
      if conflicts <> [] then
        Location.raise_errorf ~loc
          "Keys %s are reserved for global shortcuts"
          (String.concat ", " conflicts)
      else
        keys
  | _ -> keys

(* Transform: *)
(* let handled_keys = [("s", "Start")] *)
(* → compile error at this location *)
```

### **Usage with PPX**

```ocaml
(* dune *)
(library
 (name octez_manager_ui_pages)
 (preprocess (pps ppx_miaou_keys)))

(* instances.ml *)
let handled_keys = [
  ("c", "Create");
  ("s", "Start");  (* PPX ERROR HERE at compile time *)
]
```

---

## Migration Path

### **Phase 1: Runtime Validation (Immediate)**

```ocaml
(* Add to miaou: *)
- Global_keys.register()
- PAGE_SIG.handled_keys field
- Registry.validate_keys()

(* In octez-manager: *)
- Add handled_keys to each page
- Call Global_keys.register() in main

(* Result: Module-load time errors *)
```

### **Phase 2: PPX Validation (Optional)**

```ocaml
(* Add ppx_miaou_keys *)
- Compile-time checking
- Better error locations
- IDE integration

(* Result: Compile-time errors *)
```

---

## Benefits

### **1. Centralized Global Keys**

```ocaml
(* All globals in one place *)
let () =
  Global_keys.register [
    ("s", "Settings", ...);
    ("?", "Help", ...);
    (* Easy to see all reserved keys *)
  ]
```

### **2. Clear Conflicts**

```ocaml
(* Error tells you exactly what's wrong *)
Page 'instances' tries to use reserved keys: s
Reserved by: 's' (Open Settings)
```

### **3. Self-Documenting**

```ocaml
(* Page declares its keys explicitly *)
let handled_keys = [
  ("c", "Create instance");
  ("f", "Filter");
  (* Readable! *)
]
```

### **4. Help System Integration**

```ocaml
(* Generate help from declarations *)
let show_help page_name =
  let globals = Global_keys.all () in
  let page_keys = get_page_keys page_name in

  print_endline "Global Shortcuts:";
  List.iter (fun (k, desc, _) ->
    Printf.printf "  %s - %s\n" k desc
  ) globals;

  print_endline "\nPage Shortcuts:";
  List.iter (fun (k, desc) ->
    Printf.printf "  %s - %s\n" k desc
  ) page_keys
```

### **5. Tab Completion / IDE Support**

```ocaml
(* IDE can suggest available keys based on what's registered *)
```

---

## Implementation Checklist

### **For Miaou:**

- [ ] Add `miaou/miaou_core/global_keys.ml`
- [ ] Add `handled_keys` field to `PAGE_SIG`
- [ ] Add `validate_keys()` to `Registry.register()`
- [ ] Update driver to check globals before page
- [ ] Add tests for conflict detection
- [ ] Document API in miaou README

### **For Octez-Manager:**

- [ ] Add `handled_keys` to all pages:
  - [ ] `instances.ml`
  - [ ] `instance_details.ml`
  - [ ] `install_node_form.ml`
  - [ ] `install_baker_form.ml`
  - [ ] `snapshots.ml`
  - [ ] `settings.ml` (future)
- [ ] Call `Global_keys.register()` in main
- [ ] Test that conflicts are caught
- [ ] Update documentation

### **Optional PPX:**

- [ ] Create `ppx_miaou_keys` package
- [ ] Add to miaou build
- [ ] Test compile-time errors
- [ ] Add IDE integration

---

## Example: Complete Flow

```ocaml
(* 1. Main.ml - register globals FIRST *)
let () =
  Miaou.Core.Global_keys.register [
    ("s", "Settings", fun () -> Context.navigate "settings");
  ];
  register_pages ()

(* 2. Instances.ml - declare page keys *)
let handled_keys = [
  ("c", "Create");
  ("s", "Start");  (* ❌ *)
]

let register () =
  Registry.register (module ...)

(* 3. Registry validates *)
let register (module P) =
  match validate_keys P.name P.handled_keys with
  | Error "Page 'instances' tries to use reserved keys: s" ->
      failwith ...  (* ❌ Build fails! *)

(* 4. Fix the page *)
let handled_keys = [
  ("c", "Create");
  ("t", "Start");  (* ✅ Changed to 't' *)
]

(* 5. Build succeeds! *)
```

---

## Comparison: Before vs After

### **Before (No Protection)**

```ocaml
(* main.ml *)
let handle_global key = ...

(* instances.ml - whoops! *)
let handle_key state key =
  match key with
  | "s" -> start_service state  (* Silently conflicts with global *)

(* Runtime: pressing 's' → unpredictable behavior *)
```

### **After (With Validation)**

```ocaml
(* main.ml *)
Global_keys.register [("s", "Settings", ...)];

(* instances.ml *)
let handled_keys = [("s", "Start")]  (* ❌ Caught at registration *)

(* Build fails with clear error *)
```

---

## Summary

**API Design:**
```ocaml
(* 1. Register globals first *)
Global_keys.register [(key, description, handler), ...]

(* 2. Pages declare their keys *)
let handled_keys = [(key, description), ...]

(* 3. Validation at page registration *)
Registry.register (module P)  (* Checks P.handled_keys *)
```

**Enforcement:**
- Runtime: Module load time (immediate, no PPX needed)
- Compile-time: PPX optional for even earlier catching

**Result:**
- ✅ Centralized global key management
- ✅ Clear conflict detection
- ✅ Self-documenting code
- ✅ Help system integration
- ✅ No string magic, explicit declarations

---

**Document Version:** 1.0
**Last Updated:** 2025-12-10
**Status:** API Design Ready for Implementation
