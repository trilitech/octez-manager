# Functor-Based Global Configuration

**Date:** 2025-12-10
**Context:** Using functors to enforce global key constraints at module instantiation time

---

## The Insight

If **pages are functors** that take a global config module, then:

1. ✅ Config defined once at top level
2. ✅ Pages validated at functor application time
3. ✅ Type system prevents missing config
4. ✅ Module-load time checking (early!)
5. ✅ Very OCaml-idiomatic

---

## Design

### **1. Global Config Signature**

```ocaml
(* miaou/miaou_core/global_config.mli *)

module type GLOBAL_CONFIG = sig
  (** Global keyboard shortcuts *)
  type global_handler = unit -> unit

  type global_key = {
    key : string;
    description : string;
    handler : global_handler;
  }

  (** All registered global shortcuts *)
  val global_keys : global_key list

  (** Check if a key is reserved for global use *)
  val is_reserved : string -> bool

  (** Get list of reserved key strings *)
  val reserved_keys : string list

  (** Validate a page key - returns Error if reserved *)
  val validate_page_key : string -> (unit, string) result
end
```

### **2. Page Functor Signature**

```ocaml
(* miaou/miaou_core/tui_page.mli *)

module type PAGE_SIG = sig
  type state
  type msg

  val name : string

  (** Keys this page handles (validated at functor application) *)
  val handled_keys : (string * string) list

  val init : unit -> state
  val update : state -> msg -> state
  val view : state -> focus:bool -> size:LTerm_geom.size -> string
  val handle_key : state -> string -> size:LTerm_geom.size -> state
  ...
end

(** Functor type for pages *)
module type PAGE_FUNCTOR = functor (Config : GLOBAL_CONFIG) -> PAGE_SIG
```

### **3. Page Implementation as Functor**

```ocaml
(* src/ui/pages/instances.ml *)

(** Functor: takes global config, returns page module *)
module Make (Config : Miaou.Core.Global_config.GLOBAL_CONFIG) = struct
  type state = {
    services : Service_state.t list;
    filter : filter;
    selected : int;
    view_mode : view_mode;
  }

  type msg = unit

  let name = "instances"

  (** Keys this page handles *)
  let handled_keys = [
    ("c", "Create instance");
    ("f", "Filter");
    ("r", "Toggle resources");
    ("Tab", "Toggle view");
    ("s", "Start service");  (* ❌ Will fail at functor application *)
  ]

  (** Validate keys at functor application time *)
  let () =
    let conflicts =
      List.filter_map (fun (key, desc) ->
        match Config.validate_page_key key with
        | Ok () -> None
        | Error msg -> Some (key, desc, msg)
      ) handled_keys
    in
    match conflicts with
    | [] -> ()
    | conflicts ->
        failwith (
          Printf.sprintf
            "Page '%s' uses reserved keys:\n%s"
            name
            (String.concat "\n" (List.map (fun (k, d, msg) ->
              Printf.sprintf "  - '%s' (%s): %s" k d msg
            ) conflicts))
        )

  (* Rest of page implementation *)
  let init () = ...
  let update state msg = ...
  let view state ~focus ~size = ...
  let handle_key state key ~size =
    if Modal_manager.has_active () then
      (* Modal handles *)
      Modal_manager.handle_key key; state
    else
      (* Check if global key (Config provides this check) *)
      if Config.is_reserved key then
        state  (* Global handler will handle it *)
      else
        (* Page-specific handling *)
        match key with
        | "c" -> create_instance state
        | "f" -> cycle_filter state
        | "r" | "Tab" -> toggle_view_mode state
        | _ -> state
end
```

### **4. Application: Define Global Config**

```ocaml
(* src/global_config.ml *)

module Config : Miaou.Core.Global_config.GLOBAL_CONFIG = struct
  type global_handler = unit -> unit

  type global_key = {
    key : string;
    description : string;
    handler : global_handler;
  }

  let global_keys = [
    { key = "s";
      description = "Open Settings";
      handler = (fun () -> Context.navigate "settings") };

    { key = "?";
      description = "Show Help";
      handler = (fun () -> Modal_helpers.show_help ()) };

    { key = "h";
      description = "Show Help (alternate)";
      handler = (fun () -> Modal_helpers.show_help ()) };

    { key = "m";
      description = "Open Menu";
      handler = (fun () -> Modal_helpers.show_menu ()) };

    { key = "q";
      description = "Quit";
      handler = (fun () -> Context.quit_with_confirmation ()) };
  ]

  let reserved_keys =
    List.map (fun g -> g.key) global_keys

  let is_reserved key =
    List.exists (fun g -> g.key = key) global_keys

  let validate_page_key key =
    match List.find_opt (fun g -> g.key = key) global_keys with
    | None -> Ok ()
    | Some g ->
        Error (Printf.sprintf "reserved for global shortcut: %s" g.description)
end
```

### **5. Application: Instantiate Pages**

```ocaml
(* src/ui/pages.ml *)

(** Instantiate all pages with global config *)
module Instances = Instances.Make (Global_config.Config)
module Instance_details = Instance_details.Make (Global_config.Config)
module Install_node_form = Install_node_form.Make (Global_config.Config)
module Install_baker_form = Install_baker_form.Make (Global_config.Config)
module Settings = Settings.Make (Global_config.Config)
```

### **6. Application: Register Pages**

```ocaml
(* src/ui/manager_app.ml *)

let register_pages () =
  Miaou.Core.Registry.register (module Pages.Instances);
  Miaou.Core.Registry.register (module Pages.Instance_details);
  Miaou.Core.Registry.register (module Pages.Install_node_form);
  Miaou.Core.Registry.register (module Pages.Install_baker_form);
  Miaou.Core.Registry.register (module Pages.Settings)

let run () =
  register_pages ();
  (* Global handlers already in Config *)
  Driver.run_with_global_handlers
    ~global_keys:Global_config.Config.global_keys
    ~start_page:Pages.Instances.name
```

---

## What Happens at Build Time

```ocaml
(* instances.ml defines: *)
let handled_keys = [
  ("s", "Start service");  (* Oops! *)
]

(* pages.ml tries to instantiate: *)
module Instances = Instances.Make (Global_config.Config)
(*                 ^^^ Functor application happens here *)

(* At functor application, validation runs: *)
let () =
  (* This executes when functor is applied *)
  validate_keys_against_config handled_keys Config
  (* ❌ BOOM! *)

(* Build error: *)
Fatal error: exception Failure(
  "Page 'instances' uses reserved keys:
    - 's' (Start service): reserved for global shortcut: Open Settings"
)

Build failed!
```

**Validation happens at module instantiation time = early in build!**

---

## Benefits

### **1. Compile/Module-Load Time Validation**

```ocaml
(* BAD page with conflict *)
let handled_keys = [("s", "Start")]

(* Try to build: *)
$ dune build
Fatal error: Page 'instances' uses reserved keys: s
(^^ Caught at functor application, very early!)
```

### **2. Type-Safe Config Threading**

```ocaml
(* Can't instantiate page without config *)
module Instances = Instances.Make (???)
(*                                 ^^^ Must provide Config here *)

(* Config must match signature *)
module Bad_config = struct
  (* Missing required fields *)
end

module Instances = Instances.Make (Bad_config)
(* ❌ TYPE ERROR: Bad_config doesn't match GLOBAL_CONFIG *)
```

### **3. Pages Know About Globals**

```ocaml
(* Inside page functor *)
let handle_key state key ~size =
  if Config.is_reserved key then
    (* Don't handle, let global handler take it *)
    state
  else
    (* Handle page-specific key *)
    ...
```

### **4. Single Source of Truth**

```ocaml
(* Global_config.ml is THE ONLY place global keys are defined *)
let global_keys = [
  { key = "s"; description = "Settings"; handler = ... };
  (* Add/remove/change here, all pages automatically get it *)
]
```

### **5. Automatic Help Text**

```ocaml
(* Generate help from config + pages *)
let show_help page_name =
  let globals = Global_config.Config.global_keys in
  let page_keys = get_page_handled_keys page_name in

  print_endline "Global Shortcuts:";
  List.iter (fun g ->
    Printf.printf "  %s - %s\n" g.key g.description
  ) globals;

  print_endline "\nPage Shortcuts:";
  List.iter (fun (k, d) ->
    Printf.printf "  %s - %s\n" k d
  ) page_keys
```

---

## Advanced: Typed Keys via Functor

### **Config Can Provide Key Types**

```ocaml
module type GLOBAL_CONFIG = sig
  (** Phantom type for keys available to pages *)
  type page_key

  (** Create a page key if not reserved *)
  val page_key : string -> page_key option

  (** Convert page key back to string *)
  val page_key_to_string : page_key -> string

  (** Global keys (not exposed as page_key) *)
  val global_keys : (string * string * (unit -> unit)) list
end

module Config : GLOBAL_CONFIG = struct
  type page_key = string

  let reserved = ["s"; "?"; "h"; "m"; "q"]

  let page_key s =
    if List.mem s reserved then None
    else Some s

  let page_key_to_string s = s

  let global_keys = [...]
end

(* In page functor: *)
module Make (Config : GLOBAL_CONFIG) = struct
  let handled_keys =
    let open Option in
    [
      Config.page_key "c" >>= fun c -> Some (c, "Create");
      Config.page_key "s" >>= fun s -> Some (s, "Start");
      (* ^^^ Returns None, causes validation to fail *)
    ]
    |> List.filter_map (fun x -> x)

  (* Or with explicit check: *)
  let () =
    match Config.page_key "s" with
    | None -> failwith "Can't use 's' - reserved!"
    | Some k -> (* OK *)
end
```

---

## Migration Path

### **Phase 1: Keep Current API, Add Functor Wrapper**

```ocaml
(* Old page style - still works *)
module Instances_impl = struct
  let name = "instances"
  let handled_keys = [...]
  ...
end

(* New functor wrapper for validation *)
module Instances = struct
  module Make (Config : GLOBAL_CONFIG) = struct
    include Instances_impl

    (* Add validation *)
    let () = validate handled_keys Config
  end
end

(* Usage *)
module Instances = Instances.Make (Global_config.Config)
```

### **Phase 2: Full Functor Pages**

Convert pages to full functors as shown above.

---

## Implementation Checklist

### **For Miaou:**

- [ ] Add `miaou/miaou_core/global_config.mli` (signature)
- [ ] Update `PAGE_FUNCTOR` type in `tui_page.mli`
- [ ] Update driver to use global handlers from config
- [ ] Add helper for key validation in functor body
- [ ] Document functor pattern

### **For Octez-Manager:**

- [ ] Create `src/global_config.ml` with all global shortcuts
- [ ] Convert pages to functors:
  - [ ] `instances.ml` → `Instances.Make (Config)`
  - [ ] `instance_details.ml`
  - [ ] `install_node_form.ml`
  - [ ] `install_baker_form.ml`
  - [ ] `settings.ml`
- [ ] Create `src/ui/pages.ml` to instantiate all pages
- [ ] Update `manager_app.ml` to use instantiated pages
- [ ] Test that conflicts cause build failures

---

## Example: Complete Flow

### **1. Define Global Config**

```ocaml
(* src/global_config.ml *)
module Config = struct
  let global_keys = [
    { key = "s"; description = "Settings"; handler = navigate_settings };
    { key = "?"; description = "Help"; handler = show_help };
  ]

  let is_reserved key =
    List.exists (fun g -> g.key = key) global_keys

  let validate_page_key key =
    if is_reserved key then
      Error (sprintf "'%s' is reserved" key)
    else
      Ok ()
end
```

### **2. Write Page as Functor**

```ocaml
(* src/ui/pages/instances.ml *)
module Make (Config : GLOBAL_CONFIG) = struct
  let name = "instances"

  let handled_keys = [
    ("c", "Create");
    ("s", "Start");  (* BAD! *)
  ]

  (* Validation at functor application *)
  let () =
    List.iter (fun (k, _) ->
      match Config.validate_page_key k with
      | Ok () -> ()
      | Error msg -> failwith msg
    ) handled_keys

  (* Rest of implementation *)
  let init () = ...
  let handle_key state key ~size = ...
end
```

### **3. Instantiate Page**

```ocaml
(* src/ui/pages.ml *)
module Instances = Instances.Make (Global_config.Config)
(*                                  ^^^ Validation happens HERE *)
```

### **4. Build Fails Early**

```bash
$ dune build
File "src/ui/pages.ml", line 1:
Fatal error: exception Failure("'s' is reserved")

Build failed!
```

### **5. Fix the Page**

```ocaml
let handled_keys = [
  ("c", "Create");
  ("t", "Start");  (* Changed to 't' *)
]
```

### **6. Build Succeeds!**

```bash
$ dune build
...
Build succeeded!
```

---

## Comparison: Functor vs Other Approaches

| Approach | When Caught | Boilerplate | Safety | OCaml-idiomatic |
|----------|-------------|-------------|--------|-----------------|
| **Functor-based** | Module load | Medium | ⭐⭐⭐⭐⭐ | ⭐⭐⭐⭐⭐ |
| Lifecycle state | First runtime call | Low | ⭐⭐⭐⭐ | ⭐⭐⭐ |
| Builder pattern | Compile time | High | ⭐⭐⭐⭐⭐ | ⭐⭐⭐⭐ |
| GADT types | Compile time | High | ⭐⭐⭐⭐⭐ | ⭐⭐⭐ |

**Functor approach:**
- ✅ Very OCaml-idiomatic
- ✅ Module-load time validation (early!)
- ✅ Type-safe config threading
- ✅ Single source of truth
- ✅ No runtime overhead
- 😐 Medium boilerplate (functor application)
- ❌ Requires restructuring pages as functors

---

## Recommendation

### **Use Functor Approach If:**

1. ✅ You're comfortable with functors
2. ✅ You want early validation (module-load time)
3. ✅ You want config threaded through pages
4. ✅ You want very OCaml-idiomatic code
5. ✅ You're okay with restructuring pages

### **Use Lifecycle State If:**

1. ✅ You want simplest implementation
2. ✅ You want minimal changes to existing code
3. ✅ Runtime validation is good enough
4. ❌ You're not comfortable with functors

---

## My Take

**For octez-manager:** The **functor approach is perfect** because:

1. ✅ Small number of pages (5-6) - easy to convert
2. ✅ You're already using OCaml advanced features
3. ✅ Early validation at module-load time
4. ✅ Clean, type-safe config threading
5. ✅ Very maintainable

**For a larger app or library:** Lifecycle state might be simpler.

---

**Document Version:** 1.0
**Last Updated:** 2025-12-10
**Status:** Design Complete
