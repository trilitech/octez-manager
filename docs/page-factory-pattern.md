# Page Factory Pattern: Single Config, Multiple Pages

**Date:** 2025-12-10
**Context:** Registry feeds config to page functors, preventing multiple instantiations

---

## The Problem with Direct Functor Application

### **Current Design Flaw:**

```ocaml
(* User can do this - BAD! *)
module Instances1 = Instances.Make (Config1)
module Instances2 = Instances.Make (Config2)
(* Now you have the same page with TWO different configs! *)

(* Register both *)
Registry.register (module Instances1)
Registry.register (module Instances2)
(* Which config wins? Chaos! *)
```

**We want ONE config for ALL pages.**

---

## Solution: Page Factory Pattern

### **Concept:**

1. Pages register **factories** (the functor itself), not instances
2. Registry holds the **single global config**
3. Registry **instantiates** pages when registering
4. Users can't create multiple instances with different configs

---

## Design

### **1. Global Config Signature**

```ocaml
(* miaou/miaou_core/global_config.mli *)

module type GLOBAL_CONFIG = sig
  type global_handler = unit -> unit

  type global_key = {
    key : string;
    description : string;
    handler : global_handler;
  }

  val global_keys : global_key list
  val is_reserved : string -> bool
  val validate_page_key : string -> (unit, string) result
end
```

### **2. Page Factory Signature**

```ocaml
(* miaou/miaou_core/tui_page.mli *)

(** Standard page module type *)
module type PAGE_SIG = sig
  type state
  type msg

  val name : string
  val handled_keys : (string * string) list

  val init : unit -> state
  val update : state -> msg -> state
  val view : state -> focus:bool -> size:LTerm_geom.size -> string
  val handle_key : state -> string -> size:LTerm_geom.size -> state
  ...
end

(** Page factory: functor that takes config, returns page *)
module type PAGE_FACTORY = functor (Config : Global_config.GLOBAL_CONFIG) -> PAGE_SIG
```

### **3. Enhanced Registry**

```ocaml
(* miaou/miaou_core/registry.mli *)

(** Set the global config - must be called ONCE before any page registration *)
val set_global_config : (module Global_config.GLOBAL_CONFIG) -> unit

(** Register a page factory.
    The registry will instantiate the page with the global config.

    Fails if:
    - Global config not set yet
    - Page keys conflict with global keys
*)
val register_factory : (module PAGE_FACTORY) -> unit

(** Get a registered page by name *)
val find : string -> (module PAGE_SIG) option
```

### **4. Registry Implementation**

```ocaml
(* miaou/miaou_core/registry.ml *)

type state =
  | No_config
  | Config_set of (module Global_config.GLOBAL_CONFIG)
  | Locked  (* After app starts, no more registration *)

let state = ref No_config

let global_config () =
  match !state with
  | Config_set config -> Some config
  | _ -> None

let set_global_config config =
  match !state with
  | No_config ->
      state := Config_set config
  | Config_set _ ->
      failwith "Global config already set. Can only be set once."
  | Locked ->
      failwith "Cannot set config after app started."

let pages : (string, (module PAGE_SIG)) Hashtbl.t = Hashtbl.create 17

let register_factory (type a) (module Factory : PAGE_FACTORY) =
  match !state with
  | No_config ->
      failwith "Global config not set. Call Registry.set_global_config first."
  | Locked ->
      failwith "Cannot register pages after app started."
  | Config_set config ->
      (* Instantiate page with global config *)
      let module Config = (val config : Global_config.GLOBAL_CONFIG) in
      let module Page = Factory (Config) in

      (* Validate keys (already done in functor, but double-check) *)
      let conflicts =
        List.filter_map (fun (key, desc) ->
          match Config.validate_page_key key with
          | Ok () -> None
          | Error msg -> Some (key, desc, msg)
        ) Page.handled_keys
      in

      (match conflicts with
      | [] -> ()
      | conflicts ->
          failwith (
            Printf.sprintf
              "Page '%s' conflicts with global keys:\n%s"
              Page.name
              (String.concat "\n" (List.map (fun (k, d, msg) ->
                Printf.sprintf "  - '%s' (%s): %s" k d msg
              ) conflicts))
          ));

      (* Register instantiated page *)
      if Hashtbl.mem pages Page.name then
        failwith (Printf.sprintf "Page '%s' already registered" Page.name);

      Hashtbl.add pages Page.name (module Page : PAGE_SIG)

let lock () =
  state := Locked

let find name =
  Hashtbl.find_opt pages name
```

---

## Usage Pattern

### **1. Define Global Config (Once)**

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

    { key = "m";
      description = "Open Menu";
      handler = (fun () -> Modal_helpers.show_menu ()) };
  ]

  let is_reserved key =
    List.exists (fun g -> g.key = key) global_keys

  let validate_page_key key =
    match List.find_opt (fun g -> g.key = key) global_keys with
    | None -> Ok ()
    | Some g ->
        Error (Printf.sprintf "reserved for: %s" g.description)
end
```

### **2. Write Pages as Functors**

```ocaml
(* src/ui/pages/instances.ml *)

(** Page functor - takes config, returns page *)
module Make (Config : Miaou.Core.Global_config.GLOBAL_CONFIG) : Miaou.Core.Tui_page.PAGE_SIG = struct
  type state = {
    services : Service_state.t list;
    filter : filter;
    selected : int;
  }

  type msg = unit

  let name = "instances"

  let handled_keys = [
    ("c", "Create instance");
    ("f", "Filter");
    ("r", "Toggle resources");
    ("Tab", "Toggle view");
  ]

  (* Validation at functor application *)
  let () =
    List.iter (fun (key, _) ->
      match Config.validate_page_key key with
      | Ok () -> ()
      | Error msg ->
          failwith (Printf.sprintf "Page '%s' uses reserved key '%s': %s"
                     name key msg)
    ) handled_keys

  let init () = ...

  let handle_key state key ~size =
    if Modal_manager.has_active () then
      Modal_manager.handle_key key; state
    else if Config.is_reserved key then
      (* Global key, don't handle *)
      state
    else
      (* Page-specific handling *)
      match key with
      | "c" -> create_instance state
      | "f" -> cycle_filter state
      | "r" | "Tab" -> toggle_view_mode state
      | _ -> state

  let update state msg = state
  let view state ~focus ~size = ...
end
```

### **3. Application Startup**

```ocaml
(* src/main.ml *)

let register_pages () =
  (* Register page FACTORIES, not instances *)
  Miaou.Core.Registry.register_factory (module Instances.Make);
  Miaou.Core.Registry.register_factory (module Instance_details.Make);
  Miaou.Core.Registry.register_factory (module Install_node_form.Make);
  Miaou.Core.Registry.register_factory (module Install_baker_form.Make);
  Miaou.Core.Registry.register_factory (module Settings.Make)

let run () =
  (* 1. Set global config FIRST *)
  Miaou.Core.Registry.set_global_config (module Global_config.Config);

  (* 2. Register pages (they'll be instantiated with config) *)
  register_pages ();

  (* 3. Lock registry and run *)
  Miaou.Core.Registry.lock ();
  Manager_app.run ()
```

---

## What This Prevents

### **❌ Can't Create Multiple Instances:**

```ocaml
(* This is now IMPOSSIBLE - pages aren't exposed as functors to users *)

(* User can't do this anymore: *)
module Instances1 = Instances.Make (Config1)  (* ❌ Instances.Make is private *)

(* They can only do this: *)
Registry.register_factory (module Instances.Make)
(* ^^^ Registry controls instantiation with THE global config *)
```

### **❌ Can't Register Without Config:**

```ocaml
(* Try to register without setting config first *)
Registry.register_factory (module Instances.Make)
(* ❌ Exception: Global config not set *)
```

### **❌ Can't Change Config After Set:**

```ocaml
Registry.set_global_config (module Config1)
Registry.set_global_config (module Config2)
(* ❌ Exception: Global config already set *)
```

### **❌ Can't Register After App Starts:**

```ocaml
Registry.set_global_config (module Config);
register_pages ();
Manager_app.run ();  (* Calls Registry.lock() *)

(* Later, try to register another page: *)
Registry.register_factory (module NewPage.Make)
(* ❌ Exception: Cannot register pages after app started *)
```

---

## Lifecycle States

```
           set_global_config()
No_config ───────────────────> Config_set
                                    │
                                    │ register_factory() (multiple times OK)
                                    │
                                    v
                                Config_set
                                    │
                                    │ lock()
                                    v
                                 Locked
                              (no more changes)
```

---

## Error Messages

### **Forgot to Set Config**

```ocaml
Registry.register_factory (module Instances.Make)

(* Error: *)
Fatal error: exception Failure(
  "Global config not set.
   Call Registry.set_global_config before registering pages.

   Example:
     Registry.set_global_config (module Global_config.Config);
     Registry.register_factory (module Instances.Make);"
)
```

### **Config Set Twice**

```ocaml
Registry.set_global_config (module Config1);
Registry.set_global_config (module Config2);

(* Error: *)
Fatal error: exception Failure(
  "Global config already set. Can only be set once.
   Config should be defined at application startup."
)
```

### **Page Key Conflict**

```ocaml
(* instances.ml has: *)
let handled_keys = [("s", "Start service")]

(* At registration: *)
Registry.register_factory (module Instances.Make)

(* Error: *)
Fatal error: exception Failure(
  "Page 'instances' conflicts with global keys:
    - 's' (Start service): reserved for: Open Settings

  Available keys: a, b, c, d, e, f, g, i, j, k, l, n, o, p, r, t, u, v, w, x, y, z"
)
```

---

## Benefits

### **1. Single Config Enforcement**

```ocaml
(* ONE config for entire app *)
Registry.set_global_config (module Config);

(* All pages get THIS config automatically *)
Registry.register_factory (module Page1.Make);  (* Uses Config *)
Registry.register_factory (module Page2.Make);  (* Uses Config *)
Registry.register_factory (module Page3.Make);  (* Uses Config *)
```

### **2. Users Can't Misuse**

```ocaml
(* Page functors are not exposed to users *)
(* They can only register factories via Registry *)
(* Registry controls instantiation *)
```

### **3. Clear API**

```ocaml
(* Application startup - very clear order *)
let () =
  Registry.set_global_config (module Config);   (* 1. Config *)
  register_pages ();                            (* 2. Pages *)
  Manager_app.run ()                            (* 3. Run *)
```

### **4. Validation at Registration**

```ocaml
(* Conflicts caught when registering, not at random runtime points *)
Registry.register_factory (module BadPage.Make)
(* ^^^ Fails HERE if keys conflict *)
```

---

## Implementation Details

### **Registry Stores Config Internally**

```ocaml
(* miaou/miaou_core/registry.ml *)

let state = ref No_config

let set_global_config config =
  state := Config_set config

let register_factory (module Factory : PAGE_FACTORY) =
  match !state with
  | Config_set config ->
      (* Extract the config module *)
      let module Config = (val config : Global_config.GLOBAL_CONFIG) in

      (* Apply functor with config *)
      let module Page = Factory (Config) in

      (* Store instantiated page *)
      Hashtbl.add pages Page.name (module Page : PAGE_SIG)
  | _ -> failwith "..."
```

### **Driver Uses Global Config**

```ocaml
(* miaou/miaou_driver_term/lambda_term_driver.ml *)

let run start_page =
  (* Get global config from registry *)
  match Registry.global_config () with
  | None -> failwith "No global config set"
  | Some config ->
      let module Config = (val config : Global_config.GLOBAL_CONFIG) in

      (* Event loop with global key handling *)
      let rec event_loop page =
        ...
        let key = parse_event event in

        (* Check global keys first *)
        if not (Modal_manager.has_active ()) then
          match List.find_opt (fun g -> g.Config.key = key) Config.global_keys with
          | Some global ->
              global.handler ();  (* Handle global key *)
              event_loop page
          | None ->
              (* Not global, pass to page *)
              let page' = page.handle_key page.state key ~size in
              event_loop { page with state = page' }
        else
          (* Modal active *)
          Modal_manager.handle_key key;
          event_loop page
      in
      event_loop (Registry.find start_page |> Option.get)
```

---

## Complete Example

### **File Structure**

```
src/
  global_config.ml          (defines Config module)
  main.ml                   (sets config, registers pages, runs)
  ui/
    pages/
      instances.ml          (module Make (Config) = struct ... end)
      instance_details.ml   (module Make (Config) = struct ... end)
      install_node_form.ml  (module Make (Config) = struct ... end)
      settings.ml           (module Make (Config) = struct ... end)
    manager_app.ml          (calls Registry functions)
```

### **global_config.ml**

```ocaml
module Config : Miaou.Core.Global_config.GLOBAL_CONFIG = struct
  type global_handler = unit -> unit
  type global_key = { key : string; description : string; handler : global_handler }

  let global_keys = [
    { key = "s"; description = "Settings"; handler = navigate_settings };
    { key = "?"; description = "Help"; handler = show_help };
    { key = "m"; description = "Menu"; handler = show_menu };
  ]

  let is_reserved key =
    List.exists (fun g -> g.key = key) global_keys

  let validate_page_key key =
    if is_reserved key then
      Error (sprintf "Reserved by global shortcuts")
    else Ok ()
end
```

### **main.ml**

```ocaml
let register_pages () =
  let open Miaou.Core.Registry in
  register_factory (module Instances.Make);
  register_factory (module Instance_details.Make);
  register_factory (module Install_node_form.Make);
  register_factory (module Settings.Make)

let () =
  (* 1. Set config *)
  Miaou.Core.Registry.set_global_config (module Global_config.Config);

  (* 2. Register page factories *)
  register_pages ();

  (* 3. Lock and run *)
  Miaou.Core.Registry.lock ();
  Manager_app.run ()
```

### **instances.ml**

```ocaml
module Make (Config : Miaou.Core.Global_config.GLOBAL_CONFIG) = struct
  let name = "instances"

  let handled_keys = [
    ("c", "Create");
    ("f", "Filter");
    ("Tab", "Toggle view");
  ]

  (* Validation happens at functor application = registration time *)
  let () =
    List.iter (fun (k, _) ->
      match Config.validate_page_key k with
      | Error e -> failwith (sprintf "Key '%s': %s" k e)
      | Ok () -> ()
    ) handled_keys

  let init () = ...
  let handle_key state key ~size =
    if Config.is_reserved key then state
    else match key with | "c" -> create state | ...
  ...
end
```

---

## Summary

### **Pattern:**

1. ✅ **One config** - Set once at startup
2. ✅ **Registry owns config** - Stored internally
3. ✅ **Pages are factories** - Functors registered, not instances
4. ✅ **Registry instantiates** - Applies functor with config
5. ✅ **Validation at registration** - Conflicts caught early
6. ✅ **Users can't misuse** - No direct functor application

### **API:**

```ocaml
(* Setup *)
Registry.set_global_config (module Config);

(* Register factories *)
Registry.register_factory (module Page.Make);

(* Lock and run *)
Registry.lock ();
App.run ()
```

### **Guarantees:**

- ✅ Only ONE config for entire app
- ✅ Validated at registration time
- ✅ Can't change config after set
- ✅ Can't register after app starts
- ✅ Clear, linear initialization order

---

**Document Version:** 1.0
**Last Updated:** 2025-12-10
**Status:** Final Design - Ready for Implementation
