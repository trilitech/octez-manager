# Enforcing Top-Level Only API Calls

**Date:** 2025-12-10
**Context:** How to ensure `Global_keys.register` is only called during initialization, not at runtime

---

## The Problem

We want to **prevent** this:

```ocaml
(* ❌ BAD - Dynamic registration during runtime *)
let handle_some_action () =
  Global_keys.register [("x", "New key", ...)];
  (* Keys shouldn't change after app starts! *)
```

We want to **enforce** this:

```ocaml
(* ✅ GOOD - Top-level registration *)
let () =
  Global_keys.register [("s", "Settings", ...)];
  register_pages ();
  App.run ()
```

---

## Solution 1: Lifecycle State (Runtime Check) ⭐⭐⭐⭐

### **Concept**

Track app lifecycle, fail if called after initialization:

```ocaml
(* miaou/miaou_core/global_keys.ml *)

type lifecycle =
  | Initializing  (** Before app starts *)
  | Running       (** Event loop running *)

let lifecycle = ref Initializing

let register keys =
  match !lifecycle with
  | Initializing ->
      registry := keys @ !registry;
      Ok ()
  | Running ->
      Error "Cannot register global keys after application has started. \
             Global_keys.register must be called at module initialization."

let mark_running () =
  lifecycle := Running

(** Convenience: register or fail *)
let register_exn keys =
  match register keys with
  | Ok () -> ()
  | Error msg -> failwith msg
```

### **Driver Integration**

```ocaml
(* miaou/miaou_driver_term/lambda_term_driver.ml *)

let run page =
  (* Mark that initialization is done *)
  Global_keys.mark_running ();

  (* Now start event loop *)
  event_loop page
```

### **Usage**

```ocaml
(* main.ml - TOP LEVEL *)
let () =
  Global_keys.register_exn [("s", "Settings", ...)];
  (* ✅ OK - called during initialization *)

  register_pages ();
  Manager_app.run ()  (* Marks running, blocks register *)

(* Somewhere in a handler - RUNTIME *)
let handle_action () =
  Global_keys.register_exn [("x", "X", ...)];
  (* ❌ Exception: Cannot register global keys after application has started *)
```

### **Error Message**

```ocaml
Fatal error: exception Failure(
  "Cannot register global keys after application has started.
   Global_keys.register must be called at module initialization."
)
```

**Pros:**
- ✅ Simple implementation
- ✅ Clear error message
- ✅ Catches violations immediately

**Cons:**
- ❌ Runtime check, not compile-time
- ❌ Can be bypassed by resetting state

---

## Solution 2: Single-Shot Registration ⭐⭐⭐⭐⭐

### **Concept**

Only allow registration once, at the very beginning:

```ocaml
(* miaou/miaou_core/global_keys.ml *)

type registration_state =
  | NotRegistered
  | Registered of registration list

let state = ref NotRegistered

let register keys =
  match !state with
  | NotRegistered ->
      state := Registered keys;
      Ok ()
  | Registered _ ->
      Error "Global keys already registered. \
             Global_keys.register can only be called once, \
             at module initialization."

let register_exn keys =
  match register keys with
  | Ok () -> ()
  | Error msg -> failwith msg

let get_all () =
  match !state with
  | NotRegistered -> []
  | Registered keys -> keys
```

### **Usage**

```ocaml
(* main.ml *)
let () =
  Global_keys.register_exn [
    ("s", "Settings", ...);
    ("?", "Help", ...);
  ];  (* ✅ First call, OK *)

  Global_keys.register_exn [
    ("m", "Menu", ...);
  ];  (* ❌ Exception: already registered *)
```

**Forces you to do all registration in one place!**

**Pros:**
- ✅ Forces single point of registration
- ✅ Can't accidentally re-register
- ✅ Simple to implement

**Cons:**
- ❌ Inflexible if you want staged registration
- ❌ Still runtime check

---

## Solution 3: Builder Pattern with Type States ⭐⭐⭐⭐⭐

### **Concept**

Use phantom types to enforce initialization order at compile time:

```ocaml
(* miaou/miaou_core/app.ml *)

(** Phantom type for initialization phase *)
type init_phase

(** Phantom type for running phase *)
type run_phase

(** Application state parameterized by phase *)
type 'phase t = {
  global_keys : (string * string * (unit -> unit)) list;
  pages : page list;
}

(** Create new app in init phase *)
let create () : init_phase t = {
  global_keys = [];
  pages = [];
}

(** Register global keys (only in init phase) *)
let with_global_keys (app : init_phase t) keys : init_phase t =
  { app with global_keys = keys }

(** Register a page (only in init phase) *)
let with_page (app : init_phase t) page : init_phase t =
  (* Validate page keys against globals *)
  validate_page app.global_keys page;
  { app with pages = page :: app.pages }

(** Start the app (consumes init_phase, returns run_phase) *)
let run (app : init_phase t) : run_phase t =
  (* Initialize global keys *)
  Global_keys.internal_set app.global_keys;
  Global_keys.mark_running ();

  (* Run event loop *)
  Driver.run app.pages;

  (* Can't return the same app - phase changed! *)
  { global_keys = app.global_keys; pages = app.pages }
```

### **Usage**

```ocaml
(* main.ml *)
let () =
  Miaou.App.create ()
  |> Miaou.App.with_global_keys [
       ("s", "Settings", fun () -> navigate "settings");
       ("?", "Help", fun () -> show_help ());
     ]
  |> Miaou.App.with_page (module Instances)
  |> Miaou.App.with_page (module Settings)
  |> Miaou.App.run
  |> ignore  (* Returns run_phase t, can't add more keys *)

(* Try to add keys after run: *)
let () =
  let app = Miaou.App.create () |> Miaou.App.run in
  app |> Miaou.App.with_global_keys [...]
  (* ❌ TYPE ERROR: with_global_keys expects init_phase t,
                    but app is run_phase t *)
```

**Compile-time enforcement!**

**Pros:**
- ✅ **Compile-time safety** - Can't call after run
- ✅ Enforces initialization order
- ✅ Self-documenting API
- ✅ Type-driven development

**Cons:**
- ❌ More complex API
- ❌ Requires changing app structure

---

## Solution 4: Capability-Based (Most Robust) ⭐⭐⭐⭐⭐

### **Concept**

Require a "capability" token that's only available during initialization:

```ocaml
(* miaou/miaou_core/init_capability.ml *)

(** Capability proving we're in initialization phase *)
type t = private Init_cap

let cap : t option ref = ref None

(** Create capability - can only be called once *)
let create () : t =
  match !cap with
  | Some c -> c
  | None ->
      let c = (Obj.magic () : t) in
      cap := Some c;
      c

(** Destroy capability - marks end of init phase *)
let destroy () =
  cap := None
```

### **Global Keys with Capability**

```ocaml
(* miaou/miaou_core/global_keys.ml *)

let register (cap : Init_capability.t) keys =
  (* Capability proves we're in init phase *)
  registry := keys @ !registry

(** No-capability version that fails *)
let register_exn keys =
  failwith "Global_keys.register requires an Init_capability. \
           Use App.with_global_keys instead."
```

### **App Builder with Capability**

```ocaml
(* miaou/miaou_core/app.ml *)

let create () =
  let cap = Init_capability.create () in
  (cap, { pages = [] })

let with_global_keys (cap, app) keys =
  Global_keys.register cap keys;
  (cap, app)

let run (cap, app) =
  (* Destroy capability - no more registration possible *)
  Init_capability.destroy ();

  (* Run app *)
  Driver.run app.pages
```

### **Usage**

```ocaml
(* main.ml *)
let () =
  Miaou.App.create ()
  |> Miaou.App.with_global_keys [("s", "Settings", ...)]
  (* ^^^ This has the capability, can register *)
  |> Miaou.App.run
  (* ^^^ Capability destroyed here *)

(* Later, in a handler: *)
let handle_action () =
  Global_keys.register ??? [...]
  (* ❌ No capability available! Can't call this function *)
```

**Pros:**
- ✅ Capability proves initialization phase
- ✅ Can't be bypassed
- ✅ Type-safe

**Cons:**
- ❌ Complex implementation
- ❌ Capability needs to be threaded

---

## Solution 5: Module Sealing (OCaml-specific) ⭐⭐⭐

### **Concept**

Hide the registration function after initialization:

```ocaml
(* miaou/miaou_core/global_keys.ml *)

module type REGISTRATION = sig
  val register : (string * string * (unit -> unit)) list -> unit
end

module type RUNTIME = sig
  val handle : string -> bool
  val all : unit -> registration list
end

module Registration : REGISTRATION = struct
  let register keys =
    registry := keys @ !registry
end

module Runtime : RUNTIME = struct
  let handle key = ...
  let all () = !registry
end
```

### **Staged Exposure**

```ocaml
(* miaou/miaou_core/miaou.ml *)

module Init = struct
  module Global_keys = Global_keys.Registration
end

module Runtime = struct
  module Global_keys = Global_keys.Runtime
end
```

### **Usage**

```ocaml
(* At initialization - use Init module *)
let () =
  Miaou.Init.Global_keys.register [("s", "Settings", ...)];

  (* After this point, only use Miaou.Runtime *)
  Manager_app.run ()

(* In handlers - only Runtime available *)
let handle_action () =
  Miaou.Runtime.Global_keys.handle "s"
  (* Registration not available in this module *)
```

**Pros:**
- ✅ Clear separation of concerns
- ✅ Can't access registration at runtime

**Cons:**
- ❌ Easy to accidentally use wrong module
- ❌ Not enforced by types

---

## Recommended: Combination Approach

### **Best Practice: Lifecycle + Builder**

```ocaml
(* miaou/miaou_core/app.ml *)

type init_phase
type run_phase

type 'phase t = {
  global_keys : (string * string * (unit -> unit)) list;
  pages : page list;
}

let create () : init_phase t =
  { global_keys = []; pages = [] }

let with_global_keys (app : init_phase t) keys : init_phase t =
  (* Lifecycle check for safety belt *)
  if Global_keys.is_running () then
    failwith "Cannot add global keys after app started"
  else
    { app with global_keys = keys }

let with_page app page =
  if Global_keys.is_running () then
    failwith "Cannot register pages after app started"
  else
    validate_page app.global_keys page;
    { app with pages = page :: app.pages }

let run (app : init_phase t) : unit =
  (* Initialize *)
  Global_keys.internal_set app.global_keys;
  Global_keys.mark_running ();

  (* Run event loop (blocks) *)
  Driver.run app.pages
```

**Layered Protection:**
1. **Type system** - `init_phase` vs `run_phase`
2. **Lifecycle state** - Runtime check as safety belt
3. **Builder pattern** - Guided API usage

---

## Comparison Table

| Approach | Enforcement | Ease of Use | Safety | Effort |
|----------|-------------|-------------|--------|--------|
| **Lifecycle State** | Runtime | ⭐⭐⭐⭐⭐ | ⭐⭐⭐ | Low |
| **Single-Shot** | Runtime | ⭐⭐⭐⭐ | ⭐⭐⭐⭐ | Low |
| **Builder + Types** | Compile-time | ⭐⭐⭐⭐ | ⭐⭐⭐⭐⭐ | Medium |
| **Capability** | Compile-time | ⭐⭐⭐ | ⭐⭐⭐⭐⭐ | High |
| **Module Sealing** | Convention | ⭐⭐⭐ | ⭐⭐ | Low |
| **Combo (Recommended)** | Both | ⭐⭐⭐⭐ | ⭐⭐⭐⭐⭐ | Medium |

---

## Recommended Implementation

### **Phase 1: Lifecycle Check (Immediate)**

```ocaml
(* miaou/miaou_core/global_keys.ml *)

type lifecycle = Initializing | Running

let lifecycle = ref Initializing

let register keys =
  match !lifecycle with
  | Initializing ->
      registry := keys @ !registry
  | Running ->
      failwith "Cannot register global keys after app started. \
                Call Global_keys.register at module initialization only."

let mark_running () =
  lifecycle := Running
```

**Quick to implement, catches most mistakes immediately.**

### **Phase 2: Builder Pattern (Future Enhancement)**

```ocaml
(* miaou/miaou_core/app.ml *)

type 'phase t

val create : unit -> init_phase t
val with_global_keys : init_phase t -> keys -> init_phase t
val run : init_phase t -> unit
```

**Type-safe, prevents misuse at compile time.**

---

## Error Messages

### **Good Error Message**

```ocaml
let register keys =
  match !lifecycle with
  | Initializing -> ...
  | Running ->
      failwith (
        "Global_keys.register called after application started.\n\n\
         Global keys must be registered during module initialization, \
         before the event loop starts.\n\n\
         Correct usage:\n\
         \  let () =\n\
         \    Global_keys.register [(\"s\", \"Settings\", ...)];\n\
         \    Manager_app.run ()\n\n\
         Do not call Global_keys.register from:\n\
         - Event handlers\n\
         - Button callbacks\n\
         - Modal callbacks\n\
         - Any function called after app startup\n"
      )
```

---

## Testing

```ocaml
(* test/test_global_keys.ml *)

let test_cannot_register_after_run () =
  let app = Miaou.App.create ()
    |> Miaou.App.with_global_keys [("s", "Settings", fun () -> ())]
  in

  (* Spawn thread that runs app *)
  Thread.create Miaou.App.run app;
  Thread.delay 0.1;  (* Let it start *)

  (* Try to register after start *)
  try
    Global_keys.register [("x", "X", fun () -> ())];
    assert false  (* Should have failed *)
  with Failure msg ->
    assert (String.contains msg "after application started")
```

---

## Summary

**Recommended Approach:**

1. ✅ **Start with lifecycle state check** (runtime)
   - Quick to implement (~30 lines)
   - Catches violations immediately
   - Clear error messages

2. ✅ **Add builder pattern later** (compile-time)
   - Prevents misuse at type level
   - Better developer experience
   - More refactoring work

**Enforcement levels:**
- 🟡 Runtime: Lifecycle state (good enough for most cases)
- 🟢 Compile-time: Builder with phantom types (ideal)
- 🔵 Both: Layered defense (recommended)

**Implementation priority:**
1. Lifecycle state - **Do this first**
2. Builder pattern - **Do this when refactoring app structure**
3. Capability system - **Only if you need maximum safety**

---

**Document Version:** 1.0
**Last Updated:** 2025-12-10
**Status:** Design Complete
