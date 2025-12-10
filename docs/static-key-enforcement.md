# Static Key Enforcement: Preventing Reserved Key Conflicts at Compile Time

**Date:** 2025-12-10
**Context:** Design options for compile-time prevention of global shortcut conflicts

---

## The Problem

**Current situation (runtime checking):**
```ocaml
(* Global_shortcuts.ml *)
let handle key =
  match key with
  | "s" -> ... (* Reserved *)
  | "?" -> ... (* Reserved *)
  | _ -> NotGlobal

(* my_page.ml - OOPS! Conflicts with global "s" *)
let handle_key state key ~size =
  match key with
  | "s" -> start_service state  (* Runtime conflict! *)
  | ...
```

**We want:** Compiler error if a page tries to use a reserved key.

---

## Option 1: Variant-Based Keys (Recommended) ⭐⭐⭐⭐⭐

### **Concept**

Replace string keys with typed variants:

```ocaml
(* miaou/miaou_core/keys.ml - ENHANCED *)

(** Key classification *)
type key_category =
  | Global      (** Reserved for application-wide shortcuts *)
  | Navigation  (** Common navigation (arrows, enter, esc) *)
  | Available   (** Available for page-specific use *)

(** All possible keys *)
type t =
  (* Global shortcuts - RESERVED *)
  | Settings      (* 's' *)
  | Help          (* '?' or 'h' *)
  | Menu          (* 'm' *)
  | Quit          (* 'q' *)

  (* Navigation - RESERVED *)
  | Up | Down | Left | Right
  | Enter | Escape
  | PageUp | PageDown
  | Home | End
  | Tab | ShiftTab

  (* Available for pages *)
  | Char of char  (* a-z except reserved *)
  | Digit of int  (* 0-9 *)
  | Ctrl of char  (* Ctrl+a, Ctrl+b, ... *)
  | Alt of char   (* Alt+a, Alt+b, ... *)
  | F of int      (* F1-F12 *)
  | Other of string  (* Fallback for special keys *)

(** Get the category of a key *)
let category = function
  | Settings | Help | Menu | Quit -> Global
  | Up | Down | Left | Right | Enter | Escape
  | PageUp | PageDown | Home | End | Tab | ShiftTab -> Navigation
  | Char _ | Digit _ | Ctrl _ | Alt _ | F _ | Other _ -> Available

(** Parse string to key *)
let of_string = function
  | "s" -> Settings
  | "?" | "h" -> Help
  | "m" -> Menu
  | "q" -> Quit
  | "Up" -> Up
  | "Down" -> Down
  | "Enter" -> Enter
  | "Esc" | "Escape" -> Escape
  | "Tab" -> Tab
  | s when String.length s = 1 -> Char s.[0]
  | s -> Other s

(** Convert key to string (for display) *)
let to_string = function
  | Settings -> "s"
  | Help -> "?"
  | Menu -> "m"
  | Quit -> "q"
  | Up -> "↑"
  | Down -> "↓"
  | Enter -> "↵"
  | Escape -> "Esc"
  | Tab -> "Tab"
  | Char c -> String.make 1 c
  | Digit d -> string_of_int d
  | Ctrl c -> Printf.sprintf "^%c" c
  | Alt c -> Printf.sprintf "Alt+%c" c
  | F n -> Printf.sprintf "F%d" n
  | Other s -> s
```

### **Page Handler Type**

```ocaml
(* miaou/miaou_core/tui_page.mli - ENHANCED *)

module type PAGE_SIG = sig
  type state
  type msg

  val name : string
  val init : unit -> state
  val update : state -> msg -> state
  val view : state -> focus:bool -> size:LTerm_geom.size -> string

  (** NEW: Typed key handler *)
  val handle_key : state -> Keys.t -> size:LTerm_geom.size -> state

  (** NEW: Declare which keys this page handles (compile-time registry) *)
  val handled_keys : Keys.t list
end
```

### **Global Shortcuts Module**

```ocaml
(* src/ui/global_shortcuts.ml - ENHANCED *)

open Miaou.Core.Keys

(** Keys reserved for global use *)
let reserved_keys = [Settings; Help; Menu; Quit]

(** Check if a key is reserved *)
let is_reserved key = List.mem key reserved_keys

type outcome = Handled | NotGlobal

let handle key =
  match key with
  | Settings -> Context.navigate "settings"; Handled
  | Help -> Modal_helpers.show_help_modal (); Handled
  | Menu -> Modal_helpers.show_menu_modal (); Handled
  | Quit -> (* confirmation modal *) Handled
  | _ -> NotGlobal
```

### **Page Implementation**

```ocaml
(* src/ui/pages/instances.ml - ENHANCED *)

open Miaou.Core.Keys

(** COMPILE-TIME DECLARATION: Keys this page uses *)
let handled_keys = [
  Char 'c';     (* create instance *)
  Char 'f';     (* filter *)
  Char 'r';     (* toggle resources *)
  Tab;          (* toggle view mode *)
  Up; Down;     (* navigation *)
  Enter;        (* actions *)
  Escape;       (* back *)
]

(** Key handler with typed keys *)
let handle_key state key ~size =
  if Modal_manager.has_active () then (
    Modal_manager.handle_key key;
    state
  ) else
    match Global_shortcuts.handle key with
    | Handled -> state
    | NotGlobal ->
        match key with
        | Char 'c' -> create_instance state
        | Char 'f' -> cycle_filter state
        | Char 'r' | Tab -> toggle_view_mode state
        | Up -> move_selection state (-1)
        | Down -> move_selection state 1
        | Enter -> handle_enter state
        | Escape -> {state with next_page = Some "__BACK__"}
        | _ -> state  (* Unhandled key *)
```

### **Static Validation (at module registration)**

```ocaml
(* miaou/miaou_core/registry.ml - ENHANCED *)

(** Validate that a page doesn't use reserved keys *)
let validate_keys page_name handled_keys =
  let conflicts =
    List.filter Global_shortcuts.is_reserved handled_keys
  in
  match conflicts with
  | [] -> Ok ()
  | keys ->
      let key_strs = List.map Keys.to_string keys in
      Error (
        Printf.sprintf
          "Page '%s' tries to handle reserved keys: %s"
          page_name
          (String.concat ", " key_strs)
      )

(** Register a page with key validation *)
let register (module P : PAGE_SIG) =
  match validate_keys P.name P.handled_keys with
  | Error msg ->
      failwith msg  (* COMPILE-TIME ERROR via dune build *)
  | Ok () ->
      (* Continue with normal registration *)
      ...
```

### **Result**

**❌ Compile Error:**
```ocaml
(* BAD: Page tries to use reserved 's' *)
let handled_keys = [
  Char 's';  (* start service - CONFLICTS WITH GLOBAL Settings! *)
  Char 'c';
]

(* At page registration: *)
(* Fatal error: Page 'instances' tries to handle reserved keys: s *)
(* Build fails! *)
```

**✅ Compiles:**
```ocaml
(* GOOD: Uses available keys *)
let handled_keys = [
  Char 'c';  (* create *)
  Char 'f';  (* filter *)
  Tab;       (* toggle *)
]

(* No conflicts, build succeeds *)
```

---

## Option 2: GADT-Based Key Types ⭐⭐⭐⭐

### **Concept**

Use GADTs to encode key availability at the type level:

```ocaml
(* miaou/miaou_core/keys.ml - GADT VERSION *)

(** Key availability phantom type *)
type global = Global
type page = Page
type any = Any

(** Key type parameterized by availability *)
type _ t =
  (* Global-only keys *)
  | Settings : global t
  | Help : global t
  | Menu : global t
  | Quit : global t

  (* Page-available keys *)
  | Char : char -> page t
  | Ctrl : char -> page t
  | F : int -> page t

  (* Navigation keys (both can use) *)
  | Up : any t
  | Down : any t
  | Enter : any t
  | Escape : any t
  | Tab : any t

(** Coerce page key to any *)
let to_any : type a. a t -> any t = function
  | Up -> Up
  | Down -> Down
  | Enter -> Enter
  | Escape -> Escape
  | Tab -> Tab
  | Char c -> Char c
  | Ctrl c -> Ctrl c
  | F n -> F n

(** Parse string to any key *)
let of_string s : any t = ...
```

### **Page Handler**

```ocaml
(* Pages can only handle page-available or navigation keys *)

(** Type-safe page key handler *)
val handle_page_key : state -> page t -> state

(** Example *)
let handle_page_key state key =
  match key with
  | Char 'c' -> create_instance state
  | Char 'f' -> cycle_filter state
  | Char 's' -> ...  (* TYPE ERROR: 's' is Settings : global t, not page t *)
  | Up -> move_selection state (-1)
  | ...
```

**Result:** Type error if you try to handle a global key!

**Downside:** More complex types, harder to maintain.

---

## Option 3: PPX Attribute Checker ⭐⭐⭐

### **Concept**

Use a PPX to validate key handlers at compile time:

```ocaml
(* src/ui/pages/instances.ml *)

[@@@check_reserved_keys]

let handle_key state key ~size =
  match key with
  | "s" -> start_service state
  (* ^^^ PPX Error: Key "s" is reserved for global shortcuts *)
  | "c" -> create_instance state  (* OK *)
  | ...
```

### **PPX Implementation**

```ocaml
(* ppx_check_keys/ppx_check_keys.ml *)

open Ppxlib

let reserved = ["s"; "?"; "h"; "m"; "q"]

let check_match_case case =
  match case.pc_lhs.ppat_desc with
  | Ppat_constant (Pconst_string (key, _)) when List.mem key reserved ->
      Location.raise_errorf ~loc:case.pc_lhs.ppat_loc
        "Key %S is reserved for global shortcuts" key
  | _ -> ()

let check_expr expr =
  match expr.pexp_desc with
  | Pexp_match (_, cases) ->
      List.iter check_match_case cases
  | _ -> ()

(* Register PPX *)
let () =
  Driver.register_transformation "check_reserved_keys"
    ~impl:(fun str ->
      List.iter (fun item ->
        (* Check all match expressions in structure *)
        ...
      ) str;
      str
    )
```

**Pros:**
- Works with existing string-based keys
- Clear error messages at exact location

**Cons:**
- PPX complexity
- Needs maintenance for new reserved keys

---

## Option 4: Code Generation from Declaration ⭐⭐⭐

### **Concept**

Declare keys in a config file, generate handler code:

```ocaml
(* instances.keys - DECLARATION FILE *)

page instances {
  global {
    (* Automatically includes all global shortcuts *)
  }

  keys {
    'c' -> create_instance "Create new instance"
    'f' -> cycle_filter "Cycle filter"
    'r' -> toggle_resources "Toggle resource view"
    Tab -> toggle_view_mode "Toggle view mode"
    Up -> move_up "Move selection up"
    Down -> move_down "Move selection down"
    Enter -> handle_enter "Open actions"
    Esc -> go_back "Go back"
  }
}
```

**Generate:**
```ocaml
(* instances_keys.ml - GENERATED *)

type action =
  | Create_instance
  | Cycle_filter
  | Toggle_resources
  | Toggle_view_mode
  | Move_up
  | Move_down
  | Handle_enter
  | Go_back

let of_key key =
  match key with
  | "c" -> Some Create_instance
  | "f" -> Some Cycle_filter
  | "r" | "Tab" -> Some Toggle_resources
  | "Up" -> Some Move_up
  | "Down" -> Some Move_down
  | "Enter" -> Some Handle_enter
  | "Esc" -> Some Go_back
  | _ -> None

let help_text = [
  ("c", "Create new instance");
  ("f", "Cycle filter");
  ("r/Tab", "Toggle resource view");
  ...
]
```

**Page uses generated module:**
```ocaml
let handle_key state key ~size =
  match Instances_keys.of_key key with
  | Some Create_instance -> create_instance state
  | Some Cycle_filter -> cycle_filter state
  | ...
  | None -> state
```

**Validation at generation time:** If declaration uses 's', generator fails.

---

## Option 5: Dune Rule Validation ⭐⭐

### **Concept**

Add a dune rule that validates keys before compilation:

```ocaml
; src/ui/pages/dune

(rule
 (target instances.ml.validated)
 (deps instances.ml)
 (action
  (run validate_keys %{deps})))

(library
 (name octez_manager_ui_pages)
 (deps instances.ml.validated))
```

**Validator tool:**
```bash
#!/usr/bin/env ocaml

(* validate_keys.ml *)
let reserved = ["s"; "?"; "h"; "m"; "q"]

let check_file file =
  let content = read_file file in
  (* Parse OCaml, find match expressions on "key" *)
  (* Check if any cases match reserved keys *)
  (* Exit 1 if conflicts found *)
```

**Pros:**
- External to code
- Can check any pattern

**Cons:**
- Parsing OCaml is complex
- Fragile (depends on code structure)

---

## Comparison Table

| Approach | Static Check | Ease of Use | Maintenance | Type Safety |
|----------|--------------|-------------|-------------|-------------|
| **Variant Keys** | ⭐⭐⭐⭐⭐ | ⭐⭐⭐⭐ | ⭐⭐⭐⭐ | ⭐⭐⭐⭐⭐ |
| **GADT Keys** | ⭐⭐⭐⭐⭐ | ⭐⭐⭐ | ⭐⭐⭐ | ⭐⭐⭐⭐⭐ |
| **PPX Checker** | ⭐⭐⭐⭐ | ⭐⭐⭐⭐⭐ | ⭐⭐ | ⭐⭐⭐ |
| **Code Gen** | ⭐⭐⭐⭐ | ⭐⭐⭐ | ⭐⭐⭐ | ⭐⭐⭐⭐ |
| **Dune Rule** | ⭐⭐⭐ | ⭐⭐⭐ | ⭐⭐ | ⭐⭐ |

---

## Recommendation: Variant-Based Keys

### **Why:**

1. ✅ **Compile-time validation** - Conflicts caught at build time
2. ✅ **Type safety** - Can't use wrong key type
3. ✅ **Self-documenting** - `Settings` is clearer than `"s"`
4. ✅ **IDE support** - Autocomplete for keys
5. ✅ **No magic** - Plain OCaml, no PPX/codegen
6. ✅ **Easy refactoring** - Rename keys safely

### **Migration Path:**

**Phase 1:** Add typed keys alongside strings
```ocaml
(* Both work during migration *)
let handle_key state key_str ~size =
  let key = Keys.of_string key_str in
  match key with
  | Keys.Settings -> ...
  | Keys.Char 'c' -> ...
  | ...
```

**Phase 2:** Update Miaou to use typed keys
```ocaml
(* miaou passes Keys.t instead of string *)
val handle_key : state -> Keys.t -> size:LTerm_geom.size -> state
```

**Phase 3:** Deprecate string keys

---

## Implementation for Miaou

### **Files to Add/Modify:**

**1. New module:** `miaou/miaou_core/keys.ml`
```ocaml
(* Typed key definitions *)
type t = Settings | Help | Menu | Char of char | ...
let of_string : string -> t
let to_string : t -> string
let category : t -> key_category
```

**2. Enhance:** `miaou/miaou_core/tui_page.mli`
```ocaml
(* Add typed key handler *)
val handle_key : state -> Keys.t -> size:LTerm_geom.size -> state
val handled_keys : Keys.t list  (* Declaration for validation *)
```

**3. Enhance:** `miaou/miaou_core/registry.ml`
```ocaml
(* Validate keys at registration *)
let register (module P : PAGE_SIG) =
  validate_keys P.name P.handled_keys |> handle_error;
  ...
```

**4. Enhance:** `miaou/miaou_driver_term/lambda_term_driver.ml`
```ocaml
(* Parse LTerm keys to Keys.t *)
let key_of_term_event event =
  match event with
  | LTerm_event.Key { code = Char 's'; ... } -> Keys.Settings
  | LTerm_event.Key { code = Char 'c'; ... } -> Keys.Char 'c'
  | ...
```

---

## Example Usage (After Implementation)

### **Global Shortcuts**
```ocaml
(* src/ui/global_shortcuts.ml *)
open Miaou.Core.Keys

let handle = function
  | Settings -> Context.navigate "settings"; Handled
  | Help -> show_help (); Handled
  | Menu -> show_menu (); Handled
  | _ -> NotGlobal
```

### **Page Handler**
```ocaml
(* src/ui/pages/instances.ml *)
open Miaou.Core.Keys

let handled_keys = [
  Char 'c'; Char 'f'; Char 'r'; Tab; Up; Down; Enter; Escape
]

let handle_key state = function
  | Char 'c' -> create_instance state
  | Char 'f' -> cycle_filter state
  | Char 's' -> ...
  (* ^^^ COMPILE ERROR: 's' is not in handled_keys! *)
  (* And Settings is not a page-available key! *)
  | Tab -> toggle_view state
  | Up -> move_selection state (-1)
  | Down -> move_selection state 1
  | Enter -> handle_enter state
  | Escape -> go_back state
  | _ -> state
```

### **Validation at Registration**
```ocaml
(* manager_app.ml *)
let register_pages () =
  Instances.register ();
  (* If Instances.handled_keys contains Settings, build fails here *)
  ...
```

---

## Conclusion

**For Miaou Enhancement:**
- ✅ Implement **Variant-Based Keys**
- ✅ Add `Keys.t` type with variants
- ✅ Add `handled_keys` declaration to `PAGE_SIG`
- ✅ Validate at registration time
- ✅ Provide migration path from strings

**Benefits:**
- Compile-time conflict detection
- Better IDE support
- Self-documenting code
- Type-safe refactoring
- No runtime overhead

**Effort:** ~2-3 days for miaou, ~1 day for octez-manager migration

---

**Document Version:** 1.0
**Last Updated:** 2025-12-10
**Status:** Design Proposal
