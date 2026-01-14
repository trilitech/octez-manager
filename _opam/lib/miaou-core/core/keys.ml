(*****************************************************************************)
(*                                                                           *)
(* SPDX-License-Identifier: MIT                                              *)
(* Copyright (c) 2025 Nomadic Labs <contact@nomadic-labs.com>                *)
(*                                                                           *)
(*****************************************************************************)

type t =
  | Up
  | Down
  | Left
  | Right
  | Tab
  | ShiftTab
  | Enter
  | Backspace
  | Char of string
  | Control of string
  | PageUp
  | PageDown
  | Home
  | End
  | Escape
  | Delete
  | Function of int

(* Global keys reserved for application-wide functionality *)
type global_key = Settings | Help | Menu | Quit

let to_string = function
  | Up -> "Up"
  | Down -> "Down"
  | Left -> "Left"
  | Right -> "Right"
  | Tab -> "Tab"
  | ShiftTab -> "Shift-Tab"
  | Enter -> "Enter"
  | Backspace -> "Backspace"
  | Char s -> s
  | Control s -> "C-" ^ s
  | PageUp -> "PageUp"
  | PageDown -> "PageDown"
  | Home -> "Home"
  | End -> "End"
  | Escape -> "Escape"
  | Delete -> "Delete"
  | Function n -> "F" ^ string_of_int n

let of_string s =
  match s with
  | "Up" -> Some Up
  | "Down" -> Some Down
  | "Left" -> Some Left
  | "Right" -> Some Right
  | "Tab" -> Some Tab
  | "Shift-Tab" -> Some ShiftTab
  | "Enter" -> Some Enter
  | "Backspace" -> Some Backspace
  | "PageUp" -> Some PageUp
  | "PageDown" -> Some PageDown
  | "Home" -> Some Home
  | "End" -> Some End
  | "Escape" -> Some Escape
  | "Delete" -> Some Delete
  | _ when String.length s > 1 && String.get s 0 = 'F' -> (
      try Some (Function (int_of_string (String.sub s 1 (String.length s - 1))))
      with _ -> Some (Char s))
  | _ when String.length s >= 2 && String.get s 0 = 'C' && String.get s 1 = '-'
    ->
      Some (Control (String.sub s 2 (String.length s - 2)))
  | _ -> Some (Char s)

let equal a b = to_string a = to_string b

let to_label = function
  | Up -> "↑"
  | Down -> "↓"
  | Left -> "←"
  | Right -> "→"
  | Tab -> "Tab"
  | ShiftTab -> "Shift-Tab"
  | Enter -> "Enter"
  | Backspace -> "Backspace"
  | Char s -> s
  | Control s -> "C-" ^ String.uppercase_ascii s
  | PageUp -> "PgUp"
  | PageDown -> "PgDn"
  | Home -> "Home"
  | End -> "End"
  | Escape -> "Esc"
  | Delete -> "Del"
  | Function n -> "F" ^ string_of_int n

(* Global key mappings - reserved for application-wide functionality *)
let global_key_bindings =
  [
    (Control "s", Settings);
    (Char "?", Help);
    (Control "m", Menu);
    (Control "q", Quit);
  ]

let is_global_key key =
  List.exists (fun (k, _) -> equal k key) global_key_bindings

let get_global_action key =
  List.assoc_opt
    key
    (List.map (fun (k, a) -> (to_string k, a)) global_key_bindings)

let show_global_keys () =
  List.map
    (fun (key, action) ->
      ( to_label key,
        match action with
        | Settings -> "Settings"
        | Help -> "Help"
        | Menu -> "Menu"
        | Quit -> "Quit" ))
    global_key_bindings
