(******************************************************************************)
(*                                                                            *)
(* SPDX-License-Identifier: MIT                                               *)
(* Copyright (c) 2026 Nomadic Labs <contact@nomadic-labs.com>                 *)
(*                                                                            *)
(******************************************************************************)

(** Theme loading and resolution for the UI. *)

(** Theme info for display in the picker *)
type theme_info = {
  id : string;  (** Theme identifier (e.g., "dracula", "dark") *)
  name : string;  (** Display name (e.g., "Dracula", "Dark") *)
  description : string;  (** Short description *)
  dark_mode : bool;  (** Whether this is a dark theme *)
  source : [`Builtin | `Miaou | `User];  (** Where the theme comes from *)
}

(** Load a theme by name or path.
    - [None] defaults to the built-in dark theme
    - [Some "dark"|"light"|"default"] use built-in themes
    - Other Miaou built-in names (e.g., "dracula", "catppuccin-mocha")
    - Other values are treated as file paths or names in the theme folder
    Returns the theme and an optional warning message. *)
val load : ?name:string -> unit -> Miaou_style.Theme.t * string option

(** Directory for user theme overrides. *)
val themes_dir : unit -> string

(** Get current active theme. *)
val get_current : unit -> Miaou_style.Theme.t

(** Set current active theme. Also exports the theme to a temp file
    and sets MIAOU_THEME env var so the Miaou driver uses it. *)
val set_current : Miaou_style.Theme.t -> unit

(** List available theme names (built-ins + user themes). *)
val list_available : unit -> string list

(** List all available themes with full info for the theme picker.
    Returns themes grouped: octez-manager built-ins first, then Miaou built-ins,
    then user themes. *)
val list_all : unit -> theme_info list

(** Save theme preference to disk. *)
val save_preference : string -> unit
