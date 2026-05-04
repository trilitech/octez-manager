(******************************************************************************)
(*                                                                            *)
(* SPDX-License-Identifier: MIT                                               *)
(* Copyright (c) 2026 Nomadic Labs <contact@nomadic-labs.com>                 *)
(*                                                                            *)
(******************************************************************************)

(** Foldable JSON viewer with collapsible sections.

    Renders JSON with fold/unfold capability for objects and arrays.
    Tracks fold state per node and renders with indicators. *)

(** Fold state for a JSON document. *)
type t

(** {1 Creation} *)

(** Create foldable JSON from a JSON string.
    All sections are folded by default (only root level visible).
    @param json_str JSON string to parse
    @return Foldable JSON state, or None if parsing fails *)
val of_string : string -> t option

(** Create foldable JSON from parsed Yojson value.
    @param json Parsed JSON value *)
val of_json : Yojson.Safe.t -> t

(** {1 Rendering} *)

(** Render JSON with fold indicators and syntax highlighting.
    Folded sections show as {...} or [...] with item count.
    @param t Foldable JSON state
    @return Rendered string with ANSI colors *)
val render : t -> string

(** Get the raw JSON string (unfolded, no colors).
    Used for saving to file.
    @param t Foldable JSON state
    @return Raw JSON string *)
val raw : t -> string

(** {1 Fold Control} *)

(** Toggle fold state at current cursor line.
    @param t Foldable JSON state
    @param line Current line number (0-indexed)
    @return Updated state *)
val toggle_fold_at_line : t -> line:int -> t

(** Unfold all sections. *)
val unfold_all : t -> t

(** Fold all sections (except root). *)
val fold_all : t -> t

(** {1 Line Information} *)

(** Get total number of rendered lines. *)
val line_count : t -> int

(** Check if line has a foldable section.
    @param t Foldable JSON state
    @param line Line number
    @return true if line contains a foldable object/array *)
val is_foldable_line : t -> line:int -> bool
