(*****************************************************************************)
(*                                                                           *)
(* SPDX-License-Identifier: MIT                                              *)
(* Copyright (c) 2025 Nomadic Labs <contact@nomadic-labs.com>                *)
(*                                                                           *)
(*****************************************************************************)
val wrap_content_to_width : string -> int -> string

(** Modal geometry computed from terminal dimensions and UI hints. *)
type modal_geometry = {
  left : int;
  max_width : int;
  content_width : int;
  max_height : int;
  max_content_h : int;
}

(** Compute the geometry used by the modal renderer for a given terminal size. *)
val compute_modal_geometry :
  cols:int ->
  rows:int ->
  left_opt:int option ->
  max_width_opt:int option ->
  modal_geometry

(** Render a minimal subset of Markdown to ANSI-styled text suitable for the TUI.
	Supported features:
	- Headers: #, ##, ###
	- Unordered lists: lines starting with "- "
	- Ordered lists: "1. item"
	- Blockquotes: lines starting with ">"
	- Fenced code blocks: lines starting with "```"
	- Inline code: backticks, e.g., `code`
	- Bold/italic emphasis
	- Horizontal rules: "---"
	- Links: [text](url)

	Unknown constructs are left as plain text. *)
val markdown_to_ansi : string -> string

(** Center each line of ANSI-styled content to the given visible width.
	ANSI escape sequences are preserved and not counted toward the width. *)
val center_content_to_width : string -> int -> string

(** Wrap content to a width preferring word boundaries (ANSI-safe). Falls
	back to character wrapping for words longer than the width. *)
val wrap_content_to_width_words : string -> int -> string
