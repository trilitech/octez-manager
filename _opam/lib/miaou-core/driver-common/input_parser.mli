(******************************************************************************)
(*                                                                            *)
(* SPDX-License-Identifier: MIT                                               *)
(* Copyright (c) 2025 Nomadic Labs <contact@nomadic-labs.com>                 *)
(*                                                                            *)
(******************************************************************************)

(** Terminal input parser shared between drivers.

    Parses escape sequences for keyboard and mouse input. Based on
    lambda-term driver's tested implementation.

    Key feature: uses peek-then-consume pattern for draining to avoid
    losing input when a different key is in the buffer.

    Usage:
    {[
      let parser = Input_parser.create fd in
      (* ... in poll loop ... *)
      ignore (Input_parser.refill parser ~timeout_s:0.033);
      match Input_parser.parse_key parser with
      | Some key -> handle_key (Input_parser.key_to_string key)
      | None -> (* no input *)
    ]}
*)

(** Parsed key event. *)
type key =
  | Char of string  (** Regular character or UTF-8 grapheme *)
  | Enter
  | Tab
  | Backspace
  | Escape
  | Up
  | Down
  | Left
  | Right
  | Delete
  | Ctrl of char  (** Control + letter, e.g., Ctrl 'a' for C-a *)
  | Mouse of {row : int; col : int; release : bool}
      (** Mouse click. [release] is true for button release (actual click). *)
  | Refresh  (** Synthetic refresh marker (null byte) *)
  | Unknown of string  (** Unrecognized escape sequence *)

(** Parser state. *)
type t

(** Create a new parser for the given file descriptor. *)
val create : Unix.file_descr -> t

(** Read bytes into buffer with timeout.
    @param timeout_s Timeout in seconds (0.0 for non-blocking)
    @return Number of bytes read *)
val refill : t -> timeout_s:float -> int

(** Parse next key from buffer, consuming the bytes.
    Returns [None] if buffer is empty. *)
val parse_key : t -> key option

(** Peek at next key without consuming bytes.
    Returns [None] if buffer is empty or contains incomplete sequence.
    Use this for drain operations to avoid losing input. *)
val peek_key : t -> key option

(** Drain consecutive matching keys using peek-then-consume.
    Call after receiving a navigation key to prevent scroll lag.
    @return Count of drained keys *)
val drain_matching : t -> key -> int

(** Drain all Escape keys from buffer.
    Call after modal close to prevent double-Esc navigation.
    @return Count of drained keys *)
val drain_esc : t -> int

(** Convert key to string for [PAGE.handle_key].
    Examples: [Up] -> ["Up"], [Ctrl 'a'] -> ["C-a"],
    [Mouse {row=5; col=10; _}] -> ["Mouse:5:10"] *)
val key_to_string : key -> string

(** Check if key is a navigation key (Up/Down/Left/Right/Tab/Delete).
    These are candidates for draining. *)
val is_nav_key : key -> bool

(** Get pending buffer length (for debugging). *)
val pending_length : t -> int

(** Clear pending buffer. *)
val clear : t -> unit
