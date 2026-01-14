(*****************************************************************************)
(*                                                                           *)
(* SPDX-License-Identifier: MIT                                              *)
(* Copyright (c) 2025 Nomadic Labs <contact@nomadic-labs.com>                *)
(*                                                                           *)
(*****************************************************************************)

val is_utf8_lead : char -> bool

val is_esc_start : string -> int -> bool

val skip_ansi_until_m : string -> int -> int

val visible_chars_count : string -> int

val visible_byte_index_of_pos : string -> int -> int

val has_trailing_reset : string -> bool

val insert_before_reset : string -> string -> string

val pad_to_width : string -> int -> char -> string

(** Efficiently concatenate lines with newlines using Buffer *)
val concat_lines : string list -> string

(** Efficiently concatenate strings with separator using Buffer *)
val concat_with_sep : string -> string list -> string
