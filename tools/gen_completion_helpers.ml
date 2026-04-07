(******************************************************************************)
(*                                                                            *)
(* SPDX-License-Identifier: MIT                                               *)
(* Copyright (c) 2026 Nomadic Labs <contact@nomadic-labs.com>                 *)
(*                                                                            *)
(******************************************************************************)

(** Pure helper functions for shell completion script generation.
    Extracted into a library so they can be unit-tested independently of the
    gen_completion executable. *)

(** Escape a string for use inside a zsh single-quoted context.
    Replaces every ['] with ['\\'']. *)
let escape_zsh_single s =
  let parts = String.split_on_char '\'' s in
  String.concat "'\\''" parts

(** Escape a string for use inside a zsh [DESCRIPTION] bracket.
    Zsh [_arguments] uses [':'] as a structural delimiter and [']'] closes
    the description bracket, so both must be escaped. ['['] is escaped for
    symmetry. Single quotes are also escaped via {!escape_zsh_single}. *)
let escape_zsh_description s =
  let s = escape_zsh_single s in
  let buf = Buffer.create (String.length s) in
  String.iter
    (fun c ->
      match c with
      | ':' -> Buffer.add_string buf "\\:"
      | ']' -> Buffer.add_string buf "\\]"
      | '[' -> Buffer.add_string buf "\\["
      | _ -> Buffer.add_char buf c)
    s ;
  Buffer.contents buf

(** Return [true] if [name] is a valid CLI command or subcommand name.
    Rejects tokens like [--octez-version=VERSION] that appear when cmdliner
    wraps a long COMMANDS entry across multiple lines: the continuation line
    starts at the same indentation as real subcommands so the parser
    mistakenly treats the first token as a new command. *)
let is_valid_cmd_name name =
  String.length name > 0
  && String.for_all
       (fun c ->
         (c >= 'a' && c <= 'z')
         || (c >= 'A' && c <= 'Z')
         || (c >= '0' && c <= '9')
         || c = '-' || c = '_')
       name

(** Shell-quote a string for safe interpolation into a [sh -c] command.
    Wraps the string in single quotes and escapes any embedded single quotes
    as ['\\'']. Safe for arbitrary paths including those with spaces or
    shell metacharacters. *)
let quote_shell s =
  "'" ^ String.concat "'\\''" (String.split_on_char '\'' s) ^ "'"
