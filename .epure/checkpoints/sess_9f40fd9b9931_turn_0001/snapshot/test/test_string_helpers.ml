(******************************************************************************)
(*                                                                            *)
(* SPDX-License-Identifier: MIT                                               *)
(* Copyright (c) 2026 Nomadic Labs <contact@nomadic-labs.com>                 *)
(*                                                                            *)
(******************************************************************************)

(** Shared string-search helpers for tests.

    Provides [contains_substring] and [string_contains] so that
    every test file does not have to redefine its own copy. *)

(** [contains_substring haystack needle] returns [true] when [needle] appears
    anywhere inside [haystack]. *)
let contains_substring haystack needle =
  let nlen = String.length needle in
  let hlen = String.length haystack in
  if nlen = 0 then true
  else if nlen > hlen then false
  else
    let rec loop i =
      if i + nlen > hlen then false
      else if String.sub haystack i nlen = needle then true
      else loop (i + 1)
    in
    loop 0

(** [string_contains ~needle haystack] is the same as
    {!contains_substring} with a labelled [needle] argument. *)
let string_contains ~needle haystack = contains_substring haystack needle
