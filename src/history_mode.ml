(******************************************************************************)
(*                                                                            *)
(* SPDX-License-Identifier: MIT                                               *)
(* Copyright (c) 2025 Nomadic Labs <contact@nomadic-labs.com>                 *)
(*                                                                            *)
(******************************************************************************)

type t = Rolling | Full | Archive

let default = Rolling

let to_string = function
  | Rolling -> "rolling"
  | Full -> "full"
  | Archive -> "archive"

let of_string s =
  match String.lowercase_ascii (String.trim s) with
  | "rolling" -> Ok Rolling
  | "full" -> Ok Full
  | "archive" -> Ok Archive
  | other -> Error (`Msg (Printf.sprintf "Unknown history mode '%s'" other))

let pp fmt t = Format.fprintf fmt "%s" (to_string t)
