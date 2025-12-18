(******************************************************************************)
(*                                                                            *)
(* SPDX-License-Identifier: MIT                                               *)
(* Copyright (c) 2025 Nomadic Labs <contact@nomadic-labs.com>                 *)
(*                                                                            *)
(******************************************************************************)

(* Logging is always via journald - octez binaries handle their own file logging *)
type t = Journald

let default = Journald

let to_string = function Journald -> "journald"

let to_yojson = function
  | Journald -> `Assoc [("type", `String "journald")]

let of_yojson json =
  let open Yojson.Safe.Util in
  try
    match member "type" json with
    | `String "journald" -> Ok Journald
    | `String "file" ->
        (* Legacy: convert old file-based logging to journald *)
        Ok Journald
    | _ -> Error "Invalid logging mode JSON"
  with Type_error (msg, _) -> Error msg
