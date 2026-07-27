(******************************************************************************)
(*                                                                            *)
(* SPDX-License-Identifier: MIT                                               *)
(* Copyright (c) 2026 Nomadic Labs <contact@nomadic-labs.com>                 *)
(*                                                                            *)
(******************************************************************************)

type t = string

let default = "127.0.0.1:8732"

let default_dal = "127.0.0.1:10732"

let of_string s = s

let to_string t = t

let host t =
  match Host_port.split t with
  | Error _ -> None
  | Ok (h, _) ->
      let h = String.trim h in
      if h <> "" then Some h else None

let port t =
  match Host_port.split t with
  | Error _ -> None
  | Ok (_, p) -> (
      try
        let p = int_of_string (String.trim p) in
        if p > 0 && p < 65536 then Some p else None
      with _ -> None)

let to_endpoint t =
  let trimmed = String.trim t in
  if trimmed = "" then "http://127.0.0.1:8732"
  else if
    String.starts_with ~prefix:"http://" (String.lowercase_ascii trimmed)
    || String.starts_with ~prefix:"https://" (String.lowercase_ascii trimmed)
  then trimmed
  else "http://" ^ trimmed

let equal = String.equal

let compare = String.compare

let to_yojson t = `String t

let of_yojson = function
  | `String s -> Ok s
  | _ -> Error "Rpc_addr.of_yojson: expected a string"

let pp fmt t = Format.fprintf fmt "%s" t
