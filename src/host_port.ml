(******************************************************************************)
(*                                                                            *)
(* SPDX-License-Identifier: MIT                                               *)
(* Copyright (c) 2026 Nomadic Labs <contact@nomadic-labs.com>                 *)
(*                                                                            *)
(******************************************************************************)

type error = Invalid_format | Bare_ipv6

let bare_ipv6_message = "IPv6 addresses must be bracketed: [::1]:8732"

let split_bracketed (s : string) : (string * string, error) result =
  let len = String.length s in
  match String.index_opt s ']' with
  | None -> Error Invalid_format
  | Some close_idx -> (
      let host = String.sub s 0 (close_idx + 1) in
      let rest_start = close_idx + 1 in
      match rest_start < len && s.[rest_start] = ':' with
      | false -> Error Invalid_format
      | true ->
          let port = String.sub s (rest_start + 1) (len - rest_start - 1) in
          Ok (host, port))

let split (s : string) : (string * string, error) result =
  if String.length s > 0 && s.[0] = '[' then split_bracketed s
  else
    match String.split_on_char ':' s with
    | [host; port] -> Ok (host, port)
    | [] | [_] -> Error Invalid_format
    | _ :: _ :: _ :: _ -> Error Bare_ipv6
