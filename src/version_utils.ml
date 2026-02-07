(******************************************************************************)
(*                                                                            *)
(* SPDX-License-Identifier: MIT                                               *)
(* Copyright (c) 2026 Nomadic Labs <contact@nomadic-labs.com>                 *)
(*                                                                            *)
(******************************************************************************)

let parse_version s =
  (* Strip leading 'v' if present *)
  let s =
    if String.length s > 0 && s.[0] = 'v' then
      String.sub s 1 (String.length s - 1)
    else s
  in
  (* Remove any suffix after '-' (e.g., "-rc1") *)
  let base =
    match String.index_opt s '-' with Some i -> String.sub s 0 i | None -> s
  in
  try
    String.split_on_char '.' base
    |> List.map (fun part -> int_of_string (String.trim part))
  with _ -> []

let is_rc version = String.contains version '-'

let extract_rc_number version =
  if not (is_rc version) then None
  else
    try
      let parts = String.split_on_char '-' version in
      match parts with
      | [_; rc_part] when String.length rc_part > 2 ->
          let rc_str = String.sub rc_part 2 (String.length rc_part - 2) in
          Some (int_of_string rc_str)
      | _ -> None
    with _ -> None

let compare_versions v1 v2 =
  let parts1 = parse_version v1 in
  let parts2 = parse_version v2 in
  (* Treat unparseable versions as older than any valid version *)
  match (parts1, parts2) with
  | [], [] -> 0
  | [], _ -> -1
  | _, [] -> 1
  | _ -> (
      let rec compare_parts l1 l2 =
        match (l1, l2) with
        | [], [] -> 0
        | [], 0 :: t2 -> compare_parts [] t2
        | 0 :: t1, [] -> compare_parts t1 []
        | [], _ -> -1
        | _, [] -> 1
        | h1 :: t1, h2 :: t2 ->
            if h1 < h2 then -1 else if h1 > h2 then 1 else compare_parts t1 t2
      in
      let base_cmp = compare_parts parts1 parts2 in
      if base_cmp <> 0 then base_cmp
      else
        (* Same base version, check RC status *)
        match (is_rc v1, is_rc v2) with
        | false, false -> 0
        | true, false -> -1
        | false, true -> 1
        | true, true -> (
            match (extract_rc_number v1, extract_rc_number v2) with
            | Some rc1, Some rc2 -> compare rc1 rc2
            | Some _, None -> 1
            | None, Some _ -> -1
            | None, None -> 0))
