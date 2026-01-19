(******************************************************************************)
(*                                                                            *)
(* SPDX-License-Identifier: MIT                                               *)
(* Copyright (c) 2026 Nomadic Labs <contact@nomadic-labs.com>                 *)
(*                                                                            *)
(******************************************************************************)

(** {1 Parsing} *)

let unquote s =
  let len = String.length s in
  if
    len >= 2
    && ((s.[0] = '"' && s.[len - 1] = '"')
       || (s.[0] = '\'' && s.[len - 1] = '\''))
  then String.sub s 1 (len - 2)
  else s

let parse_string content =
  let lines = String.split_on_char '\n' content in
  List.filter_map
    (fun line ->
      let trimmed = String.trim line in
      if trimmed = "" || String.starts_with ~prefix:"#" trimmed then None
      else
        match String.split_on_char '=' trimmed with
        | [] -> None
        | [key] -> Some (key, "")
        | key :: rest ->
            let value = String.concat "=" rest |> String.trim |> unquote in
            Some (key, value))
    lines

let parse_file path =
  try
    let ic = open_in path in
    Fun.protect
      ~finally:(fun () -> close_in_noerr ic)
      (fun () ->
        let content = really_input_string ic (in_channel_length ic) in
        Ok (parse_string content))
  with
  | Sys_error msg -> Error msg
  | End_of_file -> Ok []

(** {1 Variable Expansion} *)

let rec expand_vars ~env str =
  (* Find ${VAR} pattern *)
  let find_var_pattern s =
    try
      let start = String.index s '$' in
      if start + 1 < String.length s && s.[start + 1] = '{' then
        (* ${VAR} format *)
        let end_pos = String.index_from s (start + 2) '}' in
        let var_name = String.sub s (start + 2) (end_pos - start - 2) in
        Some (start, end_pos + 1, var_name, true)
      else
        (* $VAR format - find end of variable name *)
        let rec find_end pos =
          if pos >= String.length s then pos
          else
            match s.[pos] with
            | 'A' .. 'Z' | 'a' .. 'z' | '0' .. '9' | '_' -> find_end (pos + 1)
            | _ -> pos
        in
        let end_pos = find_end (start + 1) in
        if end_pos > start + 1 then
          let var_name = String.sub s (start + 1) (end_pos - start - 1) in
          Some (start, end_pos, var_name, false)
        else None
    with Not_found -> None
  in

  match find_var_pattern str with
  | None -> str
  | Some (start, end_pos, var_name, _is_braced) ->
      let prefix = String.sub str 0 start in
      let suffix = String.sub str end_pos (String.length str - end_pos) in
      let replacement =
        match List.assoc_opt var_name env with
        | Some value -> value (* Expand *)
        | None ->
            (* Variable not found - keep original *)
            String.sub str start (end_pos - start)
      in
      (* Recursively expand remaining variables in suffix only (avoids infinite loop) *)
      prefix ^ replacement ^ expand_vars ~env suffix
