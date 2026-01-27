(******************************************************************************)
(*                                                                            *)
(* SPDX-License-Identifier: MIT                                               *)
(* Copyright (c) 2026 Nomadic Labs <contact@nomadic-labs.com>                 *)
(*                                                                            *)
(******************************************************************************)

(** {1 Helpers} *)

let string_contains ~needle haystack =
  let nlen = String.length needle in
  let hlen = String.length haystack in
  let rec loop idx =
    if idx + nlen > hlen then false
    else if String.sub haystack idx nlen = needle then true
    else loop (idx + 1)
  in
  if nlen = 0 then true else loop 0

(** Check if word looks like an octez/tezos binary *)
let is_octez_binary word =
  (* Extract basename to avoid matching directory paths like /var/lib/octez-external *)
  let basename = Filename.basename word in
  let basename_lower = String.lowercase_ascii basename in
  String.starts_with ~prefix:"octez-" basename_lower
  || String.starts_with ~prefix:"tezos-baker" basename_lower
  || String.starts_with ~prefix:"tezos-accuser" basename_lower

(** {1 Shell Unwrapping} *)

let is_shell_script exec_start =
  let trimmed = String.trim exec_start in
  String.starts_with ~prefix:"/bin/sh " trimmed
  || String.starts_with ~prefix:"sh " trimmed
  || String.starts_with ~prefix:"/bin/bash " trimmed
  || String.starts_with ~prefix:"bash " trimmed

(** Extract command from shell wrapper like "/bin/sh -c 'command'" *)
let unwrap_shell exec_start =
  if not (is_shell_script exec_start) then exec_start
  else
    let trimmed = String.trim exec_start in
    (* Find -c or -lc flag *)
    let has_c_flag =
      string_contains ~needle:" -c " trimmed
      || string_contains ~needle:" -lc " trimmed
    in
    if not has_c_flag then exec_start
    else
      (* Find the command after -c or -lc *)
      (* Look for quoted string after -c or -lc *)
      let rec find_quote idx =
        if idx >= String.length trimmed then None
        else if trimmed.[idx] = '\'' || trimmed.[idx] = '"' then Some idx
        else find_quote (idx + 1)
      in
      match find_quote 0 with
      | None -> exec_start (* No quotes found, return original *)
      | Some start ->
          let quote_char = trimmed.[start] in
          (* Find matching closing quote *)
          let rec find_closing idx depth =
            if idx >= String.length trimmed then String.length trimmed
            else if trimmed.[idx] = '\\' then find_closing (idx + 2) depth
            else if trimmed.[idx] = quote_char then
              if depth = 0 then idx else find_closing (idx + 1) (depth - 1)
            else find_closing (idx + 1) depth
          in
          let end_pos = find_closing (start + 1) 0 in
          String.sub trimmed (start + 1) (end_pos - start - 1)

(** {1 Binary Path Extraction} *)

(** Remove quotes from a string, including escaped quotes *)
let unquote s =
  let len = String.length s in
  (* Handle backslash-escaped double quotes in shell *)
  if
    len >= 4
    && s.[0] = '\\'
    && s.[1] = '"'
    && s.[len - 2] = '\\'
    && s.[len - 1] = '"'
  then String.sub s 2 (len - 4)
  else if
    (* Handle regular quotes *)
    len >= 2
    && ((s.[0] = '"' && s.[len - 1] = '"')
       || (s.[0] = '\'' && s.[len - 1] = '\''))
  then String.sub s 1 (len - 2)
  else s

let extract_binary_path exec_start =
  let unwrapped = unwrap_shell exec_start in
  (* Split by spaces and look for octez binary *)
  let words = String.split_on_char ' ' unwrapped in
  List.find_opt is_octez_binary words |> Option.map unquote

(** {1 Argument Parsing} *)

type parsed_args = {
  binary_path : string option;
  subcommand : string option; (* e.g., "run", "dal" for octez-baker *)
  run_mode : string option;
      (* For baker/accuser: "with local node" or "remotely" *)
  data_dir : string option;
  base_dir : string option;
  rpc_addr : string option;
  net_addr : string option;
  endpoint : string option;
  dal_endpoint : string option;
  history_mode : string option;
  network : string option;
  extra_args : string list;
  warnings : string list;
}

let empty_args =
  {
    binary_path = None;
    subcommand = None;
    run_mode = None;
    data_dir = None;
    base_dir = None;
    rpc_addr = None;
    net_addr = None;
    endpoint = None;
    dal_endpoint = None;
    history_mode = None;
    network = None;
    extra_args = [];
    warnings = [];
  }

(** Check if string contains unexpanded variable like ${VAR} or $VAR *)
let contains_variable s =
  string_contains ~needle:"${" s
  || (String.contains s '$' && String.length s > 1)

(** Helper to parse a flag with value *)
let parse_flag_value ~flag_name ~setter word rest acc =
  let value_result =
    if String.contains word '=' then
      (* --flag=value *)
      match String.split_on_char '=' word with
      | [_; value] -> Some (unquote (String.trim value), rest)
      | _ -> None
    else
      (* --flag value *)
      match rest with
      | value :: rest' -> Some (unquote (String.trim value), rest')
      | [] -> None
  in
  match value_result with
  | Some (cleaned, new_rest) ->
      if contains_variable cleaned then
        let warning = flag_name ^ " contains unexpanded variable: " ^ cleaned in
        (new_rest, {acc with warnings = warning :: acc.warnings})
      else (new_rest, setter acc cleaned)
  | None -> (rest, acc)

(** Parse --flag=value or --flag value patterns *)
let rec parse_args_list words acc =
  match words with
  | [] -> acc
  | word :: rest ->
      (* Skip shell keywords *)
      if
        word = "exec" || word = "if" || word = "then" || word = "else"
        || word = "fi" || word = ";"
      then parse_args_list rest acc
      else if String.starts_with ~prefix:"--data-dir" word then
        let new_rest, new_acc =
          parse_flag_value
            ~flag_name:"data-dir"
            ~setter:(fun a v -> {a with data_dir = Some v})
            word
            rest
            acc
        in
        parse_args_list new_rest new_acc
      else if String.starts_with ~prefix:"--base-dir" word then
        let new_rest, new_acc =
          parse_flag_value
            ~flag_name:"base-dir"
            ~setter:(fun a v -> {a with base_dir = Some v})
            word
            rest
            acc
        in
        parse_args_list new_rest new_acc
      else if String.starts_with ~prefix:"--rpc-addr" word then
        let new_rest, new_acc =
          parse_flag_value
            ~flag_name:"rpc-addr"
            ~setter:(fun a v -> {a with rpc_addr = Some v})
            word
            rest
            acc
        in
        parse_args_list new_rest new_acc
      else if String.starts_with ~prefix:"--net-addr" word then
        let new_rest, new_acc =
          parse_flag_value
            ~flag_name:"net-addr"
            ~setter:(fun a v -> {a with net_addr = Some v})
            word
            rest
            acc
        in
        parse_args_list new_rest new_acc
      else if String.starts_with ~prefix:"--endpoint" word || word = "-E" then
        let new_rest, new_acc =
          parse_flag_value
            ~flag_name:"endpoint"
            ~setter:(fun a v -> {a with endpoint = Some v})
            word
            rest
            acc
        in
        parse_args_list new_rest new_acc
      else if String.starts_with ~prefix:"--dal-node" word then
        let new_rest, new_acc =
          parse_flag_value
            ~flag_name:"dal-node"
            ~setter:(fun a v -> {a with dal_endpoint = Some v})
            word
            rest
            acc
        in
        parse_args_list new_rest new_acc
      else if String.starts_with ~prefix:"--history-mode" word then
        let new_rest, new_acc =
          parse_flag_value
            ~flag_name:"history-mode"
            ~setter:(fun a v -> {a with history_mode = Some v})
            word
            rest
            acc
        in
        parse_args_list new_rest new_acc
      else if String.starts_with ~prefix:"--network" word then
        let new_rest, new_acc =
          parse_flag_value
            ~flag_name:"network"
            ~setter:(fun a v -> {a with network = Some v})
            word
            rest
            acc
        in
        parse_args_list new_rest new_acc
      else if String.starts_with ~prefix:"-" word then
        (* Other flag - add to extra_args *)
        parse_args_list rest {acc with extra_args = word :: acc.extra_args}
      else if is_octez_binary word then
        (* Found binary path *)
        parse_args_list rest {acc with binary_path = Some (unquote word)}
      else if acc.binary_path <> None && acc.subcommand = None then
        (* First non-flag word after binary is likely a subcommand *)
        (* Common subcommands: run, config, snapshot, dal *)
        if word = "run" || word = "config" || word = "snapshot" then
          parse_args_list rest {acc with subcommand = Some word}
        else if word = "dal" then
          (* DAL can be standalone or after 'run' *)
          parse_args_list rest {acc with subcommand = Some "dal"}
        else parse_args_list rest {acc with extra_args = word :: acc.extra_args}
      else if acc.subcommand = Some "run" && word = "dal" then
        (* Special case: 'octez-baker run dal' -> change subcommand to 'dal' *)
        parse_args_list rest {acc with subcommand = Some "dal"}
      else if acc.subcommand = Some "run" && word = "accuser" then
        (* Special case: 'octez-baker run accuser' -> change subcommand to 'accuser' *)
        parse_args_list rest {acc with subcommand = Some "accuser"}
      else if acc.subcommand = Some "run" && word = "with" then
        (* Baker: 'run with local node <path>' *)
        match rest with
        | "local" :: "node" :: node_data_dir :: rest' ->
            parse_args_list
              rest'
              {
                acc with
                run_mode = Some "with local node";
                data_dir = Some (unquote node_data_dir);
              }
        | _ ->
            parse_args_list rest {acc with extra_args = word :: acc.extra_args}
      else if acc.subcommand = Some "run" && word = "remotely" then
        (* Baker: 'run remotely' *)
        parse_args_list rest {acc with run_mode = Some "remotely"}
      else
        (* Other argument *)
        parse_args_list rest {acc with extra_args = word :: acc.extra_args}

let parse exec_start =
  let unwrapped = unwrap_shell exec_start in
  (* Split by spaces *)
  let words =
    String.split_on_char ' ' unwrapped
    |> List.map String.trim
    |> List.filter (fun s -> s <> "")
  in
  let result = parse_args_list words empty_args in
  (* Reverse extra_args since we accumulated in reverse *)
  {result with extra_args = List.rev result.extra_args}
