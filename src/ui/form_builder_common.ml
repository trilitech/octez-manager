(*****************************************************************************)
(*                                                                           *)
(* SPDX-License-Identifier: MIT                                              *)
(* Copyright (c) 2025 Nomadic Labs <contact@nomadic-labs.com>                *)
(*                                                                           *)
(*****************************************************************************)

open Octez_manager_lib

(** {1 Configuration Types} *)

type core_service_config = {
  instance_name : string;
  service_user : string;
  app_bin_dir : string;
  logging : [`Journald | `File];
  enable_on_boot : bool;
  start_now : bool;
  extra_args : string;
}

type client_config = {
  base_dir : string;
  node : [`Service of string | `Endpoint of string | `None];
  node_endpoint : string;
}

type node_config = {
  network : string;
  history_mode : string;
  data_dir : string;
  rpc_addr : string;
  p2p_addr : string;
}

(** {1 Common Validators} *)

let is_nonempty s = String.trim s <> ""

let normalize s = String.lowercase_ascii (String.trim s)

let instance_in_use ~states name =
  let target = normalize name in
  target <> ""
  && List.exists
       (fun (s : Data.Service_state.t) ->
         String.equal target (normalize s.service.Service.instance))
       states

let service_user_valid ~user =
  if Common.is_root () then true
  else Result.is_ok (System_user.validate_user_for_service ~user)

let parse_host_port (s : string) : (string * int) option =
  match String.split_on_char ':' s with
  | [host; port] -> (
      try
        let p = int_of_string (String.trim port) in
        let h = String.trim host in
        if p > 0 && p < 65536 && h <> "" then Some (h, p)
        else None
      with _ -> None)
  | _ -> None

let default_service_user () =
  if Common.is_root () then "octez"
  else
    match Unix.getpwuid (Unix.geteuid ()) with
    | pw when String.trim pw.Unix.pw_name <> "" -> pw.Unix.pw_name
    | _ -> "octez"

(** {1 Helpers} *)

(** Parse shellwords-style arguments with quote support.

    Supports:
    - Single quotes: preserve everything literally
    - Double quotes: preserve spaces, allow escaping with backslash
    - Unquoted: split on spaces
    - Backslash escaping in double quotes and unquoted context

    Examples:
    - "foo bar" -> ["foo"; "bar"]
    - "foo 'bar baz'" -> ["foo"; "bar baz"]
    - "foo \"bar baz\"" -> ["foo"; "bar baz"]
    - "foo\\ bar" -> ["foo bar"]
*)
let parse_shellwords s =
  let len = String.length s in
  let rec parse_loop i acc current in_quote escape =
    if i >= len then
      (* End of string *)
      let final = if current = "" then acc else current :: acc in
      List.rev final
    else
      let c = s.[i] in
      match (in_quote, escape, c) with
      (* Handle escape sequences *)
      | (_, true, _) ->
          (* Previous char was backslash, add current char literally *)
          parse_loop (i + 1) acc (current ^ String.make 1 c) in_quote false
      | (Some '"', false, '\\') ->
          (* Backslash in double quotes - escape next char *)
          parse_loop (i + 1) acc current in_quote true
      | (None, false, '\\') ->
          (* Backslash outside quotes - escape next char *)
          parse_loop (i + 1) acc current in_quote true

      (* Handle quote boundaries *)
      | (None, false, '\'') ->
          (* Start single quote *)
          parse_loop (i + 1) acc current (Some '\'') false
      | (Some '\'', false, '\'') ->
          (* End single quote *)
          parse_loop (i + 1) acc current None false
      | (None, false, '"') ->
          (* Start double quote *)
          parse_loop (i + 1) acc current (Some '"') false
      | (Some '"', false, '"') ->
          (* End double quote *)
          parse_loop (i + 1) acc current None false

      (* Handle whitespace *)
      | (None, false, (' ' | '\t' | '\n' | '\r')) ->
          (* Whitespace outside quotes - word boundary *)
          if current = "" then
            parse_loop (i + 1) acc current None false
          else
            parse_loop (i + 1) (current :: acc) "" None false

      (* Regular characters *)
      | _ ->
          parse_loop (i + 1) acc (current ^ String.make 1 c) in_quote false
  in
  parse_loop 0 [] "" None false

let prepare_extra_args s =
  if String.trim s = "" then []
  else parse_shellwords (String.trim s)
