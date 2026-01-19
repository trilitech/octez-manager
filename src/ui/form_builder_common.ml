(******************************************************************************)
(*                                                                            *)
(* SPDX-License-Identifier: MIT                                               *)
(* Copyright (c) 2025-2026 Nomadic Labs <contact@nomadic-labs.com>            *)
(*                                                                            *)
(******************************************************************************)

open Octez_manager_lib

(** {1 Cached Service States}

    Forms call validators frequently (on every render). To avoid repeated
    syscalls, we cache service states with a short TTL. *)

let service_states_cache =
  Cache.create ~name:"service_states" ~ttl:0.5 Data.load_service_states

(** Get service states, refreshing cache if expired. Use for initial form setup. *)
let cached_service_states () = Cache.get service_states_cache

(** Get service states from cache without blocking. Use in validators during typing.
    Returns empty list if cache is empty (rare - only before first load). *)
let cached_service_states_nonblocking () =
  match Cache.get_cached service_states_cache with Some v -> v | None -> []

let invalidate_service_states_cache () = Cache.invalidate service_states_cache

(** {1 Configuration Types} *)

type core_service_config = {
  instance_name : string;
  service_user : string;
  app_bin_dir : string;
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

(** Cache for service user validation results.
    User existence rarely changes during a form session. *)
let user_valid_cache =
  Cache.create_keyed ~name:"user_validation" ~ttl:5.0 (fun user ->
      Result.is_ok (System_user.validate_user_for_service ~user))

let service_user_valid ~user =
  if Common.is_root () then true else Cache.get_keyed user_valid_cache user

let parse_host_port (s : string) : (string * int) option =
  match String.split_on_char ':' s with
  | [host; port] -> (
      try
        let p = int_of_string (String.trim port) in
        let h = String.trim host in
        if p > 0 && p < 65536 && h <> "" then Some (h, p) else None
      with _ -> None)
  | _ -> None

let default_service_user () =
  if Common.is_root () then "octez"
  else
    match Unix.getpwuid (Unix.geteuid ()) with
    | pw when String.trim pw.Unix.pw_name <> "" -> pw.Unix.pw_name
    | _ -> "octez"

let default_base_dir ~role ~instance = Common.default_role_dir role instance

(** Check if a binary exists in a directory and is executable. *)
let has_binary binary_name dir =
  let trimmed = String.trim dir in
  if trimmed = "" then false
  else
    let candidate = Filename.concat trimmed binary_name in
    Sys.file_exists candidate
    &&
      try
        Unix.access candidate [Unix.X_OK] ;
        true
      with Unix.Unix_error _ -> false

(** Check if octez-baker binary exists and is executable. *)
let has_octez_baker_binary = has_binary "octez-baker"

(** Check if octez-node binary exists and is executable. *)
let has_octez_node_binary = has_binary "octez-node"

(** Check if octez-signer binary exists and is executable. *)
let has_octez_signer_binary = has_binary "octez-signer"

(** Check if octez-dal-node binary exists and is executable. *)
let has_octez_dal_node_binary = has_binary "octez-dal-node"

(** Cache for binary accessibility validation.
    Checks if service user can execute the binary.
    Cache key format: "user|app_bin_dir|binary_name"
    Note: Uses pipe (|) as separator which is safe since it's not a valid
    character in Unix usernames or filesystem paths.
    TTL is 5s since this involves subprocess calls. *)
let binary_accessible_cache =
  Cache.create_keyed ~name:"binary_accessible" ~ttl:5.0 (fun key ->
      match String.split_on_char '|' key with
      | [user; app_bin_dir; binary_name] ->
          let binary_path = Filename.concat app_bin_dir binary_name in
          Result.is_ok (Systemd.validate_binary_access ~user ~binary_path)
      | _ -> false)

(** Validate that the service user can access and execute the binary.
    This is more comprehensive than just checking if the binary exists -
    it verifies the service user has permission to execute it.
    Returns true if accessible, false otherwise.
    Uses caching to avoid excessive subprocess calls. *)
let binary_accessible_to_user ~user ~app_bin_dir ~binary_name =
  if not (Common.is_root ()) then
    (* In user mode, just check if current user can access *)
    has_binary binary_name app_bin_dir
  else
    (* In root mode, verify service user can access *)
    let cache_key = Printf.sprintf "%s|%s|%s" user app_bin_dir binary_name in
    Cache.get_keyed binary_accessible_cache cache_key

let endpoint_with_scheme rpc_addr =
  let trimmed = String.trim rpc_addr in
  if trimmed = "" then "http://127.0.0.1:8732"
  else if
    String.starts_with ~prefix:"http://" (String.lowercase_ascii trimmed)
    || String.starts_with ~prefix:"https://" (String.lowercase_ascii trimmed)
  then trimmed
  else "http://" ^ trimmed

let endpoint_of_service (svc : Service.t) =
  endpoint_with_scheme svc.Service.rpc_addr

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
      | _, true, _ ->
          (* Previous char was backslash, add current char literally *)
          parse_loop (i + 1) acc (current ^ String.make 1 c) in_quote false
      | Some '"', false, '\\' ->
          (* Backslash in double quotes - escape next char *)
          parse_loop (i + 1) acc current in_quote true
      | None, false, '\\' ->
          (* Backslash outside quotes - escape next char *)
          parse_loop (i + 1) acc current in_quote true
      (* Handle quote boundaries *)
      | None, false, '\'' ->
          (* Start single quote *)
          parse_loop (i + 1) acc current (Some '\'') false
      | Some '\'', false, '\'' ->
          (* End single quote *)
          parse_loop (i + 1) acc current None false
      | None, false, '"' ->
          (* Start double quote *)
          parse_loop (i + 1) acc current (Some '"') false
      | Some '"', false, '"' ->
          (* End double quote *)
          parse_loop (i + 1) acc current None false
      (* Handle whitespace *)
      | None, false, (' ' | '\t' | '\n' | '\r') ->
          (* Whitespace outside quotes - word boundary *)
          if current = "" then parse_loop (i + 1) acc current None false
          else parse_loop (i + 1) (current :: acc) "" None false
      (* Regular characters *)
      | _ -> parse_loop (i + 1) acc (current ^ String.make 1 c) in_quote false
  in
  parse_loop 0 [] "" None false

let prepare_extra_args s =
  if String.trim s = "" then [] else parse_shellwords (String.trim s)

(** Find the best default app_bin_dir for a given binary.

    Priority order:
    1. Use `which <binary>` to find system-installed binary
    2. Look in registered services for a directory containing the binary
    3. Fall back to /usr/bin

    @param binary_name The name of the binary to find (e.g., "octez-node")
    @return The directory containing the binary, or /usr/bin as fallback *)
let default_app_bin_dir ~binary_name =
  (* 1. Try `which` first *)
  match Common.which binary_name with
  | Some path ->
      (* which returns full path, we need the directory *)
      Filename.dirname path
  | None -> (
      (* 2. Look in registered services for a directory with this binary *)
      match Service_registry.list () with
      | Ok services -> (
          let found =
            List.find_opt
              (fun (svc : Service.t) -> has_binary binary_name svc.app_bin_dir)
              services
          in
          match found with Some svc -> svc.app_bin_dir | None -> "/usr/bin")
      | Error _ -> "/usr/bin")
