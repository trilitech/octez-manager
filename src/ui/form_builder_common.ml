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
  bin_source : Octez_manager_lib.Binary_registry.bin_source option;
  enable_on_boot : bool;
  start_now : bool;
  extra_args : string;
  group : string option;
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
  if Paths.is_root () then true else Cache.get_keyed user_valid_cache user

let parse_host_port = Port_validation.parse_host_port

let default_service_user () =
  if Paths.is_root () then "octez"
  else
    match Unix.getpwuid (Unix.geteuid ()) with
    | pw when String.trim pw.Unix.pw_name <> "" -> pw.Unix.pw_name
    | _ -> "octez"

let default_base_dir ~role ~instance = Paths.default_role_dir role instance

(** {1 Port Initialization Helpers} *)

(** A port slot for [ensure_ports]. Each slot describes one port field to
    initialize with a free port. *)
type port_slot = {
  current : string;  (** Current value of the port field *)
  default_host : string;
      (** Default host for new address (e.g., "127.0.0.1") *)
  start_port : int;  (** Starting port number for search *)
  setter : string -> unit;  (** Callback to set the new address *)
}

(** Collect ports from service states for a given set of roles.
    Returns (rpc_ports, p2p_ports) as int lists. *)
let ports_from_states ~roles states =
  let role_matches role = List.mem role roles in
  let rpc_ports =
    states
    |> List.filter_map (fun (s : Data.Service_state.t) ->
        if role_matches s.service.Service.role then
          Port_validation.parse_port
            (Rpc_addr.to_string s.service.Service.rpc_addr)
        else None)
  in
  let p2p_ports =
    states
    |> List.filter_map (fun (s : Data.Service_state.t) ->
        if role_matches s.service.Service.role then
          Port_validation.parse_port s.service.Service.net_addr
        else None)
  in
  (rpc_ports, p2p_ports)

(** Initialize port fields with free ports. Scans existing services for
    the given [roles] to find ports to avoid, then assigns free ports to
    any slot whose current value is invalid or conflicting.

    @param roles Service roles to scan for existing port usage
    @param slots Port slots to initialize *)
let ensure_ports ~roles ~slots () =
  let states =
    try cached_service_states () with _ -> []
    (* In tests/early init, capability may be absent; default to empty. *)
  in
  let rpc_ports, p2p_ports = ports_from_states ~roles states in
  let initial_avoid = rpc_ports @ p2p_ports in
  let _final_avoid =
    List.fold_left
      (fun avoid slot ->
        let needs_new =
          match parse_host_port slot.current with
          | Some (_host, port) ->
              port < 1024 || port > 65535 || List.mem port avoid
              || Port_validation.is_port_in_use port
          | None -> true
        in
        if needs_new then (
          let port =
            Port_validation.next_free_port ~start:slot.start_port ~avoid
          in
          slot.setter (Printf.sprintf "%s:%d" slot.default_host port) ;
          port :: avoid)
        else
          match parse_host_port slot.current with
          | Some (_host, port) -> port :: avoid
          | None -> avoid)
      initial_avoid
      slots
  in
  ()

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

(** Check if octez-index binary exists and is executable. *)
let has_octez_index_binary = has_binary "octez-index"

(** Check if signatory binary exists and is executable. *)
let has_signatory_binary = has_binary "signatory"

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
  if not (Paths.is_root ()) then
    (* In user mode, just check if current user can access *)
    has_binary binary_name app_bin_dir
  else
    (* In root mode, verify service user can access *)
    let cache_key = Printf.sprintf "%s|%s|%s" user app_bin_dir binary_name in
    Cache.get_keyed binary_accessible_cache cache_key

let set_service_group ~instance_name ~group =
  match group with
  | None -> Ok ()
  | Some _ -> (
      match Service_registry.find ~instance:instance_name with
      | Ok (Some svc) -> Service_registry.write {svc with group}
      | Ok None ->
          Error
            (`Msg
               (Printf.sprintf
                  "Service '%s' not found after install, group not set"
                  instance_name))
      | Error _ as e -> e)

let require_package_manager () =
  match
    Miaou_interfaces.Capability.get
      Manager_interfaces.Package_manager_capability.key
  with
  | Some cap ->
      let module I =
        (val (cap : Manager_interfaces.Package_manager_capability.t))
      in
      Ok (module I : Manager_interfaces.Package_manager)
  | None -> Error (`Msg "Package manager capability not available")

let endpoint_with_scheme rpc_addr =
  Rpc_addr.to_endpoint (Rpc_addr.of_string rpc_addr)

let endpoint_of_service (svc : Service.t) =
  Rpc_addr.to_endpoint svc.Service.rpc_addr

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
    1. Latest managed version if any exist
    2. Use `which <binary>` to find system-installed binary
    3. Look in registered services for a directory containing the binary
    4. Fall back to /usr/bin

    @param binary_name The name of the binary to find (e.g., "octez-node")
    @return The directory containing the binary, or /usr/bin as fallback *)
let default_app_bin_dir ~binary_name =
  (* 1. Try latest managed version first *)
  match Binary_registry.list_managed_versions () with
  | Ok (latest :: _) -> (
      (* Use latest managed version if available *)
      let version_path = Binary_registry.managed_version_path latest in
      if has_binary binary_name version_path then version_path
      else
        (* Managed version exists but doesn't have this binary, try other sources *)
        match Paths.which binary_name with
        | Some path -> Filename.dirname path
        | None -> (
            match Service_registry.list () with
            | Ok services -> (
                let found =
                  List.find_opt
                    (fun (svc : Service.t) ->
                      has_binary binary_name svc.app_bin_dir)
                    services
                in
                match found with
                | Some svc -> svc.app_bin_dir
                | None -> "/usr/bin")
            | Error _ -> "/usr/bin"))
  | Ok [] | Error _ -> (
      (* 2. No managed versions, try `which` *)
      match Paths.which binary_name with
      | Some path -> Filename.dirname path
      | None -> (
          (* 3. Look in registered services for a directory with this binary *)
          match Service_registry.list () with
          | Ok services -> (
              let found =
                List.find_opt
                  (fun (svc : Service.t) ->
                    has_binary binary_name svc.app_bin_dir)
                  services
              in
              match found with
              | Some svc -> svc.app_bin_dir
              | None -> "/usr/bin")
          | Error _ -> "/usr/bin"))

(** Find the best default app_bin_dir for Signatory binary.

    Priority order:
    1. Latest managed Signatory version if any exist
    2. Use `which signatory` to find system-installed binary
    3. Look in registered services for a directory containing the binary
    4. Fall back to /usr/bin

    @return The directory containing signatory binary, or /usr/bin as fallback *)
let default_signatory_app_bin_dir () =
  (* 1. Try latest managed Signatory version first *)
  match Signatory_downloader.list_managed_versions () with
  | Ok (latest :: _) ->
      (* Use latest managed Signatory version *)
      Signatory_downloader.signatory_version_path latest
  | Ok [] | Error _ -> (
      (* 2. No managed versions, try `which signatory` *)
      match Paths.which "signatory" with
      | Some path -> Filename.dirname path
      | None -> (
          (* 3. Look in registered services for a directory with signatory binary *)
          match Service_registry.list () with
          | Ok services -> (
              let found =
                List.find_opt
                  (fun (svc : Service.t) ->
                    has_binary "signatory" svc.app_bin_dir)
                  services
              in
              match found with
              | Some svc -> svc.app_bin_dir
              | None -> "/usr/bin")
          | Error _ -> "/usr/bin"))
