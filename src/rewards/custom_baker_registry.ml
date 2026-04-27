(******************************************************************************)
(*                                                                            *)
(* SPDX-License-Identifier: MIT                                               *)
(* Copyright (c) 2026 Nomadic Labs <contact@nomadic-labs.com>                 *)
(*                                                                            *)
(******************************************************************************)

type entry = {
  instance : string;
  baker_pkh : string;
  network : string;
  label : string option;
  endpoint : string;
  payout_key_alias : string;
  base_dir : string;
  octez_client_bin : string;
  added_at : string;
}

(* ── File paths ───────────────────────────────────────────── *)

let custom_bakers_dir () = Filename.concat (Paths.registry_root ()) "rewards"

let custom_bakers_file () =
  Filename.concat (custom_bakers_dir ()) "custom_bakers.json"

let custom_bakers_lock_file () =
  Filename.concat (custom_bakers_dir ()) "custom_bakers.json.lock"

(* ── JSON serialization ───────────────────────────────────── *)

let entry_to_yojson e =
  `Assoc
    [
      ("instance", `String e.instance);
      ("baker_pkh", `String e.baker_pkh);
      ("network", `String e.network);
      ("label", match e.label with None -> `Null | Some l -> `String l);
      ("endpoint", `String e.endpoint);
      ("payout_key_alias", `String e.payout_key_alias);
      ("base_dir", `String e.base_dir);
      ("octez_client_bin", `String e.octez_client_bin);
      ("added_at", `String e.added_at);
    ]

let entry_of_yojson json =
  let open Yojson.Safe.Util in
  try
    let instance = member "instance" json |> to_string in
    let baker_pkh = member "baker_pkh" json |> to_string in
    let network = member "network" json |> to_string in
    let label = member "label" json |> to_string_option in
    let endpoint = member "endpoint" json |> to_string in
    let payout_key_alias = member "payout_key_alias" json |> to_string in
    let base_dir = member "base_dir" json |> to_string in
    let octez_client_bin = member "octez_client_bin" json |> to_string in
    let added_at = member "added_at" json |> to_string in
    Ok
      {
        instance;
        baker_pkh;
        network;
        label;
        endpoint;
        payout_key_alias;
        base_dir;
        octez_client_bin;
        added_at;
      }
  with Type_error (msg, _) ->
    Error (Printf.sprintf "custom baker entry parse error: %s" msg)

let entries_to_yojson entries =
  `Assoc [("bakers", `List (List.map entry_to_yojson entries))]

let entries_of_yojson json =
  let open Yojson.Safe.Util in
  try
    let bakers = member "bakers" json |> to_list in
    let results = List.map entry_of_yojson bakers in
    let errors =
      List.filter_map (function Error e -> Some e | Ok _ -> None) results
    in
    if errors <> [] then Error (String.concat "; " errors)
    else
      Ok (List.filter_map (function Ok e -> Some e | Error _ -> None) results)
  with Type_error (msg, _) ->
    Error (Printf.sprintf "custom_bakers.json parse error: %s" msg)

(* ── File I/O ─────────────────────────────────────────────── *)

let load_entries () =
  let path = custom_bakers_file () in
  if not (Sys.file_exists path) then []
  else
    try
      let json = Yojson.Safe.from_file path in
      match entries_of_yojson json with
      | Ok entries -> entries
      | Error msg ->
          Printf.eprintf "Warning: custom_bakers.json parse error: %s\n%!" msg ;
          []
    with exn ->
      Printf.eprintf
        "Warning: failed to read custom_bakers.json: %s\n%!"
        (Printexc.to_string exn) ;
      []

let save_entries entries =
  let dir = custom_bakers_dir () in
  File_ops.mkdir_p dir ;
  let content =
    Yojson.Safe.pretty_to_string ~std:true (entries_to_yojson entries) ^ "\n"
  in
  let owner, group =
    if Paths.is_root () then ("root", "root")
    else Paths.current_user_group_names ()
  in
  match
    File_ops.write_file
      ~mode:0o644
      ~owner
      ~group
      (custom_bakers_file ())
      content
  with
  | Ok () -> Ok ()
  | Error (`Msg msg) ->
      Error (Printf.sprintf "failed to write custom_bakers.json: %s" msg)

(* ── Validate instance name ───────────────────────────────── *)

(** Mirrors [Systemd.validate_instance_name]: alphanumeric + [. _ -] only. *)
let validate_instance_name instance =
  let is_valid_char = function
    | 'a' .. 'z' | 'A' .. 'Z' | '0' .. '9' | '.' | '_' | '-' -> true
    | _ -> false
  in
  if String.length instance = 0 then Error "instance name cannot be empty"
  else if not (String.for_all is_valid_char instance) then
    Error
      (Printf.sprintf
         "invalid instance name '%s': must contain only alphanumeric \
          characters, dots, underscores, and hyphens"
         instance)
  else Ok ()

(* ── Known-instance helpers ───────────────────────────────── *)

(** Collect managed-service instance names by scanning
    [<registry_root>/services/].  We only read filenames (not full JSON) to
    stay within the [octez_manager_common] dependency boundary. *)
let managed_service_instances () =
  let dir = Filename.concat (Paths.registry_root ()) "services" in
  if not (Sys.file_exists dir) then []
  else
    try
      Sys.readdir dir |> Array.to_list
      |> List.filter_map (fun fname ->
          if Filename.check_suffix fname ".json" then
            Some (Filename.chop_suffix fname ".json")
          else None)
    with _ -> []

(** Derive synthetic test-baker instances from [OM_TEST_BAKER]:
    format is ["network/pkh"] or ["network/pkh,network/pkh,..."].
    Each entry produces ["test-<network>"]. *)
let test_baker_instances () =
  match Sys.getenv_opt "OM_TEST_BAKER" with
  | None | Some "" -> []
  | Some s ->
      String.split_on_char ',' s
      |> List.filter_map (fun entry ->
          let entry = String.trim entry in
          match String.index_opt entry '/' with
          | None -> None
          | Some i ->
              let network = String.sub entry 0 i in
              if String.length network > 0 then
                Some (Printf.sprintf "test-%s" network)
              else None)

(* ── Public API ───────────────────────────────────────────── *)

let ensure_dir () = File_ops.mkdir_p (custom_bakers_dir ())

let list () =
  ensure_dir () ;
  File_ops.with_file_lock (custom_bakers_lock_file ()) (fun () ->
      load_entries ())

let find ~instance =
  ensure_dir () ;
  let entries =
    File_ops.with_file_lock (custom_bakers_lock_file ()) (fun () ->
        load_entries ())
  in
  List.find_opt (fun e -> String.equal e.instance instance) entries

let add entry =
  ensure_dir () ;
  File_ops.with_file_lock (custom_bakers_lock_file ()) (fun () ->
      match validate_instance_name entry.instance with
      | Error _ as e -> e
      | Ok () ->
          let existing = load_entries () in
          let custom_collision =
            List.exists
              (fun e -> String.equal e.instance entry.instance)
              existing
          in
          if custom_collision then
            Error
              (Printf.sprintf
                 "instance '%s' already exists as a custom baker"
                 entry.instance)
          else
            let managed_collision =
              List.mem entry.instance (managed_service_instances ())
            in
            if managed_collision then
              Error
                (Printf.sprintf
                   "instance '%s' already exists as a managed service"
                   entry.instance)
            else
              let test_collision =
                List.mem entry.instance (test_baker_instances ())
              in
              if test_collision then
                Error
                  (Printf.sprintf
                     "instance '%s' conflicts with an OM_TEST_BAKER entry"
                     entry.instance)
              else
                let updated = existing @ [entry] in
                save_entries updated)

let remove ~instance =
  ensure_dir () ;
  File_ops.with_file_lock (custom_bakers_lock_file ()) (fun () ->
      let existing = load_entries () in
      let found =
        List.exists (fun e -> String.equal e.instance instance) existing
      in
      if not found then
        Error (Printf.sprintf "custom baker '%s' not found" instance)
      else
        let updated =
          List.filter (fun e -> not (String.equal e.instance instance)) existing
        in
        save_entries updated)

(* ── build_instance_handle ────────────────────────────────── *)

let is_valid_network_char = function
  | 'a' .. 'z' | 'A' .. 'Z' | '0' .. '9' | '_' | '-' | '.' -> true
  | _ -> false

let build_instance_handle ~network ~baker_pkh =
  if String.length network = 0 then Error "network must not be empty"
  else if not (String.for_all is_valid_network_char network) then
    Error
      (Printf.sprintf
         "network '%s' contains invalid characters: only alphanumeric, _, -, . \
          are allowed"
         network)
  else if not (Payout_config.is_valid_baker_pkh baker_pkh) then
    Error
      (Printf.sprintf
         "invalid baker PKH '%s': must be tz1/tz2/tz3/tz4 with 36 characters"
         baker_pkh)
  else
    (* Take the first 8 characters of the PKH as the suffix. *)
    let pkh_prefix = String.sub baker_pkh 0 (min 8 (String.length baker_pkh)) in
    Ok (Printf.sprintf "custom-%s-%s" network pkh_prefix)

(* ── validate_endpoint ────────────────────────────────────── *)

let validate_endpoint s =
  match String.rindex_opt s ':' with
  | None -> Error "endpoint must have the form host:port"
  | Some i -> (
      let host = String.sub s 0 i in
      let port_str = String.sub s (i + 1) (String.length s - i - 1) in
      if String.length host = 0 then Error "endpoint host must not be empty"
      else
        match int_of_string_opt port_str with
        | None ->
            Error
              (Printf.sprintf
                 "endpoint port '%s' is not a valid integer"
                 port_str)
        | Some port ->
            if port < 1 || port > 65535 then
              Error
                (Printf.sprintf
                   "endpoint port %d is out of range (must be 1..65535)"
                   port)
            else Ok ())

(* ── resolve_octez_client_bin ─────────────────────────────── *)

let is_executable path =
  try
    let st = Unix.stat path in
    st.Unix.st_kind = Unix.S_REG
    &&
    (Unix.access path [Unix.X_OK] ;
     true)
  with Unix.Unix_error _ -> false

(** Tier 1: Look in [<registry_root>/rewards/app_bin_dirs] file for a
    recorded App_bin_dir entry, then look for [octez-client] inside it.
    This is a best-effort raw-filesystem alternative to [Directory_registry]
    since [octez_manager_rewards] does not depend on [octez_manager_lib]. *)
let resolve_via_directory_registry () =
  let dirs_file = Filename.concat (Paths.registry_root ()) "directories.json" in
  if not (Sys.file_exists dirs_file) then None
  else
    try
      let json = Yojson.Safe.from_file dirs_file in
      let open Yojson.Safe.Util in
      let entries = json |> member "directories" |> to_list in
      let app_bin_dirs =
        entries
        |> List.filter (fun e ->
            try String.equal (member "dir_type" e |> to_string) "app_bin_dir"
            with _ -> false)
        |> List.filter_map (fun e ->
            try Some (member "path" e |> to_string) with _ -> None)
      in
      (* Try each App_bin_dir, most-recently-used first (order preserved from
         the registry which stores them sorted by last_used_at). *)
      List.find_opt
        (fun dir ->
          let bin = Filename.concat dir "octez-client" in
          is_executable bin)
        app_bin_dirs
      |> Option.map (fun dir -> Filename.concat dir "octez-client")
    with _ -> None

(** Tier 2: Find the newest managed Octez version under
    [$XDG_DATA_HOME/octez-manager/binaries/] and look for [octez-client].
    Uses the same path conventions as [Binary_registry.managed_version_path]. *)
let resolve_via_binary_registry () =
  let binaries_dir =
    Filename.concat (Paths.xdg_data_home ()) "octez-manager/binaries"
  in
  if not (Sys.file_exists binaries_dir) then None
  else
    try
      let entries = Sys.readdir binaries_dir |> Array.to_list in
      let versions =
        entries
        |> List.filter (fun e ->
            String.length e > 1
            && Char.equal e.[0] 'v'
            &&
              try Sys.is_directory (Filename.concat binaries_dir e)
              with _ -> false)
        |> List.map (fun e -> String.sub e 1 (String.length e - 1))
        |> List.sort (fun a b ->
            (* Numeric-aware version comparison: split on '.' and compare parts. *)
            let parse_version v =
              String.split_on_char '.' v |> List.filter_map int_of_string_opt
            in
            let rec cmp la lb =
              match (la, lb) with
              | [], [] -> 0
              | [], _ -> -1
              | _, [] -> 1
              | a :: ra, b :: rb ->
                  let c = Int.compare a b in
                  if c <> 0 then c else cmp ra rb
            in
            cmp (parse_version b) (parse_version a))
        (* newest first *)
      in
      List.find_opt
        (fun version ->
          let bin =
            Filename.concat
              (Filename.concat binaries_dir ("v" ^ version))
              "octez-client"
          in
          is_executable bin)
        versions
      |> Option.map (fun version ->
          Filename.concat
            (Filename.concat binaries_dir ("v" ^ version))
            "octez-client")
    with _ -> None

let resolve_octez_client_bin () =
  match resolve_via_directory_registry () with
  | Some path -> Ok path
  | None -> (
      match resolve_via_binary_registry () with
      | Some path -> Ok path
      | None -> (
          match Paths.which "octez-client" with
          | Some path -> Ok path
          | None ->
              Error
                "octez-client not found: no App_bin_dir entry in directory \
                 registry, no managed Octez version installed, and \
                 octez-client is not on $PATH"))
