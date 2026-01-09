(******************************************************************************)
(*                                                                            *)
(* SPDX-License-Identifier: MIT                                               *)
(* Copyright (c) 2025 Nomadic Labs <contact@nomadic-labs.com>                 *)
(*                                                                            *)
(******************************************************************************)

open Cmdliner
open Octez_manager_lib
open Installer_types
module S = Service

let cmdliner_error msg = `Error (false, msg)

let pp_service fmt svc =
  Format.fprintf
    fmt
    "%-16s %-8s %s (%s)"
    svc.S.instance
    svc.role
    svc.network
    svc.data_dir

let print_services services =
  if services = [] then print_endline "No services registered."
  else List.iter (fun svc -> Format.printf "%a@." pp_service svc) services

let pp_logging fmt = function
  | Logging_mode.Journald -> Format.fprintf fmt "journald"

let print_service_details svc =
  (* Read env file for this instance *)
  let env =
    match Node_env.read ~inst:svc.S.instance with
    | Ok pairs -> pairs
    | Error _ -> []
  in
  let lookup key =
    match List.assoc_opt key env with Some v -> String.trim v | None -> ""
  in
  (* Try to map endpoints to managed instances *)
  let resolve_instance_for_endpoint (ep : string) ~(roles : string list) :
      string option =
    if String.trim ep = "" then None
    else
      match Service_registry.list () with
      | Error _ -> None
      | Ok (services : Service.t list) ->
          let rec find_match (lst : Service.t list) =
            match lst with
            | [] -> None
            | s :: rest ->
                if
                  List.exists (fun r -> String.equal s.role r) roles
                  && String.equal (Installer.endpoint_of_rpc s.rpc_addr) ep
                then Some s.instance
                else find_match rest
          in
          find_match services
  in
  let print_node_endpoint () =
    let node_inst = lookup "OCTEZ_NODE_INSTANCE" in
    if String.trim node_inst <> "" then
      Format.printf "Node instance : %s@." node_inst
    else
      let node_ep = lookup "OCTEZ_NODE_ENDPOINT" in
      Format.printf
        "Node endpoint : %s@."
        (if node_ep = "" then svc.rpc_addr else node_ep)
  in
  (* Common fields *)
  Format.printf "Instance      : %s@." svc.S.instance ;
  Format.printf "Service name  : %s@." (Systemd.unit_name svc.role svc.instance) ;
  Format.printf "Role          : %s@." svc.role ;
  Format.printf "Network       : %s@." svc.network ;
  (* Role-specific fields *)
  (match svc.role with
  | "node" ->
      Format.printf
        "History mode  : %s@."
        (History_mode.to_string svc.history_mode) ;
      Format.printf "Data dir      : %s@." svc.data_dir ;
      Format.printf "RPC addr      : %s@." svc.rpc_addr ;
      Format.printf "P2P addr      : %s@." svc.net_addr
  | "baker" ->
      let base_dir = lookup "OCTEZ_BAKER_BASE_DIR" in
      let dal_cfg = lookup "OCTEZ_DAL_CONFIG" in
      let delegates = lookup "OCTEZ_BAKER_DELEGATES_ARGS" in
      let lb_vote = lookup "OCTEZ_BAKER_LB_VOTE" in
      Format.printf "Base dir      : %s@." base_dir ;
      print_node_endpoint () ;
      (match String.lowercase_ascii (String.trim dal_cfg) with
      | "disabled" -> Format.printf "DAL Config    : opt-out@."
      | "" -> Format.printf "DAL Config    : auto@."
      | raw -> (
          match
            resolve_instance_for_endpoint raw ~roles:["dal-node"; "dal"]
          with
          | Some inst -> Format.printf "DAL instance  : %s@." inst
          | None -> Format.printf "DAL endpoint  : %s@." raw)) ;
      if delegates <> "" then Format.printf "Delegates     : %s@." delegates ;
      if lb_vote <> "" then Format.printf "LB Vote       : %s@." lb_vote
  | "accuser" ->
      let base_dir = lookup "OCTEZ_CLIENT_BASE_DIR" in
      Format.printf "Base dir      : %s@." base_dir ;
      print_node_endpoint ()
  | "dal-node" | "dal" ->
      let dal_data_dir = lookup "OCTEZ_DAL_DATA_DIR" in
      Format.printf "DAL data dir  : %s@." dal_data_dir ;
      print_node_endpoint () ;
      if svc.rpc_addr <> "" then
        Format.printf "DAL RPC addr  : %s@." svc.rpc_addr ;
      if svc.net_addr <> "" then
        Format.printf "DAL P2P addr  : %s@." svc.net_addr
  | _ ->
      (* Fallback for unknown roles *)
      Format.printf "Data dir      : %s@." svc.data_dir ;
      Format.printf "RPC addr      : %s@." svc.rpc_addr ;
      Format.printf "P2P addr      : %s@." svc.net_addr) ;
  (* Common footer *)
  Format.printf "Service user  : %s@." svc.service_user ;
  Format.printf "Octez bin dir : %s@." svc.app_bin_dir ;
  Format.printf "Created at    : %s@." svc.created_at ;
  Format.printf "Logging       : %a@." pp_logging svc.logging_mode ;

  (* Files & Paths Section *)
  Format.printf "@.Files & Paths:@." ;

  let service_paths =
    Systemd.get_service_paths ~role:svc.role ~instance:svc.instance
  in
  List.iter
    (fun (label, path) -> Format.printf "  %-16s: %s@." label path)
    service_paths ;

  Format.printf
    "  %-16s: %s@."
    "Service Metadata"
    (Filename.concat
       (Service_registry.services_dir ())
       (svc.instance ^ ".json")) ;

  (match
     Log_viewer.get_daily_log_file ~role:svc.role ~instance:svc.instance
   with
  | Ok path -> Format.printf "  %-16s: %s@." "Log File" path
  | Error _ -> ()) ;

  match svc.role with
  | "node" ->
      Format.printf
        "  %-16s: %s@."
        "Config File"
        (Filename.concat svc.data_dir "config.json") ;
      Format.printf
        "  %-16s: %s@."
        "Identity File"
        (Filename.concat svc.data_dir "identity.json")
  | "baker" ->
      let base_dir = lookup "OCTEZ_BAKER_BASE_DIR" in
      if base_dir <> "" then (
        Format.printf "  %-16s: %s@." "Base Directory" base_dir ;
        Format.printf
          "  %-16s: %s@."
          "Client Config"
          (Filename.concat base_dir "config"))
  | "accuser" ->
      let base_dir = lookup "OCTEZ_CLIENT_BASE_DIR" in
      if base_dir <> "" then (
        Format.printf "  %-16s: %s@." "Base Directory" base_dir ;
        Format.printf
          "  %-16s: %s@."
          "Client Config"
          (Filename.concat base_dir "config"))
  | "dal-node" | "dal" ->
      let dal_dir = lookup "OCTEZ_DAL_DATA_DIR" in
      if dal_dir <> "" then (
        Format.printf "  %-16s: %s@." "DAL Data Dir" dal_dir ;
        Format.printf
          "  %-16s: %s@."
          "Config File"
          (Filename.concat dal_dir "config.json") ;
        Format.printf
          "  %-16s: %s@."
          "Identity File"
          (Filename.concat dal_dir "identity.json"))
  | _ -> ()

let print_snapshot_entry (entry : Snapshots.entry) =
  Format.printf "%s (%s)@." entry.Snapshots.label entry.slug ;
  (match entry.Snapshots.history_mode with
  | Some hm -> Format.printf "  History mode : %s@." hm
  | None -> ()) ;
  (match entry.Snapshots.download_url with
  | Some url -> Format.printf "  HTTPS        : %s@." url
  | None -> ()) ;
  entry.Snapshots.metadata
  |> List.filter (fun (k, _) -> k <> "History mode" && k <> "HTTPS")
  |> List.iter (fun (k, v) -> Format.printf "  %-12s %s@." (k ^ ":") v) ;
  print_newline ()

let snapshot_entry_to_json (entry : Snapshots.entry) =
  let metadata =
    `List
      (List.map
         (fun (k, v) -> `Assoc [("key", `String k); ("value", `String v)])
         entry.Snapshots.metadata)
  in
  `Assoc
    [
      ("slug", `String entry.slug);
      ("label", `String entry.Snapshots.label);
      ( "history_mode",
        match entry.Snapshots.history_mode with
        | Some hm -> `String hm
        | None -> `Null );
      ( "download_url",
        match entry.Snapshots.download_url with
        | Some url -> `String url
        | None -> `Null );
      ("metadata", metadata);
    ]

let dropin_path_for ~role ~instance =
  let dir =
    if Common.is_root () then
      Printf.sprintf "/etc/systemd/system/octez-%s@%s.service.d" role instance
    else
      let base = Filename.concat (Common.xdg_config_home ()) "systemd/user" in
      Filename.concat
        base
        (Printf.sprintf "octez-%s@%s.service.d" role instance)
  in
  Filename.concat dir "override.conf"

let slurp_file path =
  let ic = open_in_bin path in
  Fun.protect
    ~finally:(fun () -> close_in_noerr ic)
    (fun () -> really_input_string ic (in_channel_length ic))

let resolve_app_bin_dir = function
  | Some dir when String.trim dir <> "" -> (
      match Common.make_absolute_path dir with
      | Ok abs_path -> Ok abs_path
      | Error msg -> Error msg)
  | _ -> (
      match Common.which "octez-node" with
      | Some path -> Ok (Filename.dirname path)
      | None ->
          Error
            "Unable to locate octez-node in PATH. Install Octez binaries or \
             pass --app-bin-dir")

let interactive_tty =
  lazy
    (let fd_isatty fd = try Unix.isatty fd with Unix.Unix_error _ -> false in
     fd_isatty Unix.stdin && fd_isatty Unix.stdout)

let is_interactive () = Lazy.force interactive_tty

let normalize_opt_string = function
  | Some s ->
      let trimmed = String.trim s in
      if String.equal trimmed "" then None else Some trimmed
  | None -> None

let prompt_input ?default question =
  if not (is_interactive ()) then None
  else
    let suffix =
      match default with
      | Some (display, _) when String.trim display <> "" ->
          Printf.sprintf " [%s]" display
      | _ -> ""
    in
    let prompt = Printf.sprintf "%s%s: " question suffix in
    match LNoise.linenoise prompt with
    | exception Sys.Break ->
        prerr_endline "" ;
        exit 130 (* Standard exit code for Ctrl-C *)
    | exception End_of_file -> Option.map snd default
    | None -> Option.map snd default
    | Some line ->
        let trimmed = String.trim line in
        if String.equal trimmed "" then Option.map snd default else Some trimmed

let rec prompt_required_string question =
  match prompt_input question with
  | Some value -> value
  | None ->
      prerr_endline "A value is required." ;
      prompt_required_string question

(* Inline copy of prompt_with_completion placed before prompt_history_mode so it
   can be referenced. This mirrors the main prompt_with_completion later in the
   file. *)
let prompt_with_completion_inline question completions =
  if not (is_interactive ()) then None
  else
    let completions_lower =
      List.map (fun c -> (c, String.lowercase_ascii c)) completions
    in
    LNoise.set_completion_callback (fun line_so_far ln_completions ->
        let prefix = String.lowercase_ascii line_so_far in
        List.iter
          (fun (candidate, candidate_lower) ->
            if String.starts_with ~prefix candidate_lower then
              LNoise.add_completion ln_completions candidate)
          completions_lower) ;
    LNoise.set_hints_callback (fun line_so_far ->
        let prefix = String.lowercase_ascii line_so_far in
        match
          List.find_opt
            (fun (_, candidate_lower) ->
              String.starts_with ~prefix candidate_lower)
            completions_lower
        with
        | Some (hint, _) when String.length hint > String.length line_so_far ->
            Some
              ( String.sub
                  hint
                  (String.length line_so_far)
                  (String.length hint - String.length line_so_far),
                LNoise.Yellow,
                false )
        | _ -> None) ;
    let res =
      match LNoise.linenoise (question ^ ": ") with
      | exception Sys.Break ->
          prerr_endline "" ;
          exit 130
      | None -> None
      | Some line ->
          let trimmed = String.trim line in
          if String.equal trimmed "" then None else Some trimmed
    in
    LNoise.set_completion_callback (fun _ _ -> ()) ;
    LNoise.set_hints_callback (fun _ -> None) ;
    res

let prompt_history_mode default =
  if not (is_interactive ()) then default
  else
    let rec loop () =
      let choices = ["rolling"; "full"; "archive"] in
      match prompt_with_completion_inline "History mode" choices with
      | Some "" | None ->
          prerr_endline "Please enter rolling, full or archive." ;
          loop ()
      | Some raw_value -> (
          let value = String.trim raw_value in
          (* First try direct parse *)
          match History_mode.of_string value with
          | Ok hm -> hm
          | Error _ -> (
              (* If value looks like "full:50" try the prefix before ':' *)
              let prefix_opt =
                try Some (String.sub value 0 (String.index value ':'))
                with Not_found -> None
              in
              match prefix_opt with
              | Some p -> (
                  match History_mode.of_string p with
                  | Ok hm -> hm
                  | Error _ ->
                      prerr_endline "Please enter rolling, full or archive." ;
                      loop ())
              | None ->
                  prerr_endline "Please enter rolling, full or archive." ;
                  loop ()))
    in
    loop ()

let prompt_yes_no question ~default =
  if not (is_interactive ()) then default
  else
    let rec loop () =
      let label = if default then "Y/n" else "y/N" in
      let fallback = if default then "yes" else "no" in
      match prompt_input ~default:(label, fallback) question with
      | Some answer -> (
          match String.lowercase_ascii (String.trim answer) with
          | "y" | "yes" | "true" | "1" -> true
          | "n" | "no" | "false" | "0" -> false
          | _ ->
              prerr_endline "Please answer yes or no." ;
              loop ())
      | None -> loop ()
    in
    loop ()

(* Prompt with linenoise for autocompletion support *)
let prompt_with_completion question completions =
  if not (is_interactive ()) then None
  else
    (* Pre-compute lowercase versions for efficient matching *)
    let completions_lower =
      List.map (fun c -> (c, String.lowercase_ascii c)) completions
    in
    (* Set up completions *)
    LNoise.set_completion_callback (fun line_so_far ln_completions ->
        let prefix = String.lowercase_ascii line_so_far in
        List.iter
          (fun (candidate, candidate_lower) ->
            if String.starts_with ~prefix candidate_lower then
              LNoise.add_completion ln_completions candidate)
          completions_lower) ;
    (* Set hints *)
    LNoise.set_hints_callback (fun line_so_far ->
        let prefix = String.lowercase_ascii line_so_far in
        match
          List.find_opt
            (fun (_, candidate_lower) ->
              String.starts_with ~prefix candidate_lower)
            completions_lower
        with
        | Some (hint, _) when String.length hint > String.length line_so_far ->
            Some
              ( String.sub
                  hint
                  (String.length line_so_far)
                  (String.length hint - String.length line_so_far),
                LNoise.Yellow,
                false )
        | _ -> None) ;
    (* Read a line, then clear callbacks to avoid leaking completions to later prompts *)
    let res =
      match LNoise.linenoise (question ^ ": ") with
      | exception Sys.Break ->
          prerr_endline "" ;
          exit 130
      | None -> None
      | Some line ->
          let trimmed = String.trim line in
          if String.equal trimmed "" then None else Some trimmed
    in
    (* Clear callbacks *)
    LNoise.set_completion_callback (fun _ _ -> ()) ;
    LNoise.set_hints_callback (fun _ -> None) ;
    res

(** Validate a port address, re-prompting in interactive mode if invalid.
    @param label Label for error messages (e.g., "RPC address")
    @param addr The address to validate
    @param default Default address if none provided (also used as example in error messages)
    @param exclude_instance Instance to exclude from "in use" checks (for edit mode)
    @return Ok addr if valid, Error msg if invalid in non-interactive mode *)
let rec validate_port_addr ~label ~addr ~default ?exclude_instance () =
  let validate a =
    Port_validation.validate_addr ~addr:a ?exclude_instance ~example:default ()
  in
  match validate addr with
  | Ok () -> Ok addr
  | Error err ->
      if is_interactive () then (
        Printf.eprintf "%s: %s\n%!" label (Port_validation.pp_error err) ;
        let new_addr =
          prompt_input ~default:(default, default) label
          |> Option.value ~default
        in
        validate_port_addr ~label ~addr:new_addr ~default ?exclude_instance ())
      else Error (Printf.sprintf "%s: %s" label (Port_validation.pp_error err))

let rec resolve_node_instance_or_endpoint ~node_instance =
  let ( let* ) = Result.bind in
  let* services = Service_registry.list () in
  let node_services =
    List.filter (fun (svc : Service.t) -> String.equal svc.role "node") services
  in
  let default = "127.0.0.1:8732" in
  let choice =
    match node_instance with
    | Some ni -> Some ni
    | None ->
        if not (is_interactive ()) then Some default
        else
          let instance_names =
            List.map (fun (svc : Service.t) -> svc.instance) node_services
          in
          (if node_services = [] then
             prerr_endline
               "No node instances found. You can specify a custom endpoint."
           else
             let instance_map =
               List.map
                 (fun (svc : Service.t) -> (svc.instance, svc.network))
                 node_services
             in
             Format.printf
               "Available node instances: %s@."
               (String.concat
                  ", "
                  (List.map
                     (fun (inst, net) -> Printf.sprintf "%s (%s)" inst net)
                     instance_map))) ;
          prompt_with_completion "Node instance" (default :: instance_names)
  in
  match choice with
  | None ->
      prerr_endline "Enter a node instance or custom endpoint" ;
      resolve_node_instance_or_endpoint ~node_instance
  | Some choice ->
      if
        List.exists
          (fun (svc : Service.t) -> String.equal svc.instance choice)
          node_services
      then Ok (`Instance choice)
      else Ok (`Endpoint choice)

let run_result = function
  | Ok () -> `Ok ()
  | Error (`Msg msg) -> cmdliner_error msg

(* Logging is always via journald - octez binaries handle their own file logging *)
let logging_mode_term = Term.(const Logging_mode.default)

let history_mode_doc =
  "History mode to configure on octez-node (rolling|full|archive)."

let history_mode_choices =
  [
    ("rolling", History_mode.Rolling);
    ("full", History_mode.Full);
    ("archive", History_mode.Archive);
  ]

let history_mode_opt_term =
  Arg.(
    value
    & opt (some (enum history_mode_choices)) None
    & info ["history-mode"] ~doc:history_mode_doc ~docv:"MODE")

let install_node_cmd =
  let instance =
    let doc = "Instance name used for node.env and systemd units." in
    Arg.(value & opt (some string) None & info ["instance"] ~doc ~docv:"NAME")
  in
  let network =
    let doc = "Chain network (default: mainnet)." in
    Arg.(value & opt (some string) None & info ["network"] ~doc ~docv:"NET")
  in
  let data_dir =
    let doc = "Custom data directory (defaults to /var/lib/octez/<inst>)." in
    Arg.(value & opt (some string) None & info ["data-dir"] ~doc ~docv:"DIR")
  in
  let rpc_addr =
    Arg.(
      value
      & opt string "127.0.0.1:8732"
      & info ["rpc-addr"] ~doc:"RPC address" ~docv:"ADDR")
  in
  let net_addr =
    Arg.(
      value & opt string "0.0.0.0:9732"
      & info ["net-addr"] ~doc:"P2P address" ~docv:"ADDR")
  in
  let service_user =
    let default_user =
      if Common.is_root () then "octez"
      else fst (Common.current_user_group_names ())
    in
    let doc = "System user owning the service." in
    Arg.(
      value & opt string default_user & info ["service-user"] ~doc ~docv:"USER")
  in
  let app_bin_dir =
    let doc =
      {|Directory containing Octez binaries (defaults to the directory holding octez-node found in \$PATH).|}
    in
    Arg.(value & opt (some string) None & info ["app-bin-dir"] ~doc ~docv:"DIR")
  in
  let extra_args =
    let doc = "Additional arguments appended to the node command." in
    Arg.(value & opt_all string [] & info ["extra-arg"] ~doc ~docv:"ARG")
  in
  let snapshot_flag =
    let doc =
      "Bootstrap by importing a snapshot before enabling the node service."
    in
    Arg.(value & flag & info ["snapshot"] ~doc)
  in
  let snapshot_uri =
    let doc =
      "Snapshot URI (path, file://, or http(s)) to import when --snapshot is \
       set."
    in
    Arg.(
      value & opt (some string) None & info ["snapshot-uri"] ~doc ~docv:"URI")
  in
  let snapshot_no_check =
    let doc =
      "Pass --no-check to octez-node snapshot import during bootstrap."
    in
    Arg.(value & flag & info ["snapshot-no-check"] ~doc)
  in
  let auto_enable =
    let doc = "Disable automatic systemctl enable --now." in
    Arg.(value & flag & info ["no-enable"] ~doc)
  in
  let preserve_data =
    let doc =
      "Preserve existing data in data-dir instead of clearing it. When set, no \
       snapshot will be imported even if --snapshot is specified."
    in
    Arg.(value & flag & info ["preserve-data"] ~doc)
  in
  let make instance_opt network_opt history_mode_opt data_dir rpc_addr net_addr
      service_user app_bin_dir extra_args snapshot_flag snapshot_uri
      snapshot_no_check no_enable preserve_data logging_mode =
    let res =
      let ( let* ) = Result.bind in
      let* app_bin_dir = resolve_app_bin_dir app_bin_dir in
      (* When preserve_data is set, require data_dir to be specified *)
      let* data_dir =
        match (preserve_data, data_dir) with
        | true, None ->
            if is_interactive () then
              Ok (Some (prompt_required_string "Data directory to preserve"))
            else
              Error
                "--data-dir is required when using --preserve-data in \
                 non-interactive mode"
        | _, dir -> Ok dir
      in
      let* data_dir_config =
        match data_dir with
        | None -> Ok None
        | Some data_dir ->
            let* r = Installer.resolve_from_data_dir data_dir in
            Ok (Some r)
      in
      (* When preserving data, config.json must exist in the data directory *)
      let* () =
        match (preserve_data, data_dir_config) with
        | true, Some (`Path dir) ->
            Error
              (Printf.sprintf
                 "Cannot preserve data: no config.json found in '%s'"
                 dir)
        | _ -> Ok ()
      in
      let instance () =
        match normalize_opt_string instance_opt with
        | Some inst -> Ok inst
        | None ->
            if is_interactive () then
              Ok (prompt_required_string "Instance name")
            else Error "Instance name is required in non-interactive mode"
      in
      match data_dir_config with
      | Some
          (`Data_dir
             {
               network;
               history_mode;
               rpc_addr = config_rpc_addr;
               net_addr = config_net_addr;
             }) ->
          let* instance = instance () in
          let* () =
            match (history_mode, history_mode_opt) with
            | history_mode, Some history_mode'
              when history_mode <> history_mode' ->
                Error
                  (Format.asprintf
                     "History mode found in the configuration incompatible \
                      with the arguments: %a <> %a"
                     History_mode.pp
                     history_mode
                     History_mode.pp
                     history_mode')
            | _ -> Ok ()
          in
          let* () =
            match (network, network_opt) with
            | network, Some network' when network <> network' ->
                Error
                  (Format.asprintf
                     "Network found in the configuration incompatible with the \
                      arguments: %s <> %s"
                     network
                     network')
            | _ -> Ok ()
          in
          let rpc_addr =
            if rpc_addr <> "127.0.0.1:8732" then rpc_addr else config_rpc_addr
          in
          let net_addr =
            if net_addr <> "0.0.0.0:9732" then net_addr else config_net_addr
          in
          (* Validate ports - exclude self in edit/preserve-data mode *)
          let* rpc_addr =
            validate_port_addr
              ~label:"RPC address"
              ~addr:rpc_addr
              ~default:"127.0.0.1:8732"
              ~exclude_instance:instance
              ()
          in
          let* net_addr =
            validate_port_addr
              ~label:"P2P address"
              ~addr:net_addr
              ~default:"0.0.0.0:9732"
              ~exclude_instance:instance
              ()
          in
          let* () =
            if snapshot_flag || Option.is_some snapshot_uri then
              Error "Snapshot cannot be imported, the data-dir already exist"
            else Ok ()
          in
          let req : node_request =
            {
              instance;
              network;
              history_mode;
              data_dir;
              rpc_addr;
              net_addr;
              service_user;
              app_bin_dir;
              extra_args;
              auto_enable = not no_enable;
              logging_mode;
              bootstrap = Genesis;
              preserve_data;
              snapshot_no_check;
            }
          in
          Result.map_error (fun (`Msg s) -> s) @@ Installer.install_node req
      | Some (`Path _) | None ->
          let* instance = instance () in
          let* network =
            match normalize_opt_string network_opt with
            | Some net -> Ok net
            | None ->
                if is_interactive () then
                  match Teztnets.list_networks () with
                  | Ok infos ->
                      let aliases =
                        List.map (fun Teztnets.{alias; _} -> alias) infos
                      in
                      let rec loop () =
                        match prompt_with_completion "Network" aliases with
                        | Some sel -> sel
                        | None ->
                            prerr_endline "Please enter a network." ;
                            loop ()
                      in
                      Ok (loop ())
                  | Error (`Msg err) -> Error err
                else Ok "mainnet"
          in
          let history_mode =
            match history_mode_opt with
            | Some hm -> hm
            | None -> prompt_history_mode History_mode.default
          in
          let snapshot_requested_initial =
            snapshot_flag || Option.is_some snapshot_uri
          in
          let snapshot_requested =
            if snapshot_requested_initial then true
            else if is_interactive () then
              prompt_yes_no
                "Download and import a tzinit snapshot before starting?"
                ~default:true
            else false
          in
          let snapshot_uri = normalize_opt_string snapshot_uri in
          let snapshot_requested =
            snapshot_requested || Option.is_some snapshot_uri
          in
          let bootstrap =
            if preserve_data then Genesis
            else if snapshot_requested then Snapshot {src = snapshot_uri}
            else Genesis
          in
          (* Validate ports for new install *)
          let* rpc_addr =
            validate_port_addr
              ~label:"RPC address"
              ~addr:rpc_addr
              ~default:"127.0.0.1:8732"
              ()
          in
          let* net_addr =
            validate_port_addr
              ~label:"P2P address"
              ~addr:net_addr
              ~default:"0.0.0.0:9732"
              ()
          in
          let req : node_request =
            {
              instance;
              network;
              history_mode;
              data_dir;
              rpc_addr;
              net_addr;
              service_user;
              app_bin_dir;
              extra_args;
              auto_enable = not no_enable;
              logging_mode;
              bootstrap;
              preserve_data;
              snapshot_no_check;
            }
          in
          Result.map_error (fun (`Msg s) -> s) @@ Installer.install_node req
    in
    match res with
    | Ok service ->
        Format.printf "Installed %s (%s)\n" service.S.instance service.network ;
        `Ok ()
    | Error msg -> cmdliner_error msg
  in
  let term =
    Term.(
      ret
        (const make $ instance $ network $ history_mode_opt_term $ data_dir
       $ rpc_addr $ net_addr $ service_user $ app_bin_dir $ extra_args
       $ snapshot_flag $ snapshot_uri $ snapshot_no_check $ auto_enable
       $ preserve_data $ logging_mode_term))
  in
  let info =
    Cmd.info "install-node" ~doc:"Install an octez-node systemd instance"
  in
  Cmd.v info term

let install_baker_cmd =
  let instance =
    let doc = "Instance name for the baker systemd unit." in
    Arg.(value & opt (some string) None & info ["instance"] ~doc ~docv:"NAME")
  in
  let node_instance =
    let doc =
      "Existing octez-manager node instance to reuse for data-dir and network. \
       Use 'octez-manager list' to see available node instances. It can also \
       be a custom RPC endpoint for the baker to contact. Defaults to \
       http://127.0.0.1:8732"
    in
    Arg.(
      value & opt (some string) None & info ["node-instance"] ~doc ~docv:"NODE")
  in
  let base_dir =
    let doc =
      "Baker base directory for wallets (defaults to an instance-specific \
       path)."
    in
    Arg.(value & opt (some string) None & info ["base-dir"] ~doc ~docv:"DIR")
  in
  let delegates =
    let doc = "Delegate key hash or alias passed as --delegate." in
    Arg.(value & opt_all string [] & info ["delegate"] ~doc ~docv:"KEY")
  in
  let dal_endpoint =
    let doc =
      "DAL node endpoint (e.g., http://localhost:10732). Use 'none' to opt-out \
       with --without-dal flag. Defaults to 'none'."
    in
    Arg.(
      value
      & opt (some string) None
      & info ["dal-endpoint"] ~doc ~docv:"ENDPOINT")
  in
  let liquidity_baking_vote =
    let doc =
      "Liquidity baking toggle vote (on, off or pass). Defaults to 'pass'."
    in
    Arg.(
      value
      & opt (some string) None
      & info ["liquidity-baking-vote"] ~doc ~docv:"VOTE")
  in
  let extra_args =
    let doc = "Additional arguments appended to the baker command." in
    Arg.(value & opt_all string [] & info ["extra-arg"] ~doc ~docv:"ARG")
  in
  let default_user =
    if Common.is_root () then "octez"
    else fst (Common.current_user_group_names ())
  in
  let service_user =
    Arg.(
      value & opt string default_user
      & info ["service-user"] ~doc:"System user owning the service" ~docv:"USER")
  in
  let app_bin_dir =
    let doc = "Directory containing Octez binaries." in
    Arg.(value & opt (some string) None & info ["app-bin-dir"] ~doc ~docv:"DIR")
  in
  let auto_enable =
    Arg.(
      value & flag
      & info ["no-enable"] ~doc:"Disable automatic systemctl enable --now")
  in
  let make instance_opt node_instance base_dir delegates dal_endpoint_opt
      liquidity_baking_vote_opt extra_args service_user app_bin_dir no_enable
      logging_mode =
    let res =
      let ( let* ) = Result.bind in
      let* app_bin_dir = resolve_app_bin_dir app_bin_dir in
      let* instance =
        match normalize_opt_string instance_opt with
        | Some inst -> Ok inst
        | None ->
            if is_interactive () then
              Ok (prompt_required_string "Instance name")
            else Error "Instance name is required in non-interactive mode"
      in
      let* choice =
        Result.map_error (fun (`Msg s) -> s)
        @@ resolve_node_instance_or_endpoint ~node_instance
      in
      let node_mode =
        match choice with
        | `Instance ins -> Local_instance ins
        | `Endpoint endpoint -> Remote_endpoint endpoint
      in
      let* liquidity_baking_vote =
        match normalize_opt_string liquidity_baking_vote_opt with
        | Some vote -> Ok (Some vote)
        | None ->
            if is_interactive () then
              let completions = ["on"; "off"; "pass"] in
              let rec ask () =
                match
                  prompt_with_completion "Liquidity baking vote" completions
                with
                | Some v -> Ok (Some v)
                | None ->
                    prerr_endline "Please choose 'on', 'off', or 'pass'." ;
                    ask ()
              in
              ask ()
            else
              Error "Liquidity baking vote is required in non-interactive mode"
      in
      (* Prompt for dal_endpoint if not provided in interactive mode *)
      (* Track both DAL config and DAL node instance name *)
      let* dal_config, dal_node =
        match normalize_opt_string dal_endpoint_opt with
        | Some ep ->
            let normalized = String.lowercase_ascii (String.trim ep) in
            if normalized = "none" then Ok (Dal_disabled, None)
            else Ok (Dal_endpoint ep, None)
        | None ->
            if is_interactive () then
              (* Get list of available DAL node instances *)
              match Service_registry.list () with
              | Error (`Msg msg) ->
                  prerr_endline ("Warning: Could not load services: " ^ msg) ;
                  Ok (Dal_disabled, None)
              | Ok services ->
                  let dal_services =
                    List.filter
                      (fun (svc : Service.t) ->
                        let role_lower = String.lowercase_ascii svc.role in
                        String.equal role_lower "dal-node"
                        || String.equal role_lower "dal")
                      services
                  in
                  if dal_services = [] then
                    let choice =
                      prompt_with_completion_inline "DAL Node endpoint" ["none"]
                      |> Option.map (fun choice ->
                          String.lowercase_ascii @@ String.trim choice)
                    in
                    match choice with
                    | Some "" | Some "none" | None -> Ok (Dal_disabled, None)
                    | Some endpoint -> Ok (Dal_endpoint endpoint, None)
                  else
                    let rec loop () =
                      let instance_names =
                        List.map
                          (fun (svc : Service.t) -> svc.instance)
                          dal_services
                      in
                      let instance_map =
                        List.map
                          (fun (svc : Service.t) ->
                            (svc.instance, svc.rpc_addr))
                          dal_services
                      in
                      Format.printf
                        "Available DAL node instances: %s@."
                        (String.concat
                           ", "
                           (List.map
                              (fun (inst, addr) ->
                                Printf.sprintf "%s (%s)" inst addr)
                              instance_map)) ;
                      match
                        prompt_with_completion
                          "DAL node instance"
                          ("none" :: instance_names)
                      with
                      | Some "" | None -> loop ()
                      | Some "none" -> Ok (Dal_disabled, None)
                      | Some selected -> (
                          (* Check if input matches existing DAL instance name, otherwise treat as endpoint *)
                          match
                            List.find_opt
                              (fun (svc : Service.t) ->
                                String.equal svc.instance selected)
                              dal_services
                          with
                          | Some svc ->
                              Ok
                                ( Dal_endpoint
                                    (Installer.endpoint_of_rpc
                                       svc.Service.rpc_addr),
                                  Some svc.instance )
                          | None ->
                              Ok
                                ( Dal_endpoint
                                    (Installer.endpoint_of_rpc selected),
                                  None ))
                    in
                    loop ()
            else Ok (Dal_disabled, None)
      in
      let req : baker_request =
        {
          instance;
          node_mode;
          base_dir;
          delegates;
          dal_config;
          dal_node;
          liquidity_baking_vote;
          extra_args;
          service_user;
          app_bin_dir;
          logging_mode;
          auto_enable = not no_enable;
          preserve_data = false;
        }
      in
      (* Installer.install_baker returns an Rresult-style error; convert it to a string-error Result *)
      match Installer.install_baker req with
      | Ok service ->
          Format.printf "Installed %s (%s)\n" service.S.instance service.network ;
          Ok ()
      | Error (`Msg s) -> Error s
    in
    match res with Ok () -> `Ok () | Error msg -> cmdliner_error msg
  in
  let term =
    Term.(
      ret
        (const make $ instance $ node_instance $ base_dir $ delegates
       $ dal_endpoint $ liquidity_baking_vote $ extra_args $ service_user
       $ app_bin_dir $ auto_enable $ logging_mode_term))
  in
  let info = Cmd.info "install-baker" ~doc:"Install an octez-baker service" in
  Cmd.v info term

let install_accuser_cmd =
  let instance =
    let doc = "Accuser instance name" in
    Arg.(value & opt (some string) None & info ["instance"] ~doc ~docv:"NAME")
  in
  let node_instance =
    let doc =
      "Existing octez-manager node instance to reuse for endpoint; can also be \
       a custom RPC endpoint"
    in
    Arg.(
      value & opt (some string) None & info ["node-instance"] ~doc ~docv:"NODE")
  in
  let base_dir =
    Arg.(
      value
      & opt (some string) None
      & info ["base-dir"] ~doc:"Accuser base directory" ~docv:"DIR")
  in
  let extra_args =
    Arg.(
      value & opt_all string []
      & info
          ["extra-arg"]
          ~doc:"Additional arguments appended to the accuser command."
          ~docv:"ARG")
  in
  let default_user =
    if Common.is_root () then "octez"
    else fst (Common.current_user_group_names ())
  in
  let service_user =
    Arg.(
      value & opt string default_user
      & info ["service-user"] ~doc:"System user" ~docv:"USER")
  in
  let app_bin_dir =
    Arg.(
      value
      & opt (some string) None
      & info
          ["app-bin-dir"]
          ~doc:"Directory containing Octez binaries"
          ~docv:"DIR")
  in
  let auto_enable =
    Arg.(
      value & flag & info ["no-enable"] ~doc:"Disable automatic enable --now")
  in
  let make instance_opt node_instance base_dir extra_args service_user
      app_bin_dir no_enable logging_mode =
    let res =
      let ( let* ) = Result.bind in
      let* app_bin_dir = resolve_app_bin_dir app_bin_dir in
      let* instance =
        match normalize_opt_string instance_opt with
        | Some inst -> Ok inst
        | None ->
            if is_interactive () then
              Ok (prompt_required_string "Instance name")
            else Error "Instance name is required in non-interactive mode"
      in
      let* choice =
        Result.map_error (fun (`Msg s) -> s)
        @@ resolve_node_instance_or_endpoint ~node_instance
      in
      let node_mode =
        match choice with
        | `Instance ins -> Local_instance ins
        | `Endpoint endpoint -> Remote_endpoint endpoint
      in
      let req : accuser_request =
        {
          instance;
          app_bin_dir;
          node_mode;
          base_dir;
          extra_args;
          service_user;
          logging_mode;
          auto_enable = not no_enable;
          preserve_data = false;
        }
      in
      match Installer.install_accuser req with
      | Ok svc -> Ok svc
      | Error (`Msg msg) -> Error msg
    in
    match res with
    | Ok service ->
        Format.printf "Installed  %s (%s)\n" service.S.instance service.network ;
        `Ok ()
    | Error msg -> cmdliner_error msg
  in
  let term =
    Term.(
      ret
        (const make $ instance $ node_instance $ base_dir $ extra_args
       $ service_user $ app_bin_dir $ auto_enable $ logging_mode_term))
  in
  let info =
    Cmd.info "install-accuser" ~doc:"Install an octez-accuser service"
  in
  Cmd.v info term

let install_dal_node_cmd =
  let instance =
    let doc = "Instance name used for dal-node.env and systemd units." in
    Arg.(value & opt (some string) None & info ["instance"] ~doc ~docv:"NAME")
  in
  let data_dir_opt =
    Arg.(
      value
      & opt (some string) None
      & info ["data-dir"] ~doc:"DAL node data directory" ~docv:"DIR")
  in
  let rpc_addr =
    Arg.(
      value
      & opt string "127.0.0.1:10732"
      & info ["rpc-addr"] ~doc:"DAL node RPC address" ~docv:"ADDR")
  in
  let net_addr =
    Arg.(
      value & opt string "0.0.0.0:11732"
      & info ["net-addr"] ~doc:"DAL node P2P address" ~docv:"ADDR")
  in
  let node_instance =
    let doc =
      "Existing octez-manager node instance to reuse for network resolution. \
       It can also be a custom RPC endpoint for the DAL node to contact."
    in
    Arg.(
      value & opt (some string) None & info ["node-instance"] ~doc ~docv:"NODE")
  in
  let extra_args =
    Arg.(
      value & opt_all string []
      & info
          ["extra-arg"]
          ~doc:"Additional arguments appended to the dal-node command."
          ~docv:"ARG")
  in
  let default_user =
    if Common.is_root () then "octez"
    else fst (Common.current_user_group_names ())
  in
  let service_user =
    Arg.(
      value & opt string default_user
      & info ["service-user"] ~doc:"System user" ~docv:"USER")
  in
  let app_bin_dir =
    Arg.(
      value
      & opt (some string) None
      & info
          ["app-bin-dir"]
          ~doc:"Directory containing Octez binaries"
          ~docv:"DIR")
  in
  let auto_enable =
    Arg.(
      value & flag & info ["no-enable"] ~doc:"Disable automatic enable --now")
  in
  let make instance_opt data_dir_opt rpc_addr net_addr node_instance extra_args
      service_user app_bin_dir no_enable logging_mode =
    match resolve_app_bin_dir app_bin_dir with
    | Error msg -> cmdliner_error msg
    | Ok app_bin_dir -> (
        let instance_result =
          match normalize_opt_string instance_opt with
          | Some inst -> Ok inst
          | None ->
              if is_interactive () then
                Ok (prompt_required_string "Instance name")
              else Error "Instance name is required in non-interactive mode"
        in
        match instance_result with
        | Error msg -> cmdliner_error msg
        | Ok instance -> (
            let data_dir =
              match data_dir_opt with
              | Some dir when String.trim dir <> "" -> dir
              | _ -> Common.default_role_dir "dal-node" instance
            in
            match resolve_node_instance_or_endpoint ~node_instance with
            | Error (`Msg msg) -> cmdliner_error msg
            | Ok node_mode -> (
                let node_endpoint =
                  match node_mode with
                  | `Endpoint ep -> Installer.endpoint_of_rpc ep
                  | `Instance inst -> (
                      match Service_registry.find ~instance:inst with
                      | Ok (Some svc) ->
                          Installer.endpoint_of_rpc svc.Service.rpc_addr
                      | _ -> Installer.endpoint_of_rpc "127.0.0.1:8732")
                in
                let maybe_network =
                  match node_mode with
                  | `Instance inst -> (
                      match Service_registry.find ~instance:inst with
                      | Ok (Some svc) -> Ok svc.Service.network
                      | _ ->
                          Teztnets.resolve_octez_node_chain
                            ~endpoint:node_endpoint)
                  | `Endpoint _ ->
                      Teztnets.resolve_octez_node_chain ~endpoint:node_endpoint
                in
                match maybe_network with
                | Error (`Msg msg) -> cmdliner_error msg
                | Ok network -> (
                    let depends_on =
                      match node_mode with
                      | `Instance inst -> Some inst
                      | _ -> None
                    in
                    (* Validate DAL node ports *)
                    match
                      validate_port_addr
                        ~label:"DAL RPC address"
                        ~addr:rpc_addr
                        ~default:"127.0.0.1:10732"
                        ()
                    with
                    | Error msg -> cmdliner_error msg
                    | Ok rpc_addr -> (
                        match
                          validate_port_addr
                            ~label:"DAL P2P address"
                            ~addr:net_addr
                            ~default:"0.0.0.0:11732"
                            ()
                        with
                        | Error msg -> cmdliner_error msg
                        | Ok net_addr -> (
                            let req : daemon_request =
                              {
                                role = "dal-node";
                                instance;
                                network;
                                history_mode = History_mode.default;
                                data_dir;
                                rpc_addr;
                                net_addr;
                                service_user;
                                app_bin_dir;
                                logging_mode;
                                service_args = extra_args;
                                extra_env =
                                  [
                                    ("OCTEZ_NODE_ENDPOINT", node_endpoint);
                                    ("OCTEZ_DAL_DATA_DIR", data_dir);
                                    ("OCTEZ_DAL_RPC_ADDR", rpc_addr);
                                    ("OCTEZ_DAL_NET_ADDR", net_addr);
                                  ];
                                extra_paths = [];
                                auto_enable = not no_enable;
                                depends_on;
                                preserve_data = false;
                              }
                            in
                            match Installer.install_daemon req with
                            | Ok service ->
                                Format.printf
                                  "Installed %s (%s)\n"
                                  service.S.instance
                                  service.network ;
                                `Ok ()
                            | Error (`Msg msg) -> cmdliner_error msg))))))
  in
  let term =
    Term.(
      ret
        (const make $ instance $ data_dir_opt $ rpc_addr $ net_addr
       $ node_instance $ extra_args $ service_user $ app_bin_dir $ auto_enable
       $ logging_mode_term))
  in
  let info =
    Cmd.info "install-dal-node" ~doc:"Install an octez-dal-node service"
  in
  Cmd.v info term

type instance_action =
  | Start
  | Stop
  | Restart
  | Remove
  | Purge
  | Show
  | Show_service
  | Logs
  | Edit

let instance_term =
  let instance =
    Arg.(value & pos 0 (some string) None & info [] ~docv:"INSTANCE")
  in
  let action =
    let actions =
      [
        ("start", Start);
        ("stop", Stop);
        ("restart", Restart);
        ("remove", Remove);
        ("purge", Purge);
        ("show", Show);
        ("show-service", Show_service);
        ("logs", Logs);
        ("edit", Edit);
      ]
    in
    Arg.(value & pos 1 (some (enum actions)) None & info [] ~docv:"ACTION")
  in
  let delete_data_dir =
    Arg.(
      value & flag
      & info
          ["delete-data-dir"]
          ~doc:"Also delete the recorded data directory when removing.")
  in
  let run instance action delete_data_dir =
    match (instance, action) with
    | None, _ -> `Help (`Pager, None)
    | Some _, None ->
        cmdliner_error
          "ACTION required \
           (start|stop|restart|remove|purge|show|show-service|logs|edit)"
    | Some inst, Some action -> (
        match action with
        | Start -> (
            (* Check for stopped dependencies *)
            let dep_check =
              Installer.get_stopped_dependencies ~instance:inst ()
            in
            match dep_check with
            | Error (`Msg e) ->
                prerr_endline ("Error checking dependencies: " ^ e) ;
                `Error (false, e)
            | Ok [] -> (
                (* No stopped dependencies, start directly *)
                let result =
                  Installer.start_service ~quiet:false ~instance:inst ()
                in
                match result with
                | Ok () -> (
                    (* Check for stopped dependents *)
                    match
                      Installer.get_stopped_dependents ~instance:inst ()
                    with
                    | Ok [] -> `Ok ()
                    | Ok stopped_deps when is_interactive () ->
                        let names =
                          String.concat
                            ", "
                            (List.map
                               (fun s -> s.Service.instance)
                               stopped_deps)
                        in
                        let should_start =
                          prompt_yes_no
                            (Printf.sprintf "Start dependents? (%s)" names)
                            ~default:true
                        in
                        if should_start then
                          List.iter
                            (fun dep ->
                              match
                                Installer.start_service
                                  ~quiet:false
                                  ~instance:dep.Service.instance
                                  ()
                              with
                              | Ok () ->
                                  Printf.printf
                                    "Started %s\n%!"
                                    dep.Service.instance
                              | Error (`Msg e) ->
                                  Printf.eprintf
                                    "Failed to start %s: %s\n%!"
                                    dep.Service.instance
                                    e)
                            stopped_deps ;
                        `Ok ()
                    | _ -> `Ok ())
                | Error (`Msg e) -> `Error (false, e))
            | Ok stopped_deps ->
                let names =
                  String.concat
                    ", "
                    (List.map (fun s -> s.Service.instance) stopped_deps)
                in
                if is_interactive () then
                  let should_start_deps =
                    prompt_yes_no
                      (Printf.sprintf
                         "Dependencies not running (%s). Start them first?"
                         names)
                      ~default:true
                  in
                  if should_start_deps then (
                    (* Start dependencies in order *)
                    let failed = ref false in
                    List.iter
                      (fun dep ->
                        if not !failed then
                          match
                            Installer.start_service
                              ~quiet:false
                              ~instance:dep.Service.instance
                              ()
                          with
                          | Ok () ->
                              Printf.printf
                                "Started %s\n%!"
                                dep.Service.instance
                          | Error (`Msg e) ->
                              Printf.eprintf
                                "Failed to start %s: %s\n%!"
                                dep.Service.instance
                                e ;
                              failed := true)
                      stopped_deps ;
                    if !failed then
                      `Error (false, "Failed to start dependencies")
                    else
                      (* Now start the target instance *)
                      let result =
                        Installer.start_service ~quiet:false ~instance:inst ()
                      in
                      match result with
                      | Ok () -> (
                          (* Check for stopped dependents *)
                          match
                            Installer.get_stopped_dependents ~instance:inst ()
                          with
                          | Ok [] -> `Ok ()
                          | Ok stopped_deps ->
                              let names =
                                String.concat
                                  ", "
                                  (List.map
                                     (fun s -> s.Service.instance)
                                     stopped_deps)
                              in
                              let should_start =
                                prompt_yes_no
                                  (Printf.sprintf
                                     "Start dependents? (%s)"
                                     names)
                                  ~default:true
                              in
                              if should_start then
                                List.iter
                                  (fun dep ->
                                    match
                                      Installer.start_service
                                        ~quiet:false
                                        ~instance:dep.Service.instance
                                        ()
                                    with
                                    | Ok () ->
                                        Printf.printf
                                          "Started %s\n%!"
                                          dep.Service.instance
                                    | Error (`Msg e) ->
                                        Printf.eprintf
                                          "Failed to start %s: %s\n%!"
                                          dep.Service.instance
                                          e)
                                  stopped_deps ;
                              `Ok ()
                          | _ -> `Ok ())
                      | Error (`Msg e) -> `Error (false, e))
                  else (
                    prerr_endline
                      "Cancelled - dependencies must be running first." ;
                    `Ok ())
                else
                  (* Non-interactive mode - just fail like before *)
                  run_result
                    (Installer.start_service ~quiet:false ~instance:inst ()))
        | Stop ->
            run_result (Installer.stop_service ~quiet:false ~instance:inst ())
        | Restart -> (
            (* Check for stopped dependencies *)
            let dep_check =
              Installer.get_stopped_dependencies ~instance:inst ()
            in
            match dep_check with
            | Error (`Msg e) ->
                prerr_endline ("Error checking dependencies: " ^ e) ;
                `Error (false, e)
            | Ok [] -> (
                (* No stopped dependencies, restart directly *)
                let result =
                  Installer.restart_service ~quiet:false ~instance:inst ()
                in
                match result with
                | Ok () -> (
                    (* Check for stopped dependents *)
                    match
                      Installer.get_stopped_dependents ~instance:inst ()
                    with
                    | Ok [] -> `Ok ()
                    | Ok stopped_deps when is_interactive () ->
                        let names =
                          String.concat
                            ", "
                            (List.map
                               (fun s -> s.Service.instance)
                               stopped_deps)
                        in
                        let should_restart =
                          prompt_yes_no
                            (Printf.sprintf "Restart dependents? (%s)" names)
                            ~default:true
                        in
                        if should_restart then
                          List.iter
                            (fun dep ->
                              match
                                Installer.restart_service
                                  ~quiet:false
                                  ~instance:dep.Service.instance
                                  ()
                              with
                              | Ok () ->
                                  Printf.printf
                                    "Restarted %s\n%!"
                                    dep.Service.instance
                              | Error (`Msg e) ->
                                  Printf.eprintf
                                    "Failed to restart %s: %s\n%!"
                                    dep.Service.instance
                                    e)
                            stopped_deps ;
                        `Ok ()
                    | _ -> `Ok ())
                | Error (`Msg e) -> `Error (false, e))
            | Ok stopped_deps ->
                let names =
                  String.concat
                    ", "
                    (List.map (fun s -> s.Service.instance) stopped_deps)
                in
                if is_interactive () then
                  let should_start_deps =
                    prompt_yes_no
                      (Printf.sprintf
                         "Dependencies not running (%s). Start them first?"
                         names)
                      ~default:true
                  in
                  if should_start_deps then (
                    (* Start dependencies in order *)
                    let failed = ref false in
                    List.iter
                      (fun dep ->
                        if not !failed then
                          match
                            Installer.start_service
                              ~quiet:false
                              ~instance:dep.Service.instance
                              ()
                          with
                          | Ok () ->
                              Printf.printf
                                "Started %s\n%!"
                                dep.Service.instance
                          | Error (`Msg e) ->
                              Printf.eprintf
                                "Failed to start %s: %s\n%!"
                                dep.Service.instance
                                e ;
                              failed := true)
                      stopped_deps ;
                    if !failed then
                      `Error (false, "Failed to start dependencies")
                    else
                      (* Now restart the target instance *)
                      let result =
                        Installer.restart_service ~quiet:false ~instance:inst ()
                      in
                      match result with
                      | Ok () -> (
                          (* Check for stopped dependents *)
                          match
                            Installer.get_stopped_dependents ~instance:inst ()
                          with
                          | Ok [] -> `Ok ()
                          | Ok stopped_deps ->
                              let names =
                                String.concat
                                  ", "
                                  (List.map
                                     (fun s -> s.Service.instance)
                                     stopped_deps)
                              in
                              let should_restart =
                                prompt_yes_no
                                  (Printf.sprintf
                                     "Restart dependents? (%s)"
                                     names)
                                  ~default:true
                              in
                              if should_restart then
                                List.iter
                                  (fun dep ->
                                    match
                                      Installer.restart_service
                                        ~quiet:false
                                        ~instance:dep.Service.instance
                                        ()
                                    with
                                    | Ok () ->
                                        Printf.printf
                                          "Restarted %s\n%!"
                                          dep.Service.instance
                                    | Error (`Msg e) ->
                                        Printf.eprintf
                                          "Failed to restart %s: %s\n%!"
                                          dep.Service.instance
                                          e)
                                  stopped_deps ;
                              `Ok ()
                          | _ -> `Ok ())
                      | Error (`Msg e) -> `Error (false, e))
                  else (
                    prerr_endline
                      "Cancelled - dependencies must be running first." ;
                    `Ok ())
                else
                  (* Non-interactive mode - just fail like before *)
                  run_result
                    (Installer.restart_service ~quiet:false ~instance:inst ()))
        | Remove ->
            run_result
              (Installer.remove_service
                 ~quiet:false
                 ~delete_data_dir
                 ~instance:inst
                 ())
        | Purge ->
            run_result
              (Installer.purge_service
                 ~quiet:false
                 ~prompt_yes_no:
                   (if is_interactive () then prompt_yes_no
                    else fun _ ~default:_ -> false)
                 ~instance:inst
                 ())
        | Show -> (
            match Service_registry.find ~instance:inst with
            | Ok (Some svc) ->
                print_service_details svc ;
                `Ok ()
            | Ok None ->
                cmdliner_error (Printf.sprintf "Unknown instance '%s'" inst)
            | Error (`Msg msg) -> cmdliner_error msg)
        | Show_service -> (
            match Service_registry.find ~instance:inst with
            | Error (`Msg msg) -> cmdliner_error msg
            | Ok None ->
                cmdliner_error (Printf.sprintf "Unknown instance '%s'" inst)
            | Ok (Some svc) ->
                let role = svc.S.role in
                let unit = Systemd.unit_name role inst in
                let print_dropin () =
                  let path = dropin_path_for ~role ~instance:inst in
                  if Sys.file_exists path then
                    try
                      let contents = slurp_file path in
                      Format.printf "# %s@.%s@." path contents
                    with Sys_error msg -> prerr_endline msg
                in
                let () =
                  match Systemd.cat_unit ~role ~instance:inst with
                  | Ok contents ->
                      Format.printf "# systemctl cat %s@.%s@." unit contents
                  | Error (`Msg msg) ->
                      prerr_endline ("systemctl cat failed: " ^ msg) ;
                      print_dropin ()
                in
                let () =
                  match Systemd.is_enabled ~role ~instance:inst with
                  | Ok state ->
                      Format.printf
                        "@.# systemctl is-enabled %s@.%s@."
                        unit
                        state
                  | Error (`Msg msg) ->
                      prerr_endline ("systemctl is-enabled failed: " ^ msg)
                in
                let () =
                  match Systemd.status ~role ~instance:inst with
                  | Ok status ->
                      Format.printf
                        "@.# systemctl status %s --no-pager@.%s@."
                        unit
                        status
                  | Error (`Msg msg) ->
                      prerr_endline ("systemctl status failed: " ^ msg)
                in
                `Ok ())
        | Logs -> (
            match Service_registry.find ~instance:inst with
            | Error (`Msg msg) -> cmdliner_error msg
            | Ok None ->
                cmdliner_error (Printf.sprintf "Unknown instance '%s'" inst)
            | Ok (Some svc) ->
                let role = svc.S.role in
                let user_flag = if Common.is_root () then "" else "--user " in
                let unit = Systemd.unit_name role inst in

                Format.printf "# Monitor logs via journald:@." ;
                Format.printf "journalctl %s-u %s -f@." user_flag unit ;

                (match Log_viewer.get_daily_log_file ~role ~instance:inst with
                | Ok path ->
                    Format.printf "@.# Monitor daily logs via tail:@." ;
                    Format.printf "tail -f %s@." path
                | Error _ -> ()) ;

                `Ok ())
        | Edit -> (
            match Service_registry.find ~instance:inst with
            | Error (`Msg msg) -> cmdliner_error msg
            | Ok None ->
                cmdliner_error (Printf.sprintf "Unknown instance '%s'" inst)
            | Ok (Some svc) ->
                let role = svc.S.role in
                (* List dependents that will be stopped *)
                let () =
                  if svc.S.dependents <> [] then
                    Format.printf
                      "@[<v>@,\
                       This will stop the following dependents:@,\
                      \  %s@,\
                       @]"
                      (String.concat ", " svc.dependents)
                in
                (* Confirm before proceeding in interactive mode *)
                let proceed =
                  if not (is_interactive ()) then true
                  else prompt_yes_no "Proceed with edit?" ~default:true
                in
                if not proceed then (
                  print_endline "Cancelled." ;
                  `Ok ())
                else (
                  (* Stop the instance (cascade stops dependents) *)
                  (match
                     Installer.stop_service ~quiet:false ~instance:inst ()
                   with
                  | Ok () -> ()
                  | Error (`Msg msg) ->
                      Format.eprintf "Warning: failed to stop service: %s@." msg) ;
                  (* Show current values - role-specific configuration *)
                  Format.printf "@.Editing instance '%s' (role: %s)@." inst role ;
                  Format.printf "@.Current configuration:@." ;
                  (* Read env file for role-specific values *)
                  let env =
                    match Node_env.read ~inst with
                    | Ok pairs -> pairs
                    | Error _ -> []
                  in
                  let lookup key =
                    match List.assoc_opt key env with
                    | Some v -> String.trim v
                    | None -> ""
                  in
                  (* Common fields *)
                  Format.printf "  Service user: %s@." svc.service_user ;
                  Format.printf "  App bin dir: %s@." svc.app_bin_dir ;
                  (* Role-specific fields *)
                  (match role with
                  | "node" ->
                      Format.printf "  Network: %s@." svc.network ;
                      Format.printf
                        "  History mode: %s@."
                        (History_mode.to_string svc.history_mode) ;
                      Format.printf "  Data dir: %s@." svc.data_dir ;
                      Format.printf "  RPC addr: %s@." svc.rpc_addr ;
                      Format.printf "  P2P addr: %s@." svc.net_addr
                  | "baker" ->
                      let base_dir = lookup "OCTEZ_BAKER_BASE_DIR" in
                      let node_inst = lookup "OCTEZ_NODE_INSTANCE" in
                      let node_ep = lookup "OCTEZ_NODE_ENDPOINT" in
                      let dal_cfg = lookup "OCTEZ_DAL_CONFIG" in
                      let delegates = lookup "OCTEZ_BAKER_DELEGATES_CSV" in
                      let lb_vote = lookup "OCTEZ_BAKER_LB_VOTE" in
                      Format.printf "  Base dir: %s@." base_dir ;
                      if node_inst <> "" then
                        Format.printf "  Node instance: %s@." node_inst
                      else Format.printf "  Node endpoint: %s@." node_ep ;
                      (match String.lowercase_ascii dal_cfg with
                      | "disabled" -> Format.printf "  DAL config: opt-out@."
                      | "" -> Format.printf "  DAL config: auto@."
                      | ep -> Format.printf "  DAL endpoint: %s@." ep) ;
                      if delegates <> "" then
                        Format.printf "  Delegates: %s@." delegates ;
                      if lb_vote <> "" then
                        Format.printf "  LB vote: %s@." lb_vote
                  | "accuser" ->
                      let base_dir = lookup "OCTEZ_CLIENT_BASE_DIR" in
                      let node_inst = lookup "OCTEZ_NODE_INSTANCE" in
                      let node_ep = lookup "OCTEZ_NODE_ENDPOINT" in
                      Format.printf "  Base dir: %s@." base_dir ;
                      if node_inst <> "" then
                        Format.printf "  Node instance: %s@." node_inst
                      else Format.printf "  Node endpoint: %s@." node_ep
                  | "dal-node" | "dal" ->
                      let dal_data_dir = lookup "OCTEZ_DAL_DATA_DIR" in
                      let node_inst = lookup "OCTEZ_NODE_INSTANCE" in
                      let node_ep = lookup "OCTEZ_NODE_ENDPOINT" in
                      let rpc_addr = lookup "OCTEZ_DAL_RPC_ADDR" in
                      let net_addr = lookup "OCTEZ_DAL_NET_ADDR" in
                      Format.printf "  DAL data dir: %s@." dal_data_dir ;
                      if node_inst <> "" then
                        Format.printf "  Node instance: %s@." node_inst
                      else Format.printf "  Node endpoint: %s@." node_ep ;
                      if rpc_addr <> "" then
                        Format.printf "  RPC addr: %s@." rpc_addr ;
                      if net_addr <> "" then
                        Format.printf "  P2P addr: %s@." net_addr
                  | _ -> ()) ;
                  (* Extra args for all roles *)
                  let extra_args_str = String.concat " " svc.extra_args in
                  if extra_args_str <> "" then
                    Format.printf "  Extra args: %s@." extra_args_str ;
                  (* Dependencies *)
                  if svc.depends_on <> None then
                    Format.printf
                      "  Depends on: %s@."
                      (Option.value ~default:"(none)" svc.depends_on) ;
                  if svc.dependents <> [] then
                    Format.printf
                      "  Dependents: %s@."
                      (String.concat ", " svc.dependents) ;
                  (* Interactive edit based on role *)
                  Format.printf
                    "@.Enter new values (press Enter to keep current):@." ;
                  (* Prompt for new instance name first *)
                  let new_instance =
                    prompt_input ~default:(inst, inst) "Instance name"
                    |> Option.value ~default:inst
                  in
                  let is_rename = new_instance <> inst in
                  let result =
                    match role with
                    | "node" ->
                        (* Node: edit RPC addr, P2P addr, extra args *)
                        let new_rpc =
                          prompt_input
                            ~default:(svc.rpc_addr, svc.rpc_addr)
                            "RPC address"
                          |> Option.value ~default:svc.rpc_addr
                        in
                        let new_net =
                          prompt_input
                            ~default:(svc.net_addr, svc.net_addr)
                            "P2P address"
                          |> Option.value ~default:svc.net_addr
                        in
                        let new_extra =
                          prompt_input
                            ~default:(extra_args_str, extra_args_str)
                            "Extra args"
                          |> Option.value ~default:extra_args_str
                        in
                        let new_extra_args =
                          String.split_on_char ' ' new_extra
                          |> List.map String.trim
                          |> List.filter (( <> ) "")
                        in
                        (* Validate ports *)
                        let ( let* ) = Result.bind in
                        let* new_rpc =
                          validate_port_addr
                            ~label:"RPC address"
                            ~addr:new_rpc
                            ~default:svc.rpc_addr
                            ~exclude_instance:inst
                            ()
                        in
                        let* new_net =
                          validate_port_addr
                            ~label:"P2P address"
                            ~addr:new_net
                            ~default:svc.net_addr
                            ~exclude_instance:inst
                            ()
                        in
                        let req : Installer_types.node_request =
                          {
                            instance = new_instance;
                            network = svc.network;
                            history_mode = svc.history_mode;
                            data_dir = Some svc.data_dir;
                            rpc_addr = new_rpc;
                            net_addr = new_net;
                            service_user = svc.service_user;
                            app_bin_dir = svc.app_bin_dir;
                            extra_args = new_extra_args;
                            auto_enable = true;
                            logging_mode = svc.logging_mode;
                            bootstrap = Installer_types.Genesis;
                            preserve_data = true;
                            snapshot_no_check = false;
                          }
                        in
                        Result.map_error
                          (fun (`Msg s) -> s)
                          (Installer.install_node req)
                    | "baker" ->
                        (* Baker: edit delegates, LB vote, extra args *)
                        let delegates = lookup "OCTEZ_BAKER_DELEGATES_CSV" in
                        let lb_vote = lookup "OCTEZ_BAKER_LB_VOTE" in
                        let base_dir = lookup "OCTEZ_BAKER_BASE_DIR" in
                        (* Get known delegate addresses for completion *)
                        let known_delegates =
                          if base_dir <> "" then
                            match
                              Keys_reader.read_public_key_hashes ~base_dir
                            with
                            | Ok keys ->
                                List.map (fun k -> k.Keys_reader.value) keys
                            | Error _ -> []
                          else []
                        in
                        let new_delegates =
                          if known_delegates = [] then
                            prompt_input
                              ~default:(delegates, delegates)
                              "Delegates (comma-separated)"
                            |> Option.value ~default:delegates
                          else (
                            Format.printf
                              "  Known delegates: %s@."
                              (String.concat ", " known_delegates) ;
                            match
                              prompt_with_completion
                                "Delegates (comma-separated)"
                                known_delegates
                            with
                            | Some "" | None -> delegates
                            | Some v -> String.trim v)
                        in
                        let new_lb_vote =
                          let completions = ["pass"; "on"; "off"] in
                          let default_val =
                            if lb_vote = "" then "pass" else lb_vote
                          in
                          match
                            prompt_with_completion "LB vote" completions
                          with
                          | Some "" | None -> default_val
                          | Some v -> String.trim v
                        in
                        let new_extra =
                          prompt_input
                            ~default:(extra_args_str, extra_args_str)
                            "Extra args"
                          |> Option.value ~default:extra_args_str
                        in
                        let new_extra_args =
                          String.split_on_char ' ' new_extra
                          |> List.map String.trim
                          |> List.filter (( <> ) "")
                        in
                        let delegates_list =
                          String.split_on_char ',' new_delegates
                          |> List.map String.trim
                          |> List.filter (( <> ) "")
                        in
                        (* Node instance selection with completion *)
                        let current_node =
                          match svc.depends_on with
                          | Some inst -> inst
                          | None -> lookup "OCTEZ_NODE_ENDPOINT"
                        in
                        let node_services =
                          match Service_registry.list () with
                          | Ok svcs ->
                              List.filter
                                (fun (s : Service.t) -> s.role = "node")
                                svcs
                          | Error _ -> []
                        in
                        let node_names =
                          List.map
                            (fun (s : Service.t) -> s.instance)
                            node_services
                        in
                        let new_node =
                          if node_names = [] then current_node
                          else (
                            Format.printf
                              "  Available nodes: %s@."
                              (String.concat ", " node_names) ;
                            match
                              prompt_with_completion "Node instance" node_names
                            with
                            | Some "" | None -> current_node
                            | Some v -> String.trim v)
                        in
                        let node_mode =
                          if List.mem new_node node_names then
                            Installer_types.Local_instance new_node
                          else Installer_types.Remote_endpoint new_node
                        in
                        (* DAL node selection with completion *)
                        let current_dal = lookup "OCTEZ_DAL_INSTANCE" in
                        let current_dal_config = lookup "OCTEZ_DAL_CONFIG" in
                        let dal_services =
                          match Service_registry.list () with
                          | Ok svcs ->
                              List.filter
                                (fun (s : Service.t) ->
                                  s.role = "dal-node" || s.role = "dal")
                                svcs
                          | Error _ -> []
                        in
                        let dal_names =
                          List.map
                            (fun (s : Service.t) -> s.instance)
                            dal_services
                        in
                        let dal_config, dal_node =
                          if dal_names = [] then
                            (* No DAL nodes, keep current config *)
                            let cfg =
                              match
                                String.lowercase_ascii current_dal_config
                              with
                              | "disabled" -> Installer_types.Dal_disabled
                              | "" -> Installer_types.Dal_auto
                              | ep -> Installer_types.Dal_endpoint ep
                            in
                            ( cfg,
                              if current_dal = "" then None
                              else Some current_dal )
                          else (
                            Format.printf
                              "  Available DAL nodes: %s@."
                              (String.concat ", " ("none" :: dal_names)) ;
                            match
                              prompt_with_completion
                                "DAL node"
                                ("none" :: "auto" :: dal_names)
                            with
                            | Some "" | None ->
                                if current_dal <> "" then
                                  ( Installer_types.Dal_endpoint
                                      (Installer.endpoint_of_rpc
                                         (match
                                            List.find_opt
                                              (fun (s : Service.t) ->
                                                s.instance = current_dal)
                                              dal_services
                                          with
                                         | Some s -> s.rpc_addr
                                         | None -> "127.0.0.1:10732")),
                                    Some current_dal )
                                else
                                  let cfg =
                                    match
                                      String.lowercase_ascii current_dal_config
                                    with
                                    | "disabled" -> Installer_types.Dal_disabled
                                    | "" -> Installer_types.Dal_auto
                                    | ep -> Installer_types.Dal_endpoint ep
                                  in
                                  (cfg, None)
                            | Some "none" -> (Installer_types.Dal_disabled, None)
                            | Some "auto" -> (Installer_types.Dal_auto, None)
                            | Some selected ->
                                if List.mem selected dal_names then
                                  let rpc =
                                    match
                                      List.find_opt
                                        (fun (s : Service.t) ->
                                          s.instance = selected)
                                        dal_services
                                    with
                                    | Some s -> s.rpc_addr
                                    | None -> "127.0.0.1:10732"
                                  in
                                  ( Installer_types.Dal_endpoint
                                      (Installer.endpoint_of_rpc rpc),
                                    Some selected )
                                else
                                  ( Installer_types.Dal_endpoint
                                      (Installer.endpoint_of_rpc selected),
                                    None ))
                        in
                        let req : Installer_types.baker_request =
                          {
                            instance = new_instance;
                            node_mode;
                            dal_config;
                            dal_node;
                            base_dir = Some (lookup "OCTEZ_BAKER_BASE_DIR");
                            delegates = delegates_list;
                            liquidity_baking_vote =
                              (if new_lb_vote = "" then None
                               else Some new_lb_vote);
                            service_user = svc.service_user;
                            app_bin_dir = svc.app_bin_dir;
                            logging_mode = svc.logging_mode;
                            extra_args = new_extra_args;
                            auto_enable = true;
                            preserve_data = true;
                          }
                        in
                        Result.map_error
                          (fun (`Msg s) -> s)
                          (Installer.install_baker req)
                    | "accuser" ->
                        (* Accuser: edit node instance, extra args *)
                        (* Node instance selection with completion *)
                        let current_node =
                          match svc.depends_on with
                          | Some inst -> inst
                          | None -> lookup "OCTEZ_NODE_ENDPOINT"
                        in
                        let node_services =
                          match Service_registry.list () with
                          | Ok svcs ->
                              List.filter
                                (fun (s : Service.t) -> s.role = "node")
                                svcs
                          | Error _ -> []
                        in
                        let node_names =
                          List.map
                            (fun (s : Service.t) -> s.instance)
                            node_services
                        in
                        let new_node =
                          if node_names = [] then current_node
                          else (
                            Format.printf
                              "  Available nodes: %s@."
                              (String.concat ", " node_names) ;
                            match
                              prompt_with_completion "Node instance" node_names
                            with
                            | Some "" | None -> current_node
                            | Some v -> String.trim v)
                        in
                        let node_mode =
                          if List.mem new_node node_names then
                            Installer_types.Local_instance new_node
                          else Installer_types.Remote_endpoint new_node
                        in
                        let new_extra =
                          prompt_input
                            ~default:(extra_args_str, extra_args_str)
                            "Extra args"
                          |> Option.value ~default:extra_args_str
                        in
                        let new_extra_args =
                          String.split_on_char ' ' new_extra
                          |> List.map String.trim
                          |> List.filter (( <> ) "")
                        in
                        let req : Installer_types.accuser_request =
                          {
                            instance = new_instance;
                            node_mode;
                            base_dir = Some (lookup "OCTEZ_CLIENT_BASE_DIR");
                            service_user = svc.service_user;
                            app_bin_dir = svc.app_bin_dir;
                            logging_mode = svc.logging_mode;
                            extra_args = new_extra_args;
                            auto_enable = true;
                            preserve_data = true;
                          }
                        in
                        Result.map_error
                          (fun (`Msg s) -> s)
                          (Installer.install_accuser req)
                    | "dal-node" | "dal" ->
                        (* DAL node: edit node instance, RPC addr, P2P addr, extra args *)
                        (* Node instance selection with completion *)
                        let current_node =
                          match svc.depends_on with
                          | Some inst -> inst
                          | None -> lookup "OCTEZ_NODE_ENDPOINT"
                        in
                        let node_services =
                          match Service_registry.list () with
                          | Ok svcs ->
                              List.filter
                                (fun (s : Service.t) -> s.role = "node")
                                svcs
                          | Error _ -> []
                        in
                        let node_names =
                          List.map
                            (fun (s : Service.t) -> s.instance)
                            node_services
                        in
                        let new_node =
                          if node_names = [] then current_node
                          else (
                            Format.printf
                              "  Available nodes: %s@."
                              (String.concat ", " node_names) ;
                            match
                              prompt_with_completion "Node instance" node_names
                            with
                            | Some "" | None -> current_node
                            | Some v -> String.trim v)
                        in
                        let new_depends_on, new_node_endpoint =
                          if List.mem new_node node_names then
                            (* Local node instance *)
                            let node_svc =
                              List.find_opt
                                (fun (s : Service.t) -> s.instance = new_node)
                                node_services
                            in
                            let ep =
                              match node_svc with
                              | Some s -> Installer.endpoint_of_rpc s.rpc_addr
                              | None -> "http://127.0.0.1:8732"
                            in
                            (Some new_node, ep)
                          else
                            (* Remote endpoint *)
                            (None, Installer.endpoint_of_rpc new_node)
                        in
                        let dal_rpc = lookup "OCTEZ_DAL_RPC_ADDR" in
                        let dal_net = lookup "OCTEZ_DAL_NET_ADDR" in
                        let new_rpc =
                          prompt_input
                            ~default:(dal_rpc, dal_rpc)
                            "DAL RPC address"
                          |> Option.value ~default:dal_rpc
                        in
                        let new_net =
                          prompt_input
                            ~default:(dal_net, dal_net)
                            "DAL P2P address"
                          |> Option.value ~default:dal_net
                        in
                        let new_extra =
                          prompt_input
                            ~default:(extra_args_str, extra_args_str)
                            "Extra args"
                          |> Option.value ~default:extra_args_str
                        in
                        let new_extra_args =
                          String.split_on_char ' ' new_extra
                          |> List.map String.trim
                          |> List.filter (( <> ) "")
                        in
                        (* Validate ports *)
                        let ( let* ) = Result.bind in
                        let* new_rpc =
                          validate_port_addr
                            ~label:"DAL RPC address"
                            ~addr:new_rpc
                            ~default:dal_rpc
                            ~exclude_instance:inst
                            ()
                        in
                        let* new_net =
                          validate_port_addr
                            ~label:"DAL P2P address"
                            ~addr:new_net
                            ~default:dal_net
                            ~exclude_instance:inst
                            ()
                        in
                        let dal_data_dir = lookup "OCTEZ_DAL_DATA_DIR" in
                        let client_base_dir = lookup "OCTEZ_CLIENT_BASE_DIR" in
                        let req : Installer_types.daemon_request =
                          {
                            role = "dal-node";
                            instance = new_instance;
                            network = svc.network;
                            history_mode = svc.history_mode;
                            data_dir = dal_data_dir;
                            rpc_addr = new_rpc;
                            net_addr = new_net;
                            service_user = svc.service_user;
                            app_bin_dir = svc.app_bin_dir;
                            logging_mode = svc.logging_mode;
                            service_args = new_extra_args;
                            extra_env =
                              [
                                ("OCTEZ_CLIENT_BASE_DIR", client_base_dir);
                                ("OCTEZ_NODE_ENDPOINT", new_node_endpoint);
                                ("OCTEZ_DAL_DATA_DIR", dal_data_dir);
                                ("OCTEZ_DAL_RPC_ADDR", new_rpc);
                                ("OCTEZ_DAL_NET_ADDR", new_net);
                              ];
                            extra_paths = [client_base_dir; dal_data_dir];
                            auto_enable = true;
                            depends_on = new_depends_on;
                            preserve_data = true;
                          }
                        in
                        Result.map_error
                          (fun (`Msg s) -> s)
                          (Installer.install_daemon req)
                    | _ ->
                        Error
                          (Printf.sprintf
                             "Edit not supported for role '%s'"
                             role)
                  in
                  match result with
                  | Ok _service -> (
                      (* Handle rename: clean up old instance if name changed *)
                      let cleanup_result =
                        if is_rename then (
                          Format.printf
                            "@.Renaming instance from '%s' to '%s'...@."
                            inst
                            new_instance ;
                          Installer.cleanup_renamed_instance
                            ~quiet:false
                            ~old_instance:inst
                            ~new_instance
                            ())
                        else Ok ()
                      in
                      match cleanup_result with
                      | Ok () ->
                          Format.printf
                            "@.Instance '%s' updated successfully.@."
                            new_instance ;
                          Format.printf
                            "@.To restart the stopped instances:@.  \
                             octez-manager instance %s start@."
                            new_instance ;
                          `Ok ()
                      | Error (`Msg msg) ->
                          Format.printf
                            "@.Instance '%s' updated but rename cleanup \
                             failed: %s@."
                            new_instance
                            msg ;
                          `Ok ())
                  | Error msg ->
                      cmdliner_error (Printf.sprintf "Edit failed: %s" msg))))
  in
  Term.(ret (const run $ instance $ action $ delete_data_dir))

let instance_cmd =
  let info = Cmd.info "instance" ~doc:"Manage existing Octez services." in
  Cmd.v info instance_term

let list_cmd =
  let term =
    let run () =
      Capabilities.register () ;
      match
        Miaou_interfaces.Capability.get
          Manager_interfaces.Service_manager_capability.key
      with
      | Some cap -> (
          let module SM = (val cap : Manager_interfaces.Service_manager) in
          match SM.list () with
          | Ok services ->
              print_services services ;
              `Ok ()
          | Error (`Msg msg) -> cmdliner_error msg)
      | None -> cmdliner_error "Service manager capability not available"
    in
    Term.(ret (const run $ const ()))
  in
  let info = Cmd.info "list" ~doc:"Show registered services" in
  Cmd.v info term

let purge_all_cmd =
  let term =
    let run () =
      Capabilities.register () ;
      match Service_registry.list () with
      | Error (`Msg msg) -> cmdliner_error msg
      | Ok services ->
          if services = [] then (
            print_endline "No services registered to purge." ;
            (* Still clear directory registry in case of stale entries *)
            (match Directory_registry.clear_all () with
            | Ok () -> Format.printf "Directory registry cleared.@."
            | Error (`Msg msg) ->
                Format.eprintf
                  "Warning: Failed to clear directory registry: %s@."
                  msg) ;
            `Ok ())
          else
            let failures = ref [] in
            List.iter
              (fun svc ->
                let instance = svc.S.instance in
                let role = svc.S.role in
                Format.printf "Purging instance '%s' (%s)...@." instance role ;
                match
                  Installer.purge_service
                    ~quiet:false
                    ~prompt_yes_no:
                      (if is_interactive () then prompt_yes_no
                       else fun _ ~default:_ -> false)
                    ~instance
                    ()
                with
                | Ok () ->
                    Format.printf "  ✓ Successfully purged '%s'@." instance
                | Error (`Msg msg) ->
                    Format.eprintf "  ✗ Failed to purge '%s': %s@." instance msg ;
                    failures := (instance, msg) :: !failures)
              services ;
            if !failures = [] then (
              (* Clear directory registry after successful purge *)
              (match Directory_registry.clear_all () with
              | Ok () -> Format.printf "@.Directory registry cleared.@."
              | Error (`Msg msg) ->
                  Format.eprintf
                    "@.Warning: Failed to clear directory registry: %s@."
                    msg) ;
              Format.printf "All instances purged successfully.@." ;
              `Ok ())
            else
              let error_summary =
                Printf.sprintf
                  "@.%d instance(s) failed to purge"
                  (List.length !failures)
              in
              cmdliner_error error_summary
    in
    Term.(ret (const run $ const ()))
  in
  let info =
    Cmd.info
      "purge-all"
      ~doc:
        "Purge all registered instances. This removes each service, deletes \
         data directories, log files, and (when run as root) drops service \
         users that are no longer referenced by other services."
  in
  Cmd.v info term

let cleanup_orphans_cmd =
  let dry_run =
    Arg.(
      value & flag
      & info
          ["dry-run"; "n"]
          ~doc:"Show what would be removed without actually deleting.")
  in
  let term =
    let run dry_run =
      Capabilities.register () ;
      match Installer.find_orphan_directories () with
      | Error (`Msg msg) -> cmdliner_error msg
      | Ok (orphan_dirs, orphan_logs) -> (
          if orphan_dirs = [] && orphan_logs = [] then (
            print_endline "No orphan directories or files found." ;
            `Ok ())
          else if dry_run then (
            print_endline "Would remove the following orphan paths:" ;
            List.iter (fun d -> Format.printf "  [dir]  %s@." d) orphan_dirs ;
            List.iter (fun f -> Format.printf "  [file] %s@." f) orphan_logs ;
            `Ok ())
          else
            match Installer.cleanup_orphans ~dry_run:false with
            | Error (`Msg msg) -> cmdliner_error msg
            | Ok (removed, errors) ->
                List.iter (fun p -> Format.printf "  ✓ Removed: %s@." p) removed ;
                List.iter
                  (fun (p, msg) ->
                    Format.eprintf "  ✗ Failed to remove %s: %s@." p msg)
                  errors ;
                if errors = [] then (
                  Format.printf
                    "@.Cleanup complete. %d item(s) removed.@."
                    (List.length removed) ;
                  `Ok ())
                else
                  cmdliner_error
                    (Printf.sprintf
                       "%d item(s) failed to remove"
                       (List.length errors)))
    in
    Term.(ret (const run $ dry_run))
  in
  let info =
    Cmd.info
      "cleanup-orphans"
      ~doc:
        "Remove orphan data directories and log files not associated with any \
         registered service. Use --dry-run to preview what would be removed."
  in
  Cmd.v info term

let cleanup_dependencies_cmd =
  let term =
    let run () =
      Capabilities.register () ;
      match Installer.cleanup_dependencies () with
      | Error (`Msg msg) -> cmdliner_error msg
      | Ok 0 ->
          print_endline "No stale dependency entries found." ;
          `Ok ()
      | Ok count ->
          Format.printf "Cleaned up %d stale dependency entrie(s).@." count ;
          `Ok ()
    in
    Term.(ret (const run $ const ()))
  in
  let info =
    Cmd.info
      "cleanup-dependencies"
      ~doc:
        "Remove stale dependency entries from service configurations. This \
         cleans up references to services that have been removed."
  in
  Cmd.v info term

let list_networks_cmd =
  let output_json =
    Arg.(value & flag & info ["json"] ~doc:"Emit JSON output instead of text.")
  in
  let term =
    let run output_json =
      match Teztnets.list_networks () with
      | Ok infos ->
          (* warn_if_fallback pairs ; -- TODO: reimplement warning if needed *)
          if output_json then
            let json =
              `List
                (List.map
                   (fun (n : Teztnets.network_info) ->
                     `Assoc
                       [
                         ("alias", `String n.alias);
                         ("network_url", `String n.network_url);
                         ("human_name", `String n.human_name);
                         ( "description",
                           match n.description with
                           | Some s -> `String s
                           | None -> `Null );
                         ( "rpc_url",
                           match n.rpc_url with
                           | Some s -> `String s
                           | None -> `Null );
                       ])
                   infos)
            in
            Yojson.Safe.pretty_to_string json |> print_endline
          else
            List.iter
              (fun (n : Teztnets.network_info) ->
                Format.printf
                  "%-16s %-24s %s@."
                  n.alias
                  n.human_name
                  n.network_url)
              infos ;
          `Ok ()
      | Error (`Msg msg) -> cmdliner_error msg
    in
    Term.(ret (const run $ output_json))
  in
  let info =
    Cmd.info
      "list-available-networks"
      ~doc:"Show networks advertised on teztnets.com (with fallbacks)."
  in
  Cmd.v info term

let list_snapshots_cmd =
  let network =
    let doc = "Network alias or teztnets.json URL to inspect." in
    Arg.(value & opt string "mainnet" & info ["network"] ~doc ~docv:"NET")
  in
  let output_json =
    Arg.(value & flag & info ["json"] ~doc:"Emit JSON output instead of text.")
  in
  let term =
    let run network output_json =
      match Snapshots.slug_of_network network with
      | None -> cmdliner_error "--network cannot be empty"
      | Some slug -> (
          match Snapshots.list ~network_slug:slug with
          | Ok entries ->
              if output_json then
                let json = `List (List.map snapshot_entry_to_json entries) in
                Yojson.Safe.pretty_to_string json |> print_endline
              else List.iter print_snapshot_entry entries ;
              `Ok ()
          | Error (`Msg msg) -> cmdliner_error msg)
    in
    Term.(ret (const run $ network $ output_json))
  in
  let info =
    Cmd.info
      "list-snapshots"
      ~doc:"List downloads published on snapshots.tzinit.org for a network."
  in
  Cmd.v info term

let ui_cmd =
  let open Cmdliner in
  let term =
    let page_arg =
      Arg.(
        value
        & opt (some string) None
        & info ["page"] ~doc:"Start on a registered page" ~docv:"NAME")
    in
    let log_flag =
      Arg.(value & flag & info ["ui-log"] ~doc:"Enable UI debug logs")
    in
    let logfile_arg =
      Arg.(
        value
        & opt (some string) None
        & info ["ui-logfile"] ~doc:"Write UI logs to FILE" ~docv:"FILE")
    in
    Term.(
      ret
        (const (fun page log logfile ->
             Capabilities.register () ;
             (* Ignore SIGPIPE to prevent crashes when subprocesses write to closed pipes *)
             Sys.set_signal Sys.sigpipe Sys.Signal_ignore ;
             let result =
               Eio_main.run @@ fun env ->
               Eio.Switch.run @@ fun sw ->
               Miaou_helpers.Fiber_runtime.init ~env ~sw ;
               Octez_manager_ui.Manager_app.run ?page ~log ?logfile ()
             in
             match result with
             | Ok () -> `Ok ()
             | Error (`Msg msg) -> cmdliner_error msg)
        $ page_arg $ log_flag $ logfile_arg))
  in
  let info =
    Cmd.info
      "ui"
      ~doc:"Launch the Miaou-based interactive interface (experimental)"
  in
  Cmd.v info term

let root_cmd =
  let doc = "Minimal Octez service manager" in
  let info = Cmd.info "octez-manager" ~doc in
  Cmd.group
    info
    [
      instance_cmd;
      install_node_cmd;
      install_baker_cmd;
      install_accuser_cmd;
      install_dal_node_cmd;
      list_cmd;
      purge_all_cmd;
      cleanup_orphans_cmd;
      cleanup_dependencies_cmd;
      list_networks_cmd;
      list_snapshots_cmd;
      ui_cmd;
    ]

let () =
  try exit (Cmd.eval root_cmd)
  with Sys.Break ->
    (* User pressed Ctrl-C during interactive prompts or operations; exit with
       the conventional 130 status code without a stack trace. *)
    prerr_endline "" ;
    exit 130
