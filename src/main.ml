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
  | Logging_mode.File {path; rotate} ->
      Format.fprintf fmt "file:%s (rotate=%b)" path rotate

let print_service_details svc =
  Format.printf "Instance      : %s@." svc.S.instance ;
  Format.printf "Service name  : %s@." (Systemd.unit_name svc.role svc.instance) ;
  Format.printf "Role          : %s@." svc.role ;
  Format.printf "Network       : %s@." svc.network ;
  Format.printf "History mode  : %s@." (History_mode.to_string svc.history_mode) ;
  Format.printf "Data dir      : %s@." svc.data_dir ;
  Format.printf "RPC addr      : %s@." svc.rpc_addr ;
  Format.printf "P2P addr      : %s@." svc.net_addr ;
  Format.printf "Service user  : %s@." svc.service_user ;
  Format.printf "Octez bin dir : %s@." svc.app_bin_dir ;
  Format.printf "Created at    : %s@." svc.created_at ;
  Format.printf "Logging       : %a@." pp_logging svc.logging_mode

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
    Format.printf "%s%s: %!" question suffix ;
    match read_line () with
    | exception End_of_file -> Option.map snd default
    | line ->
        let trimmed = String.trim line in
        if String.equal trimmed "" then Option.map snd default else Some trimmed

let rec prompt_required_string question =
  match prompt_input question with
  | Some value -> value
  | None ->
      prerr_endline "A value is required." ;
      prompt_required_string question

let prompt_string_with_default question default_value =
  match prompt_input ~default:(default_value, default_value) question with
  | Some value -> value
  | None -> default_value

let prompt_history_mode default =
  if not (is_interactive ()) then default
  else
    let default_label = History_mode.to_string default in
    let rec loop () =
      match
        prompt_input
          ~default:(default_label, default_label)
          "History mode (rolling/full/archive)"
      with
      | Some value -> (
          match History_mode.of_string value with
          | Ok hm -> hm
          | Error _ ->
              prerr_endline "Please enter rolling, full, or archive." ;
              loop ())
      | None -> loop ()
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

let run_result = function
  | Ok () -> `Ok ()
  | Error (`Msg msg) -> cmdliner_error msg

let logging_mode_term =
  let doc_file = "Write Octez logs to PATH (file logging)." in
  let log_file =
    Arg.(
      value
      & opt (some string) None
      & info ["log-file"] ~doc:doc_file ~docv:"PATH")
  in
  let doc_rotate = "Request log rotation for file logging." in
  let log_rotate = Arg.(value & flag & info ["log-rotate"] ~doc:doc_rotate) in
  let doc_journal = "Send logs to journald instead of files." in
  let journald = Arg.(value & flag & info ["journald"] ~doc:doc_journal) in
  let make journald log_rotate log_file =
    if journald then Logging_mode.Journald
    else
      let rotate = if log_rotate then true else Common.is_root () in
      let path = match log_file with Some p -> p | None -> "" in
      Logging_mode.File {path; rotate}
  in
  Term.(const make $ journald $ log_rotate $ log_file)

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

let history_mode_conv : History_mode.t Cmdliner.Arg.conv =
  Arg.conv (History_mode.of_string, History_mode.pp)

let install_node_cmd =
  let instance =
    let doc = "Instance name used for node.env and systemd units." in
    Arg.(
      value & opt (some string) None & info ["instance"; "i"] ~doc ~docv:"NAME")
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
      "Directory containing Octez binaries (defaults to the directory holding \
       octez-node found in $PATH)."
    in
    Arg.(value & opt (some string) None & info ["app-bin-dir"] ~doc ~docv:"DIR")
  in
  let extra_args =
    let doc = "Extra --flag passed to octez-node." in
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
  let snapshot_kind =
    let doc =
      "Snapshot variant from snapshots.tzinit.org (rolling, full, full:50, \
       ...)."
    in
    Arg.(
      value & opt (some string) None & info ["snapshot-kind"] ~doc ~docv:"KIND")
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
  let make instance_opt network_opt history_mode_opt data_dir rpc_addr net_addr
      service_user app_bin_dir extra_args snapshot_flag snapshot_uri
      snapshot_kind snapshot_no_check no_enable logging_mode =
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
            let network =
              match normalize_opt_string network_opt with
              | Some net -> net
              | None ->
                  if is_interactive () then
                    prompt_string_with_default
                      "Network (network name, e.g. mainnet)"
                      "mainnet"
                  else "mainnet"
            in
            let history_mode =
              match history_mode_opt with
              | Some hm -> hm
              | None -> prompt_history_mode History_mode.default
            in
            let snapshot_requested_initial =
              snapshot_flag
              || Option.is_some snapshot_uri
              || Option.is_some snapshot_kind
            in
            let snapshot_requested =
              if snapshot_requested_initial then true
              else if is_interactive () then
                prompt_yes_no
                  "Download and import a tzinit snapshot before starting?"
                  ~default:true
              else false
            in
            let snapshot_kind =
              match normalize_opt_string snapshot_kind with
              | Some _ as provided -> provided
              | None when snapshot_requested && is_interactive () ->
                  prompt_input
                    "Snapshot kind (rolling/full/full:50, leave blank for auto)"
                  |> normalize_opt_string
              | None -> None
            in
            let snapshot_uri =
              match normalize_opt_string snapshot_uri with
              | Some _ as provided -> provided
              | None when snapshot_requested && is_interactive () ->
                  prompt_input
                    "Snapshot URI (path or URL, leave blank to auto-select)"
                  |> normalize_opt_string
              | None -> None
            in
            let snapshot_requested =
              snapshot_requested
              || Option.is_some snapshot_uri
              || Option.is_some snapshot_kind
            in
            let bootstrap =
              if snapshot_requested then
                Snapshot {src = snapshot_uri; kind = snapshot_kind}
              else Genesis
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
                preserve_data = false;
                snapshot_no_check;
              }
            in
            match Installer.install_node req with
            | Ok service ->
                Format.printf
                  "Installed %s (%s)\n"
                  service.S.instance
                  service.network ;
                `Ok ()
            | Error (`Msg msg) -> cmdliner_error msg))
  in
  let term =
    Term.(
      ret
        (const make $ instance $ network $ history_mode_opt_term $ data_dir
       $ rpc_addr $ net_addr $ service_user $ app_bin_dir $ extra_args
       $ snapshot_flag $ snapshot_uri $ snapshot_kind $ snapshot_no_check
       $ auto_enable $ logging_mode_term))
  in
  let info =
    Cmd.info "install-node" ~doc:"Install an octez-node systemd instance"
  in
  Cmd.v info term

let install_baker_cmd =
  let instance =
    let doc = "Instance name for the baker systemd unit." in
    Arg.(
      value & opt (some string) None & info ["instance"; "i"] ~doc ~docv:"NAME")
  in
  let node_instance =
    let doc =
      "Existing octez-manager node instance to reuse for data-dir and network."
    in
    Arg.(
      value & opt (some string) None & info ["node-instance"] ~doc ~docv:"NODE")
  in
  let node_data_dir =
    let doc =
      "Path to the local node data directory (required if --node-instance is \
       omitted)."
    in
    Arg.(
      value & opt (some string) None & info ["node-data-dir"] ~doc ~docv:"DIR")
  in
  let node_endpoint =
    let doc = "Custom RPC endpoint for the baker to contact." in
    Arg.(
      value & opt (some string) None & info ["node-endpoint"] ~doc ~docv:"URI")
  in
  let base_dir =
    let doc =
      "Baker base directory for wallets (defaults to an instance-specific \
       path)."
    in
    Arg.(value & opt (some string) None & info ["base-dir"] ~doc ~docv:"DIR")
  in
  let network =
    let doc =
      "Override the target network (defaults to the node network or mainnet)."
    in
    Arg.(value & opt (some string) None & info ["network"] ~doc ~docv:"NET")
  in
  let delegates =
    let doc = "Delegate key hash or alias passed as --delegate." in
    Arg.(value & opt_all string [] & info ["delegate"] ~doc ~docv:"KEY")
  in
  let dal_endpoint =
    let doc =
      "DAL node endpoint (e.g., http://localhost:10732). Use 'none' or \
       'disabled' to opt-out with --without-dal flag."
    in
    Arg.(
      value
      & opt (some string) None
      & info ["dal-endpoint"] ~doc ~docv:"ENDPOINT")
  in
  let liquidity_baking_vote =
    let doc =
      "Liquidity baking toggle vote (on, off, or pass). Required for baker to \
       start."
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
  let make instance_opt node_instance node_data_dir node_endpoint base_dir
      network delegates dal_endpoint_opt liquidity_baking_vote_opt extra_args
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
            let lb_vote_result =
              match normalize_opt_string liquidity_baking_vote_opt with
              | Some vote -> Ok (Some vote)
              | None ->
                  if is_interactive () then
                    let vote =
                      prompt_required_string
                        "Liquidity baking vote (on/off/pass)"
                    in
                    Ok (Some vote)
                  else
                    Error
                      "Liquidity baking vote is required in non-interactive \
                       mode. Use --liquidity-baking-vote"
            in
            match lb_vote_result with
            | Error msg -> cmdliner_error msg
            | Ok liquidity_baking_vote -> (
                let dal_config_result =
                  match normalize_opt_string dal_endpoint_opt with
                  | Some ep ->
                      let normalized =
                        String.lowercase_ascii (String.trim ep)
                      in
                      if normalized = "none" || normalized = "disabled" then
                        Ok Dal_disabled
                      else Ok (Dal_endpoint ep)
                  | None ->
                      if is_interactive () then
                        let response =
                          prompt_required_string
                            "DAL node endpoint (or 'none' to opt-out)"
                        in
                        let normalized =
                          String.lowercase_ascii (String.trim response)
                        in
                        if normalized = "none" || normalized = "disabled" then
                          Ok Dal_disabled
                        else Ok (Dal_endpoint response)
                      else Ok Dal_auto
                in
                match dal_config_result with
                | Error msg -> cmdliner_error msg
                | Ok dal_config -> (
                    let req : baker_request =
                      {
                        instance;
                        network;
                        node_instance;
                        node_data_dir;
                        node_endpoint;
                        node_mode = `Auto;
                        base_dir;
                        delegates;
                        dal_config;
                        liquidity_baking_vote;
                        extra_args;
                        service_user;
                        app_bin_dir;
                        logging_mode;
                        auto_enable = not no_enable;
                      }
                    in
                    match Installer.install_baker req with
                    | Ok service ->
                        Format.printf
                          "Installed %s (%s)\n"
                          service.S.instance
                          service.network ;
                        `Ok ()
                    | Error (`Msg msg) -> cmdliner_error msg))))
  in
  let term =
    Term.(
      ret
        (const make $ instance $ node_instance $ node_data_dir $ node_endpoint
       $ base_dir $ network $ delegates $ dal_endpoint $ liquidity_baking_vote
       $ extra_args $ service_user $ app_bin_dir $ auto_enable
       $ logging_mode_term))
  in
  let info = Cmd.info "install-baker" ~doc:"Install an octez-baker service" in
  Cmd.v info term

let install_accuser_cmd =
  let instance =
    Arg.(
      required
      & opt (some string) None
      & info ["instance"] ~doc:"Accuser instance name" ~docv:"NAME")
  in
  let network =
    Arg.(
      value & opt string "mainnet"
      & info ["network"] ~doc:"Target Tezos network" ~docv:"NET")
  in
  let endpoint =
    Arg.(
      value
      & opt string "http://127.0.0.1:8732"
      & info ["endpoint"] ~doc:"RPC endpoint to monitor" ~docv:"URI")
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
      & info ["extra-arg"] ~doc:"Additional octez-accuser arguments" ~docv:"ARG")
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
  let data_dir_opt =
    Arg.(
      value
      & opt (some string) None
      & info
          ["data-dir"]
          ~doc:"State directory (defaults to a role-specific path)"
          ~docv:"DIR")
  in
  let make instance network endpoint base_dir extra_args service_user
      app_bin_dir no_enable logging_mode data_dir_opt =
    match resolve_app_bin_dir app_bin_dir with
    | Error msg -> cmdliner_error msg
    | Ok app_bin_dir -> (
        let data_dir =
          match data_dir_opt with
          | Some dir when String.trim dir <> "" -> dir
          | _ -> Common.default_role_dir "accuser" instance
        in
        let final_base_dir =
          match base_dir with
          | Some dir when String.trim dir <> "" -> dir
          | _ -> data_dir
        in
        let service_args =
          ["run"; "--endpoint"; endpoint; "--base-dir"; final_base_dir]
          @ extra_args
        in
        let req : daemon_request =
          {
            role = "accuser";
            instance;
            network;
            history_mode = History_mode.default;
            data_dir;
            rpc_addr = endpoint;
            net_addr = "";
            service_user;
            app_bin_dir;
            logging_mode;
            service_args;
            extra_env = [];
            extra_paths = [];
            auto_enable = not no_enable;
          }
        in
        match Installer.install_daemon req with
        | Ok service ->
            Format.printf
              "Installed %s (%s)\n"
              service.S.instance
              service.network ;
            `Ok ()
        | Error (`Msg msg) -> cmdliner_error msg)
  in
  let term =
    Term.(
      ret
        (const make $ instance $ network $ endpoint $ base_dir $ extra_args
       $ service_user $ app_bin_dir $ auto_enable $ logging_mode_term
       $ data_dir_opt))
  in
  let info =
    Cmd.info "install-accuser" ~doc:"Install an octez-accuser service"
  in
  Cmd.v info term

let install_signer_cmd =
  let instance =
    Arg.(
      required
      & opt (some string) None
      & info ["instance"] ~doc:"Signer instance name" ~docv:"NAME")
  in
  let network =
    Arg.(
      value & opt string "generic"
      & info ["network"] ~doc:"Label stored in the registry" ~docv:"NET")
  in
  let base_dir =
    Arg.(
      value
      & opt (some string) None
      & info ["base-dir"] ~doc:"Signer data directory" ~docv:"DIR")
  in
  let address =
    Arg.(
      value & opt string "127.0.0.1"
      & info
          ["address"]
          ~doc:"Listening address for the socket signer"
          ~docv:"ADDR")
  in
  let port =
    Arg.(
      value & opt int 6732
      & info ["port"] ~doc:"TCP port for the signer" ~docv:"PORT")
  in
  let password_file =
    Arg.(
      value
      & opt (some string) None
      & info
          ["password-file"]
          ~doc:"Path to the password file passed to octez-signer"
          ~docv:"FILE")
  in
  let authorize =
    let doc = "Authorize NAME:PUBLIC_KEY (multiple allowed)." in
    Arg.(value & opt_all string [] & info ["authorize"] ~doc ~docv:"SPEC")
  in
  let no_auth =
    Arg.(
      value & flag & info ["no-auth"] ~doc:"Disable --require-authentication")
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
  let parse_authorize specs =
    let rec loop acc = function
      | [] -> Ok (List.rev acc)
      | raw :: rest ->
          let trimmed = String.trim raw in
          if trimmed = "" then loop acc rest
          else
            let name_opt, key =
              match String.split_on_char ':' trimmed with
              | [] -> (None, "")
              | [key] -> (None, key)
              | name :: tail -> (Some name, String.concat ":" tail)
            in
            let key = String.trim key in
            if key = "" then
              Error (`Msg (Printf.sprintf "Invalid --authorize entry '%s'" raw))
            else
              let name_opt =
                match name_opt with
                | None -> None
                | Some name ->
                    let n = String.trim name in
                    if n = "" then None else Some n
              in
              loop ((name_opt, key) :: acc) rest
    in
    loop [] specs
  in
  let make instance network base_dir address port password_file authorize_specs
      no_auth service_user app_bin_dir no_enable logging_mode =
    match resolve_app_bin_dir app_bin_dir with
    | Error msg -> cmdliner_error msg
    | Ok app_bin_dir -> (
        match parse_authorize authorize_specs with
        | Error (`Msg msg) -> cmdliner_error msg
        | Ok authorized_keys -> (
            let req : signer_request =
              {
                instance;
                network;
                base_dir;
                address;
                port;
                service_user;
                app_bin_dir;
                logging_mode;
                require_auth = not no_auth;
                password_file;
                auto_enable = not no_enable;
                authorized_keys;
              }
            in
            match Installer.install_signer req with
            | Ok service ->
                Format.printf
                  "Signer %s listening on %s (%s)\n"
                  service.S.instance
                  service.S.rpc_addr
                  service.S.data_dir ;
                Format.printf
                  "Authorize more keys with: %s --base-dir %s add authorized \
                   key <pk> --name <alias>\n"
                  (Filename.concat app_bin_dir "octez-signer")
                  service.S.data_dir ;
                Format.printf
                  "Point bakers at it via: octez-client -d <baker-base-dir> -R \
                   %s config update\n"
                  service.S.rpc_addr ;
                `Ok ()
            | Error (`Msg msg) -> cmdliner_error msg))
  in
  let term =
    Term.(
      ret
        (const make $ instance $ network $ base_dir $ address $ port
       $ password_file $ authorize $ no_auth $ service_user $ app_bin_dir
       $ auto_enable $ logging_mode_term))
  in
  let info = Cmd.info "install-signer" ~doc:"Install an octez-signer service" in
  Cmd.v info term

let install_dal_node_cmd =
  let instance =
    let doc = "Instance name used for dal-node.env and systemd units." in
    Arg.(
      value & opt (some string) None & info ["instance"; "i"] ~doc ~docv:"NAME")
  in
  let network =
    Arg.(
      value & opt string "mainnet"
      & info ["network"] ~doc:"Target network" ~docv:"NET")
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
  let endpoint =
    Arg.(
      value
      & opt string "http://127.0.0.1:8732"
      & info ["endpoint"] ~doc:"Tezos node RPC endpoint" ~docv:"URI")
  in
  let extra_args =
    Arg.(
      value & opt_all string []
      & info ["extra-arg"] ~doc:"Additional dal-node arguments" ~docv:"ARG")
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
  let make instance_opt network data_dir_opt rpc_addr net_addr endpoint
      extra_args service_user app_bin_dir no_enable logging_mode =
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
            let service_args =
              [
                "run";
                "--data-dir";
                data_dir;
                "--rpc-addr";
                rpc_addr;
                "--net-addr";
                net_addr;
                "--endpoint";
                endpoint;
              ]
              @ extra_args
            in
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
                service_args;
                extra_env = [];
                extra_paths = [];
                auto_enable = not no_enable;
              }
            in
            match Installer.install_daemon req with
            | Ok service ->
                Format.printf
                  "Installed %s (%s)\n"
                  service.S.instance
                  service.network ;
                `Ok ()
            | Error (`Msg msg) -> cmdliner_error msg))
  in
  let term =
    Term.(
      ret
        (const make $ instance $ network $ data_dir_opt $ rpc_addr $ net_addr
       $ endpoint $ extra_args $ service_user $ app_bin_dir $ auto_enable
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
  | Refresh_snapshot

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
        ("refresh-from-new-snapshot", Refresh_snapshot);
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
  let snapshot_uri_override =
    Arg.(
      value
      & opt (some string) None
      & info
          ["snapshot-uri"]
          ~doc:"Snapshot URI used by the refresh-from-new-snapshot action."
          ~docv:"URI")
  in
  let snapshot_kind_override =
    Arg.(
      value
      & opt (some string) None
      & info
          ["snapshot-kind"]
          ~doc:
            "Snapshot slug (rolling, full:50, ...) when refreshing from tzinit."
          ~docv:"KIND")
  in
  let snapshot_network_override =
    Arg.(
      value
      & opt (some string) None
      & info
          ["snapshot-network"]
          ~doc:
            "Override the tzinit network slug/URL for \
             refresh-from-new-snapshot."
          ~docv:"NET")
  in
  let snapshot_history_mode_override =
    Arg.(
      value
      & opt (some history_mode_conv) None
      & info
          ["snapshot-history-mode"]
          ~doc:
            "History-mode hint used when auto-selecting snapshot kinds during \
             refresh."
          ~docv:"MODE")
  in
  let snapshot_no_check =
    Arg.(
      value & flag
      & info
          ["snapshot-no-check"]
          ~doc:
            "Pass --no-check to octez-node snapshot import during \
             refresh-from-new-snapshot.")
  in
  let run instance action delete_data_dir snapshot_uri_override
      snapshot_kind_override snapshot_network_override
      snapshot_history_mode_override snapshot_no_check =
    match (instance, action) with
    | None, _ -> `Help (`Pager, None)
    | Some _, None ->
        cmdliner_error
          "ACTION required \
           (start|stop|restart|remove|purge|show|show-service|refresh-from-new-snapshot)"
    | Some inst, Some action -> (
        match action with
        | Start -> run_result (Installer.start_service ~instance:inst)
        | Stop -> run_result (Installer.stop_service ~instance:inst)
        | Restart -> run_result (Installer.restart_service ~instance:inst)
        | Remove ->
            run_result
              (Installer.remove_service ~delete_data_dir ~instance:inst)
        | Purge -> run_result (Installer.purge_service ~instance:inst)
        | Refresh_snapshot ->
            run_result
              (Installer.refresh_instance_from_snapshot
                 ~instance:inst
                 ?snapshot_uri:snapshot_uri_override
                 ?snapshot_kind:snapshot_kind_override
                 ?network:snapshot_network_override
                 ?history_mode:snapshot_history_mode_override
                 ~no_check:snapshot_no_check
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
                `Ok ()))
  in
  Term.(
    ret
      (const run $ instance $ action $ delete_data_dir $ snapshot_uri_override
     $ snapshot_kind_override $ snapshot_network_override
     $ snapshot_history_mode_override $ snapshot_no_check))

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
            `Ok ())
          else
            let failures = ref [] in
            List.iter
              (fun svc ->
                let instance = svc.S.instance in
                let role = svc.S.role in
                Format.printf "Purging instance '%s' (%s)...@." instance role ;
                match Installer.purge_service ~instance with
                | Ok () ->
                    Format.printf "  ✓ Successfully purged '%s'@." instance
                | Error (`Msg msg) ->
                    Format.eprintf "  ✗ Failed to purge '%s': %s@." instance msg ;
                    failures := (instance, msg) :: !failures)
              services ;
            if !failures = [] then (
              Format.printf "@.All instances purged successfully.@." ;
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
                         ( "human_name",
                           match n.human_name with
                           | Some s -> `String s
                           | None -> `Null );
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
                  (Option.value ~default:"" n.human_name)
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

let snapshots_import_cmd =
  let instance =
    Arg.(
      required
      & opt (some string) None
      & info ["instance"] ~doc:"Node instance to refresh" ~docv:"NAME")
  in
  let snapshot_uri =
    Arg.(
      value
      & opt (some string) None
      & info
          ["snapshot-uri"]
          ~doc:"Path or URL to a snapshot archive"
          ~docv:"URI")
  in
  let snapshot_kind =
    Arg.(
      value
      & opt (some string) None
      & info
          ["snapshot-kind"]
          ~doc:"Snapshot slug to fetch from tzinit (rolling, full, full50, ...)"
          ~docv:"KIND")
  in
  let network =
    Arg.(
      value
      & opt (some string) None
      & info
          ["network"]
          ~doc:"Override the network alias/URL used to reach tzinit"
          ~docv:"NET")
  in
  let history_mode =
    Arg.(
      value
      & opt (some history_mode_conv) None
      & info
          ["history-mode"]
          ~doc:"Override history mode when resolving --snapshot-kind"
          ~docv:"MODE")
  in
  let no_check =
    Arg.(
      value & flag
      & info ["no-check"] ~doc:"Pass --no-check to octez-node snapshot import")
  in
  let make instance snapshot_uri snapshot_kind network history_mode no_check =
    run_result
      (Installer.import_snapshot_for_instance
         ~instance
         ?snapshot_uri
         ?snapshot_kind
         ?network
         ?history_mode
         ~no_check
         ())
  in
  let term =
    Term.(
      ret
        (const make $ instance $ snapshot_uri $ snapshot_kind $ network
       $ history_mode $ no_check))
  in
  let info =
    Cmd.info
      "import"
      ~doc:
        "Stop the node, (re)import a snapshot, and restart the service if it \
         was running."
  in
  Cmd.v info term

let snapshots_cmd =
  let info = Cmd.info "snapshots" ~doc:"Snapshot management helpers" in
  Cmd.group info [snapshots_import_cmd]

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
      install_signer_cmd;
      install_dal_node_cmd;
      list_cmd;
      purge_all_cmd;
      list_networks_cmd;
      list_snapshots_cmd;
      snapshots_cmd;
      ui_cmd;
    ]

let () = exit (Cmd.eval root_cmd)
