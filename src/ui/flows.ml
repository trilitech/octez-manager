(******************************************************************************)
(*                                                                            *)
(* SPDX-License-Identifier: MIT                                               *)
(* Copyright (c) 2025-2026 Nomadic Labs <contact@nomadic-labs.com>            *)
(*                                                                            *)
(******************************************************************************)

open Octez_manager_lib
open Installer_types
open Rresult

let ( let* ) = Result.bind

let is_valid_instance_char c =
  match c with
  | 'a' .. 'z' | 'A' .. 'Z' | '0' .. '9' | '-' | '_' | '.' -> true
  | _ -> false

let instance_has_valid_chars name = String.for_all is_valid_instance_char name

let invalid_instance_name_error_msg =
  "Instance name contains invalid characters. "
  ^ Helpers.invalid_instance_name_chars_msg

(** Strip "node-" prefix from instance name to create cleaner dependent names.
    E.g., "node-shadownet" -> "shadownet" so baker becomes "baker-shadownet" *)
let strip_node_prefix inst =
  if String.starts_with ~prefix:"node-" inst then
    String.sub inst 5 (String.length inst - 5)
  else inst

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

let create_node_flow ~on_success =
  let open Modal_helpers in
  prompt_text_modal
    ~title:"Instance Name"
    ~on_submit:(fun instance ->
      let instance = String.trim instance in
      if instance = "" then
        show_error ~title:"Error" "Instance name cannot be empty"
      else if not (instance_has_valid_chars instance) then
        show_error ~title:"Error" invalid_instance_name_error_msg
      else
        open_choice_modal
          ~title:"Network"
          ~items:["mainnet"; "ghostnet"; "weeklynet"]
          ~to_string:(fun x -> x)
          ~on_select:(fun network ->
            open_choice_modal
              ~title:"History Mode"
              ~items:
                [History_mode.Rolling; History_mode.Full; History_mode.Archive]
              ~to_string:History_mode.to_string
              ~on_select:(fun history_mode ->
                open_choice_modal
                  ~title:"Bootstrap"
                  ~items:[`Genesis; `Snapshot]
                  ~to_string:(function
                    | `Genesis -> "Genesis" | `Snapshot -> "Snapshot (Auto)")
                  ~on_select:(fun bootstrap_choice ->
                    let bootstrap =
                      match bootstrap_choice with
                      | `Genesis -> Genesis
                      | `Snapshot -> Snapshot {src = None}
                    in
                    let request =
                      {
                        instance;
                        network;
                        history_mode;
                        data_dir = None;
                        rpc_addr = "127.0.0.1:8732";
                        net_addr = "0.0.0.0:9732";
                        service_user = "octez";
                        app_bin_dir =
                          Form_builder_common.default_app_bin_dir
                            ~binary_name:"octez-node";
                        logging_mode = Logging_mode.Journald;
                        extra_args = [];
                        auto_enable = true;
                        preserve_data = false;
                        bootstrap;
                        snapshot_no_check = false;
                        tmp_dir = None;
                        keep_snapshot = false;
                      }
                    in
                    let res =
                      let* (module PM) = require_package_manager () in
                      PM.install_node request
                    in
                    match res with
                    | Ok _ ->
                        show_success
                          ~title:"Success"
                          ("Node " ^ instance ^ " created.") ;
                        on_success ()
                    | Error (`Msg e) -> show_error ~title:"Error" e)
                  ())
              ())
          ())
    ()

let create_baker_flow ~services ~on_success =
  let open Modal_helpers in
  let nodes =
    services
    |> List.filter (fun s -> s.Service.role = "node")
    |> List.map (fun s -> s.Service.instance)
  in
  if nodes = [] then
    show_error ~title:"Error" "No nodes available. Create a node first."
  else
    open_choice_modal
      ~title:"Select Parent Node"
      ~items:nodes
      ~to_string:(fun x -> x)
      ~on_select:(fun parent_node ->
        prompt_text_modal
          ~title:"Baker Instance Name"
          ~initial:("baker-" ^ strip_node_prefix parent_node)
          ~on_submit:(fun instance ->
            let instance = String.trim instance in
            if instance = "" then
              show_error ~title:"Error" "Instance name cannot be empty"
            else if not (instance_has_valid_chars instance) then
              show_error ~title:"Error" invalid_instance_name_error_msg
            else
              prompt_text_modal
                ~title:"Delegates (comma separated)"
                ~on_submit:(fun delegates_str ->
                  let delegates =
                    String.split_on_char ',' delegates_str
                    |> List.map String.trim
                    |> List.filter (( <> ) "")
                  in
                  let request =
                    {
                      instance;
                      node_mode = Local_instance parent_node;
                      base_dir = None;
                      delegates;
                      dal_config = Dal_auto;
                      dal_node = None;
                      liquidity_baking_vote = None;
                      extra_args = [];
                      service_user = "octez";
                      app_bin_dir =
                        Form_builder_common.default_app_bin_dir
                          ~binary_name:"octez-baker";
                      logging_mode = Logging_mode.Journald;
                      auto_enable = true;
                      preserve_data = false;
                    }
                  in
                  let res =
                    let* (module PM) = require_package_manager () in
                    PM.install_baker request
                  in
                  match res with
                  | Ok _ ->
                      show_success
                        ~title:"Success"
                        ("Baker " ^ instance ^ " created.") ;
                      on_success ()
                  | Error (`Msg e) -> show_error ~title:"Error" e)
                ())
          ())
      ()

let create_accuser_flow ~on_success =
  let open Modal_helpers in
  prompt_text_modal
    ~title:"Accuser Instance Name"
    ~on_submit:(fun instance ->
      let instance = String.trim instance in
      if instance = "" then
        show_error ~title:"Error" "Instance name cannot be empty"
      else if not (instance_has_valid_chars instance) then
        show_error ~title:"Error" invalid_instance_name_error_msg
      else
        open_choice_modal
          ~title:"Network"
          ~items:["mainnet"; "ghostnet"; "weeklynet"]
          ~to_string:(fun x -> x)
          ~on_select:(fun network ->
            let base_dir = Common.default_role_dir "accuser" instance in
            let node_endpoint = "http://127.0.0.1:8732" in
            let request =
              {
                role = "accuser";
                instance;
                network;
                history_mode = History_mode.default;
                data_dir = base_dir;
                rpc_addr = node_endpoint;
                net_addr = "";
                service_user = "octez";
                app_bin_dir =
                  Form_builder_common.default_app_bin_dir
                    ~binary_name:"octez-baker";
                logging_mode = Logging_mode.Journald;
                service_args = [];
                extra_env =
                  [
                    ("OCTEZ_CLIENT_BASE_DIR", base_dir);
                    ("OCTEZ_NODE_ENDPOINT", node_endpoint);
                  ];
                extra_paths = [base_dir];
                auto_enable = true;
                depends_on = None;
                preserve_data = false;
              }
            in
            let res =
              let* (module PM) = require_package_manager () in
              PM.install_daemon request
            in
            match res with
            | Ok _ ->
                show_success
                  ~title:"Success"
                  ("Accuser " ^ instance ^ " created.") ;
                on_success ()
            | Error (`Msg e) -> show_error ~title:"Error" e)
          ())
    ()

let create_dal_node_flow ~on_success =
  let open Modal_helpers in
  prompt_text_modal
    ~title:"DAL Node Instance Name"
    ~on_submit:(fun instance ->
      let instance = String.trim instance in
      if instance = "" then
        show_error ~title:"Error" "Instance name cannot be empty"
      else if not (instance_has_valid_chars instance) then
        show_error ~title:"Error" invalid_instance_name_error_msg
      else
        open_choice_modal
          ~title:"Network"
          ~items:["mainnet"; "ghostnet"; "weeklynet"]
          ~to_string:(fun x -> x)
          ~on_select:(fun network ->
            let client_base_dir = Common.default_role_dir "dal-node" instance in
            let dal_data_dir = Common.default_role_dir "dal-node" instance in
            let node_endpoint = "http://127.0.0.1:8732" in
            let request =
              {
                role = "dal-node";
                instance;
                network;
                history_mode = History_mode.default;
                data_dir = dal_data_dir;
                rpc_addr = "127.0.0.1:10732";
                net_addr = "0.0.0.0:11732";
                service_user = "octez";
                app_bin_dir =
                  Form_builder_common.default_app_bin_dir
                    ~binary_name:"octez-baker";
                logging_mode = Logging_mode.Journald;
                service_args =
                  [
                    "--rpc-addr";
                    "127.0.0.1:10732";
                    "--net-addr";
                    "0.0.0.0:11732";
                  ];
                extra_env =
                  [
                    ("OCTEZ_CLIENT_BASE_DIR", client_base_dir);
                    ("OCTEZ_NODE_ENDPOINT", node_endpoint);
                    ("OCTEZ_DAL_DATA_DIR", dal_data_dir);
                  ];
                extra_paths = [client_base_dir; dal_data_dir];
                auto_enable = true;
                depends_on = None;
                preserve_data = false;
              }
            in
            let res =
              let* (module PM) = require_package_manager () in
              PM.install_daemon request
            in
            match res with
            | Ok _ ->
                show_success
                  ~title:"Success"
                  ("DAL Node " ^ instance ^ " created.") ;
                on_success ()
            | Error (`Msg e) -> show_error ~title:"Error" e)
          ())
    ()
