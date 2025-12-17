(*****************************************************************************)
(*                                                                           *)
(* SPDX-License-Identifier: MIT                                              *)
(* Copyright (c) 2025 Nomadic Labs <contact@nomadic-labs.com>                *)
(*                                                                           *)
(*****************************************************************************)

(** Node installation form using field bundles.

    Demonstrates bundle composition for the most complex form with snapshot
    import, caching, and preserve_data flow. *)

open Octez_manager_lib
open Installer_types
open Rresult

let ( let* ) = Result.bind

let name = "install_node_form_v3"

(** {1 Custom Types} *)

type tzinit_snapshot = {
  network_slug : string;
  kind_slug : string;
  label : string;
}

type snapshot_selection = [`None | `Url of string | `Tzinit of tzinit_snapshot]
type preserve_data = [`Auto | `Keep | `Refresh]

(** {1 Model} *)

type model = {
  core : Form_builder_common.core_service_config;
  node : Form_builder_common.node_config;
  (* Node-specific fields *)
  snapshot : snapshot_selection;
  preserve_data : preserve_data;
}

let initial_model = {
  core = {
    instance_name = "node";
    service_user = Form_builder_common.default_service_user ();
    app_bin_dir = "/usr/bin";
    logging = `File;
    enable_on_boot = true;
    start_now = true;
    extra_args = "";
  };
  node = {
    network = "mainnet";
    history_mode = "rolling";
    data_dir = "";
    rpc_addr = "127.0.0.1:8732";
    p2p_addr = "0.0.0.0:9732";  (* All interfaces for peer reachability *)
  };
  snapshot = `None;
  preserve_data = `Auto;
}

(** {1 Helper Functions} *)

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

let has_octez_node_binary dir =
  let trimmed = String.trim dir in
  if trimmed = "" then false
  else
    let candidate = Filename.concat trimmed "octez-node" in
    Sys.file_exists candidate
    &&
      try
        Unix.access candidate [Unix.X_OK] ;
        true
      with Unix.Unix_error _ -> false

(** {1 Snapshot Cache} *)

let snapshot_cache : (string, Snapshots.entry list * float) Hashtbl.t =
  Hashtbl.create 7

let snapshot_cache_lock = Mutex.create ()
let snapshot_cache_ttl = 300.  (* 5 minutes *)
let snapshot_inflight : (string, unit) Hashtbl.t = Hashtbl.create 7

let fetch_snapshot_list slug =
  match Snapshots.list ~network_slug:slug with
  | Ok entries -> Ok entries
  | Error (`Msg e) -> Error e

let cache_snapshot slug entries =
  Mutex.protect snapshot_cache_lock (fun () ->
      Hashtbl.replace snapshot_cache slug (entries, Unix.gettimeofday ()))

let schedule_snapshot_fetch slug =
  let should_fetch =
    Mutex.protect snapshot_cache_lock (fun () ->
        if not (Hashtbl.mem snapshot_inflight slug) then (
          Hashtbl.add snapshot_inflight slug () ;
          true)
        else false)
  in
  if should_fetch then
    Background_runner.submit_blocking (fun () ->
        Fun.protect
          ~finally:(fun () ->
            Mutex.protect snapshot_cache_lock (fun () ->
                Hashtbl.remove snapshot_inflight slug))
          (fun () ->
            match fetch_snapshot_list slug with
            | Ok entries -> cache_snapshot slug entries
            | Error msg ->
                prerr_endline
                  (Printf.sprintf "Background snapshot fetch failed: %s" msg)))

let snapshot_entries_from_cache slug =
  Mutex.protect snapshot_cache_lock (fun () ->
      match Hashtbl.find_opt snapshot_cache slug with
      | Some (entries, ts) ->
          if Unix.gettimeofday () -. ts > snapshot_cache_ttl then
            schedule_snapshot_fetch slug ;
          Some entries
      | None -> None)

let prefetch_snapshot_list network =
  match Snapshots.slug_of_network network with
  | Some slug -> (
      match snapshot_entries_from_cache slug with
      | Some _ -> ()
      | None -> schedule_snapshot_fetch slug)
  | None -> ()

(** {1 Custom Fields} *)

let snapshot_field =
  Form_builder.custom
    ~label:"Snapshot Import"
    ~get:(fun m ->
      match m.snapshot with
      | `None -> "None"
      | `Url url -> if url = "" then "Custom URL..." else url
      | `Tzinit snap -> Printf.sprintf "tzinit · %s (%s)" snap.label snap.kind_slug)
    ~edit:(fun model_ref ->
      let snapshots_opt =
        match Snapshots.slug_of_network (!model_ref).node.network with
        | Some slug -> snapshot_entries_from_cache slug
        | None -> None
      in

      let items =
        match snapshots_opt with
        | Some entries ->
            `None :: (entries |> List.map (fun e -> `Tzinit e)) @ [`Custom]
        | None ->
            [`None; `Custom]
      in

      let to_string = function
        | `None -> "None (manual sync)"
        | `Custom -> "Custom URL..."
        | `Tzinit e ->
            Printf.sprintf "%s (%s)"
              e.Snapshots.label
              e.Snapshots.slug
      in

      let on_select choice =
        match choice with
        | `None -> model_ref := {!model_ref with snapshot = `None}
        | `Tzinit e ->
            let snap = {
              network_slug = e.Snapshots.network;
              kind_slug = e.Snapshots.slug;
              label = e.Snapshots.label;
            } in
            model_ref := {!model_ref with snapshot = `Tzinit snap}
        | `Custom ->
            Modal_helpers.prompt_text_modal
              ~title:"Snapshot URL"
              ~placeholder:(Some "https://...")
              ~initial:(match !model_ref.snapshot with `Url u -> u | _ -> "")
              ~on_submit:(fun url -> model_ref := {!model_ref with snapshot = `Url url})
              ()
      in
      Modal_helpers.open_choice_modal ~title:"Import Snapshot" ~items ~to_string ~on_select)
    ()

(** {1 Form Specification} *)

let spec =
  let open Form_builder in
  let open Form_builder_bundles in
  {
    title = " Install Node ";
    initial_model;

    fields =
      [
        (* Instance name with auto-update of data_dir *)
        validated_text
          ~label:"Instance Name"
          ~get:(fun m -> m.core.instance_name)
          ~set:(fun instance_name m ->
            let old = m.core.instance_name in
            let default_dir = Common.default_role_dir "node" instance_name in
            let keep_data_dir =
              String.trim m.node.data_dir <> ""
              && not (String.equal m.node.data_dir (Common.default_role_dir "node" old))
            in
            let new_core = {m.core with instance_name} in
            let new_node = {m.node with data_dir = (if keep_data_dir then m.node.data_dir else default_dir)} in
            {m with core = new_core; node = new_node})
          ~validate:(fun m ->
            let states = Data.load_service_states () in
            if not (Form_builder_common.is_nonempty m.core.instance_name) then
              Error "Instance name is required"
            else if Form_builder_common.instance_in_use ~states m.core.instance_name then
              Error "Instance name already exists"
            else Ok ());
      ]
      @ node_fields
          ~get_node:(fun m -> m.node)
          ~set_node:(fun node m -> {m with node})
          ()
      @ [
        snapshot_field;
      ]
      @ core_service_fields
          ~get_core:(fun m -> m.core)
          ~set_core:(fun core m -> {m with core})
          ~binary:"octez-node"
          ~subcommand:["run"]
          ~binary_validator:has_octez_node_binary
          ~skip_instance_name:true  (* We define instance_name manually above with custom logic *)
          ();

    pre_submit = None;
    on_init = Some (fun model -> prefetch_snapshot_list model.node.network);
    on_refresh = None;

    pre_submit_modal = Some (fun model ->
      (* Check if data_dir exists and has content *)
      let trimmed_dir = String.trim model.node.data_dir in
      if trimmed_dir = "" then None
      else
        let data_exists = Sys.file_exists trimmed_dir in
        if data_exists && Sys.is_directory trimmed_dir then
          let is_empty =
            try
              let entries = Sys.readdir trimmed_dir in
              Array.length entries = 0
            with _ -> false
          in
          if is_empty then None
          else
            Some (Form_builder.PreSubmitModal {
              title = "Data Directory Exists";
              message = Some (Printf.sprintf "Directory '%s' already exists with data." trimmed_dir);
              choices = [`Refresh; `Keep];
              to_string = (function
                | `Refresh -> "Refresh (import snapshot)"
                | `Keep -> "Keep (preserve existing)");
              on_choice = (fun (choice : [`Refresh | `Keep]) model ->
                {model with preserve_data = (choice :> preserve_data)});
            })
        else None);

    on_submit = (fun model ->
      let history_mode =
        match History_mode.of_string model.node.history_mode with
        | Ok hm -> hm
        | Error _ -> History_mode.default
      in

      let logging_mode =
        match model.core.logging with
        | `Journald -> Logging_mode.Journald
        | `File ->
            let dir = Common.default_log_dir ~role:"node" ~instance:model.core.instance_name in
            let path = Filename.concat dir "node.log" in
            Logging_mode.File {path; rotate = true}
      in

      let extra_args = Form_builder_common.prepare_extra_args model.core.extra_args in

      let data_dir =
        let trimmed = String.trim model.node.data_dir in
        if trimmed = "" then Common.default_data_dir model.core.instance_name
        else trimmed
      in

      (* Resolve bootstrap method *)
      let bootstrap =
        match model.snapshot with
        | `None -> Genesis
        | `Url url -> Snapshot {src = Some url; kind = None}
        | `Tzinit snap -> Snapshot {src = None; kind = Some snap.kind_slug}
      in

      let req : Installer_types.node_request = {
        instance = model.core.instance_name;
        network = model.node.network;
        history_mode;
        data_dir = Some data_dir;
        rpc_addr = model.node.rpc_addr;
        net_addr = model.node.p2p_addr;
        service_user = model.core.service_user;
        app_bin_dir = model.core.app_bin_dir;
        logging_mode;
        extra_args;
        auto_enable = model.core.enable_on_boot;
        bootstrap;
        preserve_data = model.preserve_data = `Keep;
        snapshot_no_check = false;
      } in

      let* () =
        if Common.is_root () then
          System_user.ensure_service_account ~name:model.core.service_user
        else Ok ()
      in
      let* (module PM) = require_package_manager () in
      let* _service = PM.install_node req in
      Ok ());
  }

module Page = Form_builder.Make(struct
  type nonrec model = model
  let spec = spec
end)

let page : Miaou.Core.Registry.page = (module Page)

let register () =
  if not (Miaou.Core.Registry.exists name) then
    Miaou.Core.Registry.register name page
