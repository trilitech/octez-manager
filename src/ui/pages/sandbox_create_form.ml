(******************************************************************************)
(*                                                                            *)
(* SPDX-License-Identifier: MIT                                               *)
(* Copyright (c) 2026 Nomadic Labs <contact@nomadic-labs.com>                 *)
(*                                                                            *)
(******************************************************************************)

(** Sandbox creation wizard.

    A minimal form that collects the parameters needed to spin up an isolated
    sandbox environment: network, name, binary location, service user, RPC
    address, delegate count, and an optional snapshot URI.

    LAYOUT RULE: rendered by Form_builder — no manual string alignment. *)

open Octez_manager_lib

let name = "sandbox-create"

(* ─── Model ─────────────────────────────────────────────────────────────── *)

type model = {
  network : string;
  sandbox_name : string;
  app_bin_dir : string;
  bin_source : Binary_registry.bin_source option;
  service_user : string;
  rpc_addr : string;
  max_delegates : string;
  snapshot : string;
  num_nodes : string;
  num_bakers : string;
  accuser : bool;
}

let default_rpc_addr () =
  let avoid_rpc, _ = Port_validation.ports_from_services () in
  let avoid = List.map fst avoid_rpc in
  let port = Port_validation.next_free_port ~start:18732 ~avoid in
  Printf.sprintf "127.0.0.1:%d" port

let make_initial_model () =
  {
    network = "https://teztnets.com/shadownet";
    sandbox_name = Sandbox.unique_name ~base:"sandbox";
    app_bin_dir =
      Form_builder_common.default_app_bin_dir ~binary_name:"octez-node";
    bin_source = None;
    service_user = Form_builder_common.default_service_user ();
    rpc_addr = default_rpc_addr ();
    max_delegates = "20";
    snapshot = "";
    num_nodes = "1";
    num_bakers = "1";
    accuser = false;
  }

(* ─── Fields ─────────────────────────────────────────────────────────────── *)

let network_field =
  let open Form_builder in
  custom
    ~label:"Network"
    ~get:(fun m -> m.network)
    ~edit:(fun model_ref ->
      let fallback () =
        Modal_helpers.prompt_text_modal
          ~title:"Network URL"
          ~initial:!model_ref.network
          ~on_submit:(fun network -> model_ref := {!model_ref with network})
          ()
      in
      match Teztnets.list_networks () with
      | Error _ -> fallback ()
      | Ok nets ->
          let sorted =
            List.sort
              (fun (a : Teztnets.network_info) b ->
                String.compare
                  (String.lowercase_ascii a.human_name)
                  (String.lowercase_ascii b.human_name))
              nets
          in
          let items = List.map (fun n -> `Net n) sorted @ [`Custom] in
          let to_string = function
            | `Net (n : Teztnets.network_info) ->
                Printf.sprintf "%s · %s" n.human_name n.network_url
            | `Custom -> "Custom URL or slug..."
          in
          let on_select = function
            | `Net (n : Teztnets.network_info) ->
                model_ref := {!model_ref with network = n.network_url}
            | `Custom -> fallback ()
          in
          Modal_helpers.open_choice_modal
            ~title:"Network"
            ~items
            ~to_string
            ~on_select
            ())
    ()
  |> with_hint
       "Tezos network to sandbox. Press Enter to browse available networks."

let sandbox_name_field =
  Form_builder.(
    validated_text
      ~label:"Sandbox Name"
      ~get:(fun m -> m.sandbox_name)
      ~set:(fun sandbox_name m -> {m with sandbox_name})
      ~validate:(fun m ->
        (* Must satisfy Sandbox_config_registry.is_safe_name, which rejects
           the name with an exception at creation time otherwise. *)
        let name = m.sandbox_name in
        let is_valid_char = function
          | 'a' .. 'z' | 'A' .. 'Z' | '0' .. '9' | '-' | '_' -> true
          | _ -> false
        in
        if not (Form_builder_common.is_nonempty name) then
          Error "Sandbox name is required"
        else if String.length name > 64 then
          Error "Sandbox name must be at most 64 characters"
        else if not (String.for_all is_valid_char name) then
          Error
            "Only alphanumeric characters (a-z, A-Z, 0-9), hyphens (-), and \
             underscores (_) are allowed"
        else Ok ())
    |> with_hint
         "Unique name for this sandbox. Auto-generated if left as default.")

let app_bin_dir_field =
  Form_builder.(
    app_bin_dir
      ~label:"App Bin Dir"
      ~get:(fun m -> m.app_bin_dir)
      ~set:(fun app_bin_dir bin_source m ->
        {m with app_bin_dir; bin_source = Some bin_source})
      ~validate:(fun m ->
        Form_builder_common.has_octez_node_binary m.app_bin_dir)
      ()
    |> with_hint "Directory containing octez-node and octez-baker binaries.")

let service_user_field =
  Form_builder.(
    text
      ~label:"Service User"
      ~get:(fun m -> m.service_user)
      ~set:(fun service_user m -> {m with service_user})
    |> with_hint "System user that runs the sandbox services.")

let rpc_addr_field =
  Form_builder.(
    endpoint
      ~label:"RPC Address"
      ~get:(fun m -> m.rpc_addr)
      ~set:(fun rpc_addr m -> {m with rpc_addr})
      ~default_port:18732
      ()
    |> with_hint
         "Node RPC address (host:port). Auto-assigned to avoid conflicts.")

let max_delegates_field =
  Form_builder.(
    validated_text
      ~label:"Max Delegates"
      ~get:(fun m -> m.max_delegates)
      ~set:(fun max_delegates m -> {m with max_delegates})
      ~validate:(fun m ->
        match int_of_string_opt (String.trim m.max_delegates) with
        | Some n when n > 0 -> Ok ()
        | _ -> Error "Must be a positive integer")
    |> with_hint
         "Number of active delegates to impersonate via yes-wallet. Default: \
          20.")

(* ─── Snapshot Cache ─────────────────────────────────────────────────────── *)

let snapshot_cache =
  Cache.create_safe_keyed ~name:"sandbox-snapshots" ~ttl:60.0 ()

let snapshot_inflight : (string, unit) Hashtbl.t = Hashtbl.create 4

let snapshot_inflight_lock = Mutex.create ()

let schedule_snapshot_fetch slug =
  let should_fetch =
    Mutex.protect snapshot_inflight_lock (fun () ->
        if not (Hashtbl.mem snapshot_inflight slug) then (
          Hashtbl.add snapshot_inflight slug () ;
          true)
        else false)
  in
  if should_fetch then
    Background_runner.submit_blocking (fun () ->
        Fun.protect
          ~finally:(fun () ->
            Mutex.protect snapshot_inflight_lock (fun () ->
                Hashtbl.remove snapshot_inflight slug))
          (fun () ->
            match Snapshots.list ~network_slug:slug with
            | Ok entries -> Cache.set_safe_keyed snapshot_cache slug entries
            | Error (`Msg msg) ->
                Cmd_runner.append_debug_log
                  (Printf.sprintf "Sandbox snapshot fetch: %s" msg)))

let rolling_entries_for_network slug =
  match Cache.get_safe_keyed_cached snapshot_cache slug with
  | None -> None
  | Some entries ->
      let rolling =
        List.filter
          (fun (e : Snapshots.entry) ->
            match e.history_mode with
            | None -> true
            | Some hm -> String.equal (String.lowercase_ascii hm) "rolling")
          entries
      in
      Some rolling

let num_nodes_field =
  Form_builder.(
    validated_text
      ~label:"Num Nodes"
      ~get:(fun m -> m.num_nodes)
      ~set:(fun num_nodes m -> {m with num_nodes})
      ~validate:(fun m ->
        match int_of_string_opt (String.trim m.num_nodes) with
        | Some n when n >= 1 -> Ok ()
        | _ -> Error "Must be an integer ≥ 1")
    |> with_hint
         "Number of nodes to create. Nodes 2+ will peer to node 1. Default: 1.")

let num_bakers_field =
  Form_builder.(
    validated_text
      ~label:"Num Bakers"
      ~get:(fun m -> m.num_bakers)
      ~set:(fun num_bakers m -> {m with num_bakers})
      ~validate:(fun m ->
        match int_of_string_opt (String.trim m.num_bakers) with
        | Some n when n >= 1 -> Ok ()
        | _ -> Error "Must be an integer ≥ 1")
    |> with_hint
         "Number of bakers to create. Delegates are split evenly. Default: 1.")

let accuser_field =
  Form_builder.(
    toggle
      ~label:"Install Accuser"
      ~get:(fun m -> m.accuser)
      ~set:(fun accuser m -> {m with accuser})
    |> with_hint
         "Install an octez-accuser service connected to node 1. Default: off.")

let snapshot_field =
  let open Form_builder in
  custom
    ~label:"Snapshot URI"
    ~get:(fun m ->
      match String.trim m.snapshot with "" -> "Auto-fetch" | url -> url)
    ~edit:(fun model_ref ->
      let slug_opt = Snapshots.slug_of_network !model_ref.network in
      let entries_opt =
        match slug_opt with
        | None -> None
        | Some slug -> (
            match rolling_entries_for_network slug with
            | Some _ as entries -> entries
            | None ->
                schedule_snapshot_fetch slug ;
                None)
      in
      let items =
        match (slug_opt, entries_opt) with
        | Some _, None -> [`Auto; `Loading; `Custom]
        | _, Some entries ->
            (`Auto :: List.map (fun e -> `Entry e) entries) @ [`Custom]
        | None, None -> [`Auto; `Custom]
      in
      let to_string = function
        | `Loading -> Context.render_spinner "Loading snapshots..."
        | `Auto -> "Auto-fetch (recommended)"
        | `Custom -> "Custom URL or file path..."
        | `Entry (e : Snapshots.entry) ->
            Printf.sprintf "%s (%s)" e.label e.slug
      in
      let on_select = function
        | `Loading -> ()
        | `Auto -> model_ref := {!model_ref with snapshot = ""}
        | `Custom ->
            Modal_helpers.prompt_text_modal
              ~title:"Snapshot URI"
              ~placeholder:(Some "https://... or /path/to/snapshot.rolling")
              ~initial:!model_ref.snapshot
              ~on_submit:(fun snapshot ->
                model_ref := {!model_ref with snapshot})
              ()
        | `Entry (e : Snapshots.entry) ->
            let url = Option.value ~default:"" e.download_url in
            model_ref := {!model_ref with snapshot = url}
      in
      Modal_helpers.open_choice_modal
        ~title:"Snapshot"
        ~items
        ~to_string
        ~on_tick:Context.tick_spinner
        ~on_select
        ())
    ()
  |> with_hint
       "Rolling snapshot to import. Auto-fetch picks the best available. Press \
        Enter to browse Teztnets downloads."

(* ─── Spec ──────────────────────────────────────────────────────────────── *)

let spec =
  {
    Form_builder.title = " Create Sandbox ";
    initial_model = make_initial_model;
    fields =
      (fun _model ->
        [
          network_field;
          sandbox_name_field;
          app_bin_dir_field;
          service_user_field;
          rpc_addr_field;
          max_delegates_field;
          num_nodes_field;
          num_bakers_field;
          accuser_field;
          snapshot_field;
        ]);
    on_init = None;
    on_refresh = None;
    pre_submit = None;
    pre_submit_modal = None;
    on_submit =
      (fun model ->
        let bin_source =
          match model.bin_source with
          | Some bs -> bs
          | None -> Binary_registry.Raw_path model.app_bin_dir
        in
        let max_delegates =
          match int_of_string_opt (String.trim model.max_delegates) with
          | Some n when n > 0 -> n
          | _ -> 20
        in
        let num_nodes =
          match int_of_string_opt (String.trim model.num_nodes) with
          | Some n when n >= 1 -> n
          | _ -> 1
        in
        let num_bakers =
          match int_of_string_opt (String.trim model.num_bakers) with
          | Some n when n >= 1 -> n
          | _ -> 1
        in
        let rpc_addr =
          let s = String.trim model.rpc_addr in
          if String.equal s "" then None else Some s
        in
        let snapshot =
          let s = String.trim model.snapshot in
          if String.equal s "" then None else Some s
        in
        let sandbox_name =
          let s = String.trim model.sandbox_name in
          if String.equal s "" then None else Some s
        in
        let description =
          Printf.sprintf
            "Create sandbox %s"
            (Option.value ~default:"sandbox" sandbox_name)
        in
        Job_manager.submit
          ~timeout:None
          ~description
          ~on_complete:(fun _ -> Context.mark_instances_dirty ())
          (fun ~append_log () ->
            Sandbox.create
              ~on_log:(fun msg -> append_log (msg ^ "\n"))
              ~network:model.network
              ?name:sandbox_name
              ?rpc_addr
              ?snapshot
              ~max_delegates
              ~num_nodes
              ~num_bakers
              ~accuser:model.accuser
              ~bin_source
              ~service_user:model.service_user
              ~app_bin_dir:model.app_bin_dir
              ()
            |> Result.map ignore) ;
        Ok ());
  }

(* ─── Registration ──────────────────────────────────────────────────────── *)

module Page = Form_builder.Make (struct
  type nonrec model = model

  let spec = spec
end)

let page : Miaou.Core.Registry.page = (module Page)

let register () =
  if not (Miaou.Core.Registry.exists name) then
    Miaou.Core.Registry.register name page
