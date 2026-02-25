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
}

let default_rpc_addr () =
  let avoid_rpc, _ = Port_validation.ports_from_services () in
  let avoid = List.map fst avoid_rpc in
  let port = Port_validation.next_free_port ~start:18732 ~avoid in
  Printf.sprintf "127.0.0.1:%d" port

let make_initial_model () =
  {
    network = "https://teztnets.com/ghostnet";
    sandbox_name = Sandbox.unique_name ~base:"sandbox";
    app_bin_dir =
      Form_builder_common.default_app_bin_dir ~binary_name:"octez-node";
    bin_source = None;
    service_user = Form_builder_common.default_service_user ();
    rpc_addr = default_rpc_addr ();
    max_delegates = "20";
    snapshot = "";
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
    text
      ~label:"Sandbox Name"
      ~get:(fun m -> m.sandbox_name)
      ~set:(fun sandbox_name m -> {m with sandbox_name})
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

let snapshot_field =
  Form_builder.(
    text
      ~label:"Snapshot URI"
      ~get:(fun m -> m.snapshot)
      ~set:(fun snapshot m -> {m with snapshot})
    |> with_hint
         "Optional snapshot URL or file path. Leave empty to auto-fetch.")

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
        Job_manager.submit ~timeout:None ~description (fun ~append_log () ->
            Sandbox.create
              ~on_log:(fun msg -> append_log (msg ^ "\n"))
              ~network:model.network
              ?name:sandbox_name
              ?rpc_addr
              ?snapshot
              ~max_delegates
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
