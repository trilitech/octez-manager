(******************************************************************************)
(*                                                                            *)
(* SPDX-License-Identifier: MIT                                               *)
(* Copyright (c) 2025 Nomadic Labs <contact@nomadic-labs.com>                 *)
(*                                                                            *)
(******************************************************************************)

open Installer_types

module type Service_manager = sig
  val list : unit -> (Service.t list, [`Msg of string]) result

  val find : instance:string -> (Service.t option, [`Msg of string]) result

  val register : Service.t -> (unit, [`Msg of string]) result

  val unregister : instance:string -> (unit, [`Msg of string]) result
end

module Service_manager_capability = struct
  type t = (module Service_manager)

  let key : t Miaou_interfaces.Capability.key =
    Miaou_interfaces.Capability.create ~name:"service_manager"
end

module type Package_manager = sig
  val install_node :
    ?quiet:bool ->
    ?on_log:(string -> unit) ->
    node_request ->
    (Service.t, [`Msg of string]) result

  val install_daemon :
    ?quiet:bool -> daemon_request -> (Service.t, [`Msg of string]) result

  val install_baker :
    ?quiet:bool -> baker_request -> (Service.t, [`Msg of string]) result
end

module Package_manager_capability = struct
  type t = (module Package_manager)

  let key : t Miaou_interfaces.Capability.key =
    Miaou_interfaces.Capability.create ~name:"package_manager"
end

module type Tezos_node_manager = sig end

module Tezos_node_manager_capability = struct
  type t = (module Tezos_node_manager)

  let key : t Miaou_interfaces.Capability.key =
    Miaou_interfaces.Capability.create ~name:"tezos_node_manager"
end

module type Tezos_client_manager = sig end

module Tezos_client_manager_capability = struct
  type t = (module Tezos_client_manager)

  let key : t Miaou_interfaces.Capability.key =
    Miaou_interfaces.Capability.create ~name:"tezos_client_manager"
end

module type Installer = sig
  include Package_manager

  include Service_manager

  include Tezos_node_manager

  include Tezos_client_manager

  (* Legacy monolithic interface, to be deprecated or kept for CLI convenience *)
  val remove_service :
    ?quiet:bool ->
    delete_data_dir:bool ->
    instance:string ->
    unit ->
    (unit, [`Msg of string]) result

  val start_service :
    ?quiet:bool -> instance:string -> unit -> (unit, [`Msg of string]) result

  val stop_service :
    ?quiet:bool -> instance:string -> unit -> (unit, [`Msg of string]) result

  val restart_service :
    ?quiet:bool -> instance:string -> unit -> (unit, [`Msg of string]) result

  val purge_service :
    ?quiet:bool ->
    prompt_yes_no:(string -> default:bool -> bool) ->
    instance:string ->
    unit ->
    (unit, [`Msg of string]) result
end

module Installer_capability = struct
  type t = (module Installer)

  let key : t Miaou_interfaces.Capability.key =
    Miaou_interfaces.Capability.create ~name:"installer"
end

module type System = sig
  val is_root : unit -> bool

  val home_dir : unit -> string

  val xdg_config_home : unit -> string

  val xdg_data_home : unit -> string

  val xdg_state_home : unit -> string

  val which : string -> string option

  val ensure_dir_path :
    owner:string ->
    group:string ->
    mode:int ->
    string ->
    (unit, [`Msg of string]) result

  val write_file :
    mode:int ->
    owner:string ->
    group:string ->
    string ->
    string ->
    (unit, [`Msg of string]) result

  val run :
    ?quiet:bool ->
    ?on_log:(string -> unit) ->
    string list ->
    (unit, [`Msg of string]) result

  val run_out : string list -> (string, [`Msg of string]) result

  val run_as :
    ?quiet:bool ->
    ?on_log:(string -> unit) ->
    user:string ->
    string list ->
    (unit, [`Msg of string]) result

  val copy_file : string -> string -> (unit, [`Msg of string]) result

  val remove_path : string -> unit

  val remove_tree : string -> (unit, [`Msg of string]) result

  val is_port_in_use : int -> bool
end

module System_capability = struct
  type t = (module System)

  let key : t Miaou_interfaces.Capability.key =
    Miaou_interfaces.Capability.create ~name:"system"
end

module type Network_explorer = sig
  val list_networks :
    unit -> (Teztnets.network_info list, [`Msg of string]) result
end

module Network_explorer_capability = struct
  type t = (module Network_explorer)

  let key : t Miaou_interfaces.Capability.key =
    Miaou_interfaces.Capability.create ~name:"network_explorer"
end

module type Snapshot_provider = sig
  val list :
    network_slug:string -> (Snapshots.entry list, [`Msg of string]) result

  val fetch_entry :
    network_slug:string ->
    slug:string ->
    label:string ->
    (Snapshots.entry option, [`Msg of string]) result

  val slug_of_network : string -> string option
end

module Snapshot_provider_capability = struct
  type t = (module Snapshot_provider)

  let key : t Miaou_interfaces.Capability.key =
    Miaou_interfaces.Capability.create ~name:"snapshot_provider"
end
