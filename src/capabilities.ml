open Rresult
open Manager_interfaces

module Logger = struct
  type level = Miaou_interfaces.Logger_capability.level =
    | Debug
    | Info
    | Warning
    | Error

  type t = Miaou_interfaces.Logger_capability.t

  let make () =
    let logf level msg =
      let prefix =
        match level with
        | Debug -> "DBG"
        | Info -> "INF"
        | Warning -> "WRN"
        | Error -> "ERR"
      in
      let line = Printf.sprintf "[%s] %s" prefix msg in
      Common.append_debug_log line
    in
    let set_enabled _ = () in
    let set_logfile _ = Ok () in
    {Miaou_interfaces.Logger_capability.logf; set_enabled; set_logfile}
end

module System : System = struct
  include Common
end

module Service_manager : Service_manager = struct
  include Service_registry

  let register = write

  let unregister = remove
end

module Network_explorer : Network_explorer = struct
  let list_networks () = Teztnets.list_networks ()
end

module Snapshot_provider : Snapshot_provider = struct
  include Snapshots
end

module Service_lifecycle_impl = struct
  let start ~role ~service =
    Systemd.start ~role ~instance:service
    |> Result.map_error (function `Msg m -> m)

  let stop ~role ~service =
    Systemd.stop ~role ~instance:service
    |> Result.map_error (function `Msg m -> m)

  let restart ~role ~service =
    Systemd.restart ~role ~instance:service
    |> Result.map_error (function `Msg m -> m)

  let status ~role ~service =
    match Systemd.is_active ~role ~instance:service with
    | Ok true -> Ok `Active
    | Ok false -> Ok `Inactive
    | Error (`Msg m) -> Ok (`Failed m)

  let install_unit ~role ~app_bin_dir ~user =
    let app_bin_dir = Option.value ~default:"" app_bin_dir in
    Systemd.install_unit ~role ~app_bin_dir ~user
    |> Result.map_error (function `Msg m -> m)

  let write_dropin_node ~inst ~data_dir ~app_bin_dir:_ =
    let logging_mode = Logging_mode.default_for ~instance:inst ~role:"node" in
    Systemd.write_dropin_node ~inst ~data_dir ~logging_mode
    |> Result.map_error (function `Msg m -> m)

  let enable_start ~role ~inst =
    Systemd.enable ~role ~instance:inst ~start_now:true
    |> Result.map_error (function `Msg m -> m)

  let enable ~role ~inst =
    Systemd.enable ~role ~instance:inst ~start_now:false
    |> Result.map_error (function `Msg m -> m)

  let disable ~role ~inst =
    Systemd.disable ~role ~instance:inst ~stop_now:false
    |> Result.map_error (function `Msg m -> m)

  let remove_instance_files ~inst ~remove_data =
    Installer.remove_service ~delete_data_dir:remove_data ~instance:inst
    |> Result.map_error (function `Msg m -> m)
end

module Package_manager : Package_manager = struct
  let install_node = Installer.install_node

  let install_daemon = Installer.install_daemon

  let install_baker = Installer.install_baker

  let install_signer = Installer.install_signer
end

module Tezos_node_manager : Tezos_node_manager = struct
  let schedule_refresh = Installer.schedule_refresh

  let unschedule_refresh = Installer.unschedule_refresh

  let generate_secret_key = Installer.generate_secret_key

  let import_snapshot_for_instance = Installer.import_snapshot_for_instance

  let refresh_instance_from_snapshot = Installer.refresh_instance_from_snapshot
end

module Tezos_client_manager : Tezos_client_manager = struct
  let list_keys = Installer.list_keys

  let add_authorized_key = Installer.add_authorized_key
end

module Installer : Installer = struct
  include Package_manager
  include Service_manager
  include Tezos_node_manager
  include Tezos_client_manager

  let remove_service = Installer.remove_service

  let start_service = Installer.start_service

  let stop_service = Installer.stop_service

  let restart_service = Installer.restart_service

  let purge_service = Installer.purge_service
end

let register () =
  Miaou_interfaces.Capability.register
    Installer_capability.key
    (module Installer : Installer) ;
  Miaou_interfaces.Capability.register
    Package_manager_capability.key
    (module Package_manager : Package_manager) ;
  Miaou_interfaces.Capability.register
    Tezos_node_manager_capability.key
    (module Tezos_node_manager : Tezos_node_manager) ;
  Miaou_interfaces.Capability.register
    Tezos_client_manager_capability.key
    (module Tezos_client_manager : Tezos_client_manager) ;
  Miaou_interfaces.Capability.register
    System_capability.key
    (module System : System) ;
  Miaou_interfaces.Capability.register
    Service_manager_capability.key
    (module Service_manager : Service_manager) ;
  Miaou_interfaces.Capability.register
    Network_explorer_capability.key
    (module Network_explorer : Network_explorer) ;
  Miaou_interfaces.Capability.register
    Snapshot_provider_capability.key
    (module Snapshot_provider : Snapshot_provider) ;
  Miaou_interfaces.Logger_capability.set (Logger.make ()) ;
  Miaou_interfaces.Service_lifecycle.register
    (Miaou_interfaces.Service_lifecycle.create
       ~start:Service_lifecycle_impl.start
       ~stop:Service_lifecycle_impl.stop
       ~restart:Service_lifecycle_impl.restart
       ~status:Service_lifecycle_impl.status
       ~install_unit:Service_lifecycle_impl.install_unit
       ~write_dropin_node:Service_lifecycle_impl.write_dropin_node
       ~enable_start:Service_lifecycle_impl.enable_start
       ~enable:Service_lifecycle_impl.enable
       ~disable:Service_lifecycle_impl.disable
       ~remove_instance_files:Service_lifecycle_impl.remove_instance_files)
