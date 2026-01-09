(******************************************************************************)
(*                                                                            *)
(* SPDX-License-Identifier: MIT                                               *)
(* Copyright (c) 2025 Nomadic Labs <contact@nomadic-labs.com>                 *)
(*                                                                            *)
(******************************************************************************)

type bootstrap = Genesis | Snapshot of {src : string option}

type resolved_data_dir_config = {
  network : string;
  history_mode : History_mode.t;
  rpc_addr : string;
  net_addr : string;
}

let pp_resolved_data_dir_config fmt {network; history_mode; rpc_addr; net_addr}
    =
  Format.fprintf
    fmt
    "Network: %s, History-mode: %s, Rpc-addr: %s, Net-addr: %s"
    network
    (History_mode.to_string history_mode)
    rpc_addr
    net_addr

type node_request = {
  instance : string;
  network : string;
  history_mode : History_mode.t;
  data_dir : string option;
  rpc_addr : string;
  net_addr : string;
  service_user : string;
  app_bin_dir : string;
  logging_mode : Logging_mode.t;
  extra_args : string list;
  auto_enable : bool;
  bootstrap : bootstrap;
  preserve_data : bool;
  snapshot_no_check : bool;
}

type daemon_request = {
  role : string;
  instance : string;
  network : string;
  history_mode : History_mode.t;
  data_dir : string;
  rpc_addr : string;
  net_addr : string;
  service_user : string;
  app_bin_dir : string;
  logging_mode : Logging_mode.t;
  service_args : string list;
  extra_env : (string * string) list;
  extra_paths : string list;
  auto_enable : bool;
  depends_on : string option;
  preserve_data : bool;
}

type baker_node_mode =
  | Local_instance of string  (** instance name *)
  | Remote_endpoint of string  (** endpoint *)

type resolved_baker_node_mode = Local of Service.t | Remote of string

type dal_config = Dal_auto | Dal_disabled | Dal_endpoint of string

type baker_request = {
  instance : string;
  node_mode : baker_node_mode;
  base_dir : string option;
  delegates : string list;
  dal_config : dal_config;
  liquidity_baking_vote : string option;
  extra_args : string list;
  service_user : string;
  app_bin_dir : string;
  logging_mode : Logging_mode.t;
  auto_enable : bool;
  preserve_data : bool;
}

type accuser_request = {
  instance : string;
  node_mode : baker_node_mode;
  base_dir : string option;
  extra_args : string list;
  service_user : string;
  app_bin_dir : string;
  logging_mode : Logging_mode.t;
  auto_enable : bool;
  preserve_data : bool;
}

type snapshot_file = {path : string; cleanup : bool}

type snapshot_resolution = {
  download_url : string;
  network_slug : string;
  kind_slug : string;
}

type snapshot_plan =
  | No_snapshot
  | Direct_snapshot of {uri : string}
  | Tzinit_snapshot of snapshot_resolution

type snapshot_metadata = {
  auto : bool;
  uri : string option;
  network_slug : string option;
  kind_slug : string option;
  no_check : bool;
}

type file_backup = {tmp_path : string; original_path : string}
