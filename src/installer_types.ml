type bootstrap =
  | Genesis
  | Snapshot of {src : string option; kind : string option}

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
}

type baker_node_mode = [`Auto | `Local | `Remote]

type baker_request = {
  instance : string;
  network : string option;
  node_instance : string option;
  node_data_dir : string option;
  node_endpoint : string option;
  node_mode : baker_node_mode;
  base_dir : string option;
  delegates : string list;
  dal_endpoint : string option;
  extra_args : string list;
  service_user : string;
  app_bin_dir : string;
  logging_mode : Logging_mode.t;
  auto_enable : bool;
}

type signer_request = {
  instance : string;
  network : string;
  base_dir : string option;
  address : string;
  port : int;
  service_user : string;
  app_bin_dir : string;
  logging_mode : Logging_mode.t;
  require_auth : bool;
  password_file : string option;
  auto_enable : bool;
  authorized_keys : (string option * string) list;
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
