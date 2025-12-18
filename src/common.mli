(******************************************************************************)
(*                                                                            *)
(* SPDX-License-Identifier: MIT                                               *)
(* Copyright (c) 2025 Nomadic Labs <contact@nomadic-labs.com>                 *)
(*                                                                            *)
(******************************************************************************)

val is_root : unit -> bool

val home_dir : unit -> string

val xdg_config_home : unit -> string

val xdg_data_home : unit -> string

val xdg_state_home : unit -> string

val current_user_group_names : unit -> string * string

val env_instances_base_dir : unit -> string

val default_data_dir : string -> string

val default_role_dir : string -> string -> string

val default_log_dir : role:string -> instance:string -> string

val which : string -> string option

val make_absolute_path : string -> (string, string) result

val ensure_dir_path :
  owner:string ->
  group:string ->
  mode:int ->
  string ->
  (unit, Rresult.R.msg) result

val ensure_tree_owner :
  owner:string -> group:string -> string -> (unit, Rresult.R.msg) result

val write_file :
  mode:int ->
  owner:string ->
  group:string ->
  string ->
  string ->
  (unit, Rresult.R.msg) result

val append_debug_log : string -> unit

val run : string list -> (unit, [> `Msg of string]) result

val run_out : string list -> (string, [> `Msg of string]) result

val run_as : user:string -> string list -> (unit, [> `Msg of string]) result

val download_file :
  url:string -> dest_path:string -> (unit, [> `Msg of string]) result

val download_file_with_progress :
  url:string ->
  dest_path:string ->
  on_progress:(int -> int option -> unit) ->
  (unit, [> `Msg of string]) result

(** Kill any active download process and clean up partial file. Call on exit. *)
val kill_active_download : unit -> unit

val sh_quote : string -> string

val cmd_to_string : string list -> string

val remove_path : string -> unit

val remove_tree : string -> (unit, [> `Msg of string]) result

val copy_file : string -> string -> (unit, [> `Msg of string]) result

val is_port_in_use : int -> bool
