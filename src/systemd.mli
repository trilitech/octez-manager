(******************************************************************************)
(*                                                                            *)
(* SPDX-License-Identifier: MIT                                               *)
(* Copyright (c) 2025 Nomadic Labs <contact@nomadic-labs.com>                 *)
(*                                                                            *)
(******************************************************************************)

include Service_backend.S

val unit_name : string -> string -> string

val cat_unit :
  role:string -> instance:string -> (string, [`Msg of string]) result

val install_unit :
  ?quiet:bool ->
  role:string ->
  app_bin_dir:string ->
  user:string ->
  unit ->
  (unit, [`Msg of string]) result

val write_dropin :
  ?quiet:bool ->
  role:string ->
  inst:string ->
  data_dir:string ->
  logging_mode:Logging_mode.t ->
  ?extra_paths:string list ->
  unit ->
  (unit, [`Msg of string]) result

val write_dropin_node :
  ?quiet:bool ->
  inst:string ->
  data_dir:string ->
  logging_mode:Logging_mode.t ->
  unit ->
  (unit, [`Msg of string]) result

val remove_dropin : role:string -> instance:string -> unit

type logrotate_spec = {role : string; paths : string list}

val sync_logrotate : logrotate_spec list -> (unit, [`Msg of string]) result

module For_tests : sig
  val role_binary : string -> string

  val unit_path : string -> string

  val dropin_dir : string -> string -> string

  val dropin_path : string -> string -> string

  val unit_template :
    role:string ->
    app_bin_dir:string ->
    user:string ->
    ?prestart:string ->
    unit ->
    string

  val render_logging_lines : Logging_mode.t -> string list

  val exec_line : string -> string
end
