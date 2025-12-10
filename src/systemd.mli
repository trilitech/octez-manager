include Service_backend.S

val unit_name : string -> string -> string

val cat_unit :
  role:string -> instance:string -> (string, [`Msg of string]) result

val install_unit :
  role:string ->
  app_bin_dir:string ->
  user:string ->
  (unit, [`Msg of string]) result

val write_dropin :
  role:string ->
  inst:string ->
  data_dir:string ->
  logging_mode:Logging_mode.t ->
  ?extra_paths:string list ->
  unit ->
  (unit, [`Msg of string]) result

val write_dropin_node :
  inst:string ->
  data_dir:string ->
  logging_mode:Logging_mode.t ->
  (unit, [`Msg of string]) result

val remove_dropin : role:string -> instance:string -> unit

val install_refresh_timer :
  instance:string ->
  frequency:string ->
  cmd:string ->
  user:string ->
  (unit, [`Msg of string]) result

val remove_refresh_timer : instance:string -> unit

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

  val disable_user_logrotate_timer : unit -> unit
end
