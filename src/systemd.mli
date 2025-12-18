include Service_backend.S

val unit_name : Service.role -> string -> string

val cat_unit :
  role:Service.role -> instance:string -> (string, [`Msg of string]) result

val install_unit :
  role:Service.role ->
  app_bin_dir:string ->
  user:string ->
  (unit, [`Msg of string]) result

val write_dropin :
  role:Service.role ->
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

val remove_dropin : role:Service.role -> instance:string -> unit

val install_refresh_timer :
  instance:string ->
  frequency:string ->
  cmd:string ->
  user:string ->
  (unit, [`Msg of string]) result

val remove_refresh_timer : instance:string -> unit

type logrotate_spec = {role : Service.role; paths : string list}

val sync_logrotate : logrotate_spec list -> (unit, [`Msg of string]) result

module For_tests : sig
  val role_binary : Service.role -> string

  val unit_path : Service.role -> string

  val dropin_dir : Service.role -> string -> string

  val dropin_path : Service.role -> string -> string

  val unit_template :
    role:Service.role ->
    app_bin_dir:string ->
    user:string ->
    ?prestart:string ->
    unit ->
    string

  val render_logging_lines : Logging_mode.t -> string list

  val exec_line : Service.role -> string
end
