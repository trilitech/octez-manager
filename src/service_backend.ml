module type S = sig
  val start :
    role:Role.t -> instance:string -> (unit, [> `Msg of string]) result

  val stop :
    role:Role.t -> instance:string -> (unit, [> `Msg of string]) result

  val restart :
    role:Role.t -> instance:string -> (unit, [> `Msg of string]) result

  val enable :
    role:Role.t ->
    instance:string ->
    start_now:bool ->
    (unit, [> `Msg of string]) result

  val disable :
    role:Role.t ->
    instance:string ->
    stop_now:bool ->
    (unit, [> `Msg of string]) result

  val is_active :
    role:Role.t -> instance:string -> (bool, [> `Msg of string]) result

  val is_enabled :
    role:Role.t -> instance:string -> (string, [> `Msg of string]) result

  val status :
    role:Role.t -> instance:string -> (string, [> `Msg of string]) result
end
