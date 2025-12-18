module type S = sig
  val start :
    role:Service.role -> instance:string -> (unit, [> `Msg of string]) result

  val stop :
    role:Service.role -> instance:string -> (unit, [> `Msg of string]) result

  val restart :
    role:Service.role -> instance:string -> (unit, [> `Msg of string]) result

  val enable :
    role:Service.role ->
    instance:string ->
    start_now:bool ->
    (unit, [> `Msg of string]) result

  val disable :
    role:Service.role ->
    instance:string ->
    stop_now:bool ->
    (unit, [> `Msg of string]) result

  val is_active :
    role:Service.role -> instance:string -> (bool, [> `Msg of string]) result

  val is_enabled :
    role:Service.role -> instance:string -> (string, [> `Msg of string]) result

  val status :
    role:Service.role -> instance:string -> (string, [> `Msg of string]) result
end
