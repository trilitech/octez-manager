val register_pages : unit -> unit

val run :
  ?page:string ->
  ?log:bool ->
  ?logfile:string ->
  unit ->
  (unit, [> `Msg of string]) result
