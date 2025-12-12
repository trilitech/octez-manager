val registry_root : unit -> string

val services_dir : unit -> string

val write : Service.t -> (unit, Rresult.R.msg) result

val list : unit -> (Service.t list, Rresult.R.msg) result

val remove : instance:string -> role:string -> (unit, Rresult.R.msg) result

val find :
  instance:string -> role:string -> (Service.t option, Rresult.R.msg) result
