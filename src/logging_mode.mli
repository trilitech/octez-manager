type t = Journald | File of {path : string; rotate : bool}

val default_for : instance:string -> role:Role.t -> t

val to_string : t -> string

val to_yojson : t -> Yojson.Safe.t

val of_yojson : Yojson.Safe.t -> (t, string) result
