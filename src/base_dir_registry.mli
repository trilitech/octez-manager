(* DEPRECATED: Use Directory_registry directly with dir_type:Client_base_dir *)

type base_dir_entry = Directory_registry.directory_entry

val add :
  path:string -> linked_services:string list -> (unit, [`Msg of string]) result

val find_by_path : string -> (base_dir_entry option, [`Msg of string]) result

val list : unit -> (base_dir_entry list, [`Msg of string]) result

val remove : string -> (unit, [`Msg of string]) result
