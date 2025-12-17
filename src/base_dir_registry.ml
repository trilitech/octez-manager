(* DEPRECATED: Use Directory_registry directly with dir_type:Client_base_dir

   This module is kept for backward compatibility but now just wraps
   Directory_registry. All new code should use Directory_registry directly. *)

type base_dir_entry = Directory_registry.directory_entry

let add ~path ~linked_services =
  Directory_registry.add ~path ~dir_type:Client_base_dir ~linked_services

let find_by_path path = Directory_registry.find_by_path path

let list () = Directory_registry.list ~dir_type:Client_base_dir ()

let remove path = Directory_registry.remove path
