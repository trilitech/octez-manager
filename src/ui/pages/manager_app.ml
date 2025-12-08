(*****************************************************************************)
(*                                                                           *)
(* SPDX-License-Identifier: MIT                                              *)
(* Copyright (c) 2025 Nomadic Labs <contact@nomadic-labs.com>                *)
(*                                                                           *)
(*****************************************************************************)

open Rresult
open Octez_manager_lib

let ( let* ) = Result.bind

let register_pages () =
  Instances.register () ;
  Instance_details.register () ;
  Install_node_form.register () ;
  Install_baker_form.register ()

let find_page_or_default name default_name =
  let module Registry = Miaou.Core.Registry in
  match Registry.find name with
  | Some page -> Ok page
  | None -> (
      match Registry.find default_name with
      | Some page ->
          prerr_endline
            (Printf.sprintf
               "Unknown page '%s', falling back to '%s'"
               name
               default_name) ;
          Ok page
      | None -> Error (`Msg "Instances page missing from registry"))

let run ?page ?(log = false) ?logfile () =
  Capabilities.register () ;
  register_pages () ;
  Runtime.initialize ~log ?logfile () ;
  let start_name = Option.value ~default:Instances.name page in
  let rec loop history current_name =
    let* current_page = find_page_or_default current_name Instances.name in
    match Miaou.Core.Lambda_term_driver.run current_page with
    | `Quit -> Ok ()
    | `SwitchTo "__BACK__" -> (
        match history with [] -> Ok () | prev :: rest -> loop rest prev)
    | `SwitchTo next_page -> loop (current_name :: history) next_page
  in
  loop [] start_name
