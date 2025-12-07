open Rresult
open Octez_manager_lib

let ( let* ) = Result.bind

let register_pages () =
  Dashboard.register () ;
  Instances.register () ;
  Instance_details.register () ;
  Installers.register () ;
  Install_node_form.register () ;
  Install_baker_form.register () ;
  Snapshots_page.register () ;
  Networks_page.register () ;
  Signers_page.register () ;
  Settings_page.register () ;
  Jobs_page.register ()

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
      | None -> Error (`Msg "Dashboard page missing from registry"))

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
