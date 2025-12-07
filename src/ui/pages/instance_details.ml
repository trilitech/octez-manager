module Widgets = Miaou_widgets_display.Widgets
module Vsection = Miaou_widgets_layout.Vsection
module Keys = Miaou.Core.Keys
open Octez_manager_lib
open Installer_types
open Rresult

let ( let* ) = Result.bind

let name = "instance_details"

type state = {
  instance : string;
  service : Service.t option;
  error : string option;
  next_page : string option;
}

type msg = unit

let load_service instance =
  match Service_registry.find ~instance with
  | Ok (Some svc) -> Ok svc
  | Ok None -> Error ("Instance not found: " ^ instance)
  | Error (`Msg e) -> Error e

let init () =
  match Context.take_pending_instance_detail () with
  | Some instance -> (
      match load_service instance with
      | Ok service ->
          {instance; service = Some service; error = None; next_page = None}
      | Error e -> {instance; service = None; error = Some e; next_page = None})
  | None ->
      {
        instance = "";
        service = None;
        error = Some "No instance selected";
        next_page = None;
      }

let update s _ = s

let refresh s = s

let move s _ = s

let enter s = s

let service_select s _ = s

let service_cycle s _ = s

let back s = {s with next_page = Some "__BACK__"}

let keymap _ = [("Esc", back, "Back")]

let header s =
  [
    Widgets.title_highlight (" Instance Details · " ^ s.instance);
    (match s.service with
    | Some svc -> Widgets.dim (svc.Service.role ^ " @ " ^ svc.Service.network)
    | None -> "");
  ]

let footer = [Widgets.dim "Enter: actions  Esc: back"]

let view_details svc =
  let field label value =
    Printf.sprintf "%s %s" (Widgets.dim (Printf.sprintf "%-15s" label)) value
  in
  [
    field "Instance" svc.Service.instance;
    field "Role" svc.Service.role;
    field "Network" svc.Service.network;
    field "History Mode" (History_mode.to_string svc.Service.history_mode);
    field "Data Dir" svc.Service.data_dir;
    field "RPC Addr" svc.Service.rpc_addr;
    field "P2P Addr" svc.Service.net_addr;
    field "Service User" svc.Service.service_user;
    field "Bin Dir" svc.Service.app_bin_dir;
    field "Created At" svc.Service.created_at;
    field
      "Logging"
      (match svc.Service.logging_mode with
      | Logging_mode.Journald -> "journald"
      | Logging_mode.File {path; rotate} ->
          Printf.sprintf "file:%s (rotate=%b)" path rotate);
    field "Extra Args" (String.concat " " svc.Service.extra_args);
  ]

let view s ~focus:_ ~size =
  let body =
    match (s.error, s.service) with
    | Some err, _ -> [Widgets.red ("Error: " ^ err)]
    | None, Some svc -> view_details svc
    | None, None -> ["Loading..."]
  in
  Vsection.render ~size ~header:(header s) ~footer ~child:(fun _ ->
      String.concat "\n" body)

let handle_modal_key s key ~size:_ =
  Miaou.Core.Modal_manager.handle_key key ;
  s

let require_package_manager () =
  match
    Miaou_interfaces.Capability.get
      Manager_interfaces.Package_manager_capability.key
  with
  | Some cap ->
      let module I =
        (val (cap : Manager_interfaces.Package_manager_capability.t))
      in
      Ok (module I : Manager_interfaces.Package_manager)
  | None -> Error (`Msg "Package manager capability not available")

let apply_node_update s update_fn =
  match s.service with
  | None -> s
  | Some svc -> (
      let req =
        {
          instance = svc.Service.instance;
          network = svc.Service.network;
          history_mode = svc.Service.history_mode;
          data_dir = Some svc.Service.data_dir;
          rpc_addr = svc.Service.rpc_addr;
          net_addr = svc.Service.net_addr;
          service_user = svc.Service.service_user;
          app_bin_dir = svc.Service.app_bin_dir;
          logging_mode = svc.Service.logging_mode;
          extra_args = svc.Service.extra_args;
          auto_enable = true;
          bootstrap = Genesis;
        }
      in
      let req = update_fn req in
      let res =
        let* (module PM) = require_package_manager () in
        PM.install_node req
      in
      match res with
      | Ok new_svc ->
          Modal_helpers.show_success ~title:"Success" "Configuration updated." ;
          {s with service = Some new_svc}
      | Error (`Msg e) ->
          Modal_helpers.show_error ~title:"Error" e ;
          s)

let edit_config_modal s =
  match s.service with
  | None -> s
  | Some svc ->
      if svc.Service.role <> "node" then (
        Modal_helpers.show_error
          ~title:"Not Supported"
          "Editing is currently only supported for nodes." ;
        s)
      else (
        Modal_helpers.open_choice_modal
          ~title:("Edit Config · " ^ svc.Service.instance)
          ~items:[`ExtraArgs; `LoggingMode]
          ~to_string:(function
            | `ExtraArgs -> "Extra Arguments" | `LoggingMode -> "Logging Mode")
          ~on_select:(fun choice ->
            match choice with
            | `ExtraArgs ->
                let initial = String.concat " " svc.Service.extra_args in
                Modal_helpers.prompt_text_modal
                  ~title:"Extra Arguments"
                  ~initial
                  ~on_submit:(fun text ->
                    let extra_args =
                      String.split_on_char ' ' text
                      |> List.map String.trim
                      |> List.filter (( <> ) "")
                    in
                    ignore
                      (apply_node_update s (fun req -> {req with extra_args})))
                  ()
            | `LoggingMode ->
                Modal_helpers.open_choice_modal
                  ~title:"Select Logging Mode"
                  ~items:[`Journald; `File]
                  ~to_string:(function
                    | `Journald -> "Journald" | `File -> "File (rotate)")
                  ~on_select:(fun mode ->
                    match mode with
                    | `Journald ->
                        ignore
                          (apply_node_update s (fun req ->
                               {req with logging_mode = Logging_mode.Journald}))
                    | `File ->
                        Modal_helpers.prompt_text_modal
                          ~title:"Log File Path"
                          ~initial:
                            (Common.default_log_dir
                               ~role:"node"
                               ~instance:svc.Service.instance
                            ^ "/node.log")
                          ~on_submit:(fun path ->
                            let logging_mode =
                              Logging_mode.File {path; rotate = true}
                            in
                            ignore
                              (apply_node_update s (fun req ->
                                   {req with logging_mode})))
                          ())) ;
        s)

let open_actions_modal s =
  match s.service with
  | None -> s
  | Some svc ->
      Modal_helpers.open_choice_modal
        ~title:("Actions · " ^ svc.Service.instance)
        ~items:[`EditConfig; `Overrides]
        ~to_string:(function
          | `EditConfig -> "Edit Configuration"
          | `Overrides -> "Service Overrides")
        ~on_select:(fun choice ->
          match choice with
          | `EditConfig -> ignore (edit_config_modal s)
          | `Overrides ->
              Modal_helpers.show_error
                ~title:"Not Implemented"
                "Overrides not implemented yet") ;
      s

let handle_key s key ~size:_ =
  if Miaou.Core.Modal_manager.has_active () then (
    Miaou.Core.Modal_manager.handle_key key ;
    s)
  else
    match Keys.of_string key with
    | Some (Keys.Char "Esc") | Some (Keys.Char "q") ->
        {s with next_page = Some "__BACK__"}
    | Some Keys.Enter -> open_actions_modal s
    | _ -> s

let next_page s = s.next_page

let has_modal _ = Miaou.Core.Modal_manager.has_active ()

module Page : Miaou.Core.Tui_page.PAGE_SIG = struct
  type nonrec state = state

  type nonrec msg = msg

  let init = init

  let update = update

  let refresh = refresh

  let move = move

  let enter = enter

  let service_select = service_select

  let service_cycle = service_cycle

  let back = back

  let keymap = keymap

  let view = view

  let handle_key = handle_key

  let handle_modal_key = handle_modal_key

  let next_page = next_page

  let has_modal = has_modal
end

let page : Miaou.Core.Registry.page =
  (module Page : Miaou.Core.Tui_page.PAGE_SIG)

let register () =
  if not (Miaou.Core.Registry.exists name) then
    Miaou.Core.Registry.register name page
