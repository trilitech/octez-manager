(******************************************************************************)
(*                                                                            *)
(* SPDX-License-Identifier: MIT                                               *)
(* Copyright (c) 2026 Nomadic Labs <contact@nomadic-labs.com>                 *)
(*                                                                            *)
(******************************************************************************)

(** Signatory information page - shows CLI commands and available versions *)

open Octez_manager_lib

let name = "signatory_info"

type state = {versions : Signatory_downloader.version_info list option}

type msg = unit

type pstate = state Miaou.Core.Navigation.t

let init () =
  (* Try to fetch versions, but don't block if it fails *)
  let versions =
    match Signatory_downloader.fetch_versions ~include_prerelease:false () with
    | Ok vs -> Some vs
    | Error _ -> None
  in
  Miaou.Core.Navigation.make {versions}

let update ps _ = ps

let view ps ~focus:_ ~size:_ =
  let s = ps.Miaou.Core.Navigation.s in
  let open Miaou_widgets_display.Widgets in
  let header =
    [
      title_highlight " Signatory Installation ";
      "";
      "Signatory is a remote-signing service for Tezos bakers.";
      "";
    ]
  in
  let versions_section =
    match s.versions with
    | Some versions ->
        let version_lines =
          [bold "Available Signatory Versions:"; ""]
          @ (versions
            |> List.filteri (fun i _ -> i < 5)
            (* Show top 5 *)
            |> List.map (fun (v : Signatory_downloader.version_info) ->
                let date_str =
                  match v.release_date with
                  | Some d -> Printf.sprintf " (%s)" d
                  | None -> ""
                in
                Printf.sprintf
                  "  %s v%s%s"
                  (if v.is_prerelease then yellow "●" else green "●")
                  v.version
                  date_str))
        in
        version_lines
    | None ->
        [
          bold "Available Signatory Versions:";
          "";
          red "Failed to fetch versions from GitHub";
        ]
  in
  let cli_section =
    [
      "";
      "";
      bold "To install Signatory, use the CLI:";
      "";
      dim "  # List available versions";
      "  octez-manager binaries list-signatory";
      "";
      dim "  # Download a specific version";
      "  octez-manager binaries download-signatory --version 1.3.1";
      "";
      dim "  # List installed versions";
      "  octez-manager binaries list-managed-signatory";
      "";
      dim "  # Remove a version";
      "  octez-manager binaries remove-signatory --version 1.3.1";
      "";
      "";
      dim "Press Esc to go back";
    ]
  in
  String.concat "\n" (header @ versions_section @ cli_section)

let handle_key ps key ~size:_ =
  match Miaou.Core.Keys.of_string key with
  | Some Miaou.Core.Keys.Escape -> Miaou.Core.Navigation.back ps
  | _ -> ps

let handle_modal_key ps key ~size:_ =
  Miaou.Core.Modal_manager.handle_key key ;
  ps

let has_modal _ = Miaou.Core.Modal_manager.has_active ()

let move _ _ = assert false

let service_select _ _ = assert false

let service_cycle _ _ = assert false

let back ps = Miaou.Core.Navigation.back ps

let handled_keys () = Miaou.Core.Keys.[Escape]

let keymap _ =
  let noop ps = ps in
  let kb key help =
    {Miaou.Core.Tui_page.key; action = noop; help; display_only = true}
  in
  [kb "Esc" "Back"; kb "?" "Help"]

module Page_Impl :
  Miaou.Core.Tui_page.PAGE_SIG with type state = state and type msg = msg =
struct
  type nonrec state = state

  type nonrec msg = msg

  type nonrec pstate = pstate

  type key_binding = state Miaou.Core.Tui_page.key_binding_desc

  let init = init

  let update = update

  let refresh ps = ps

  let view = view

  let handle_key = handle_key

  let handle_modal_key = handle_modal_key

  let has_modal = has_modal

  let move = move

  let service_select = service_select

  let service_cycle = service_cycle

  let back = back

  let handled_keys = handled_keys

  let keymap = keymap

  let on_key ps key ~size =
    let ps' = handle_key ps (Miaou.Core.Keys.to_string key) ~size in
    (ps', Miaou_interfaces.Key_event.Handled)

  let on_modal_key ps key ~size =
    let ps' = handle_modal_key ps (Miaou.Core.Keys.to_string key) ~size in
    (ps', Miaou_interfaces.Key_event.Handled)

  let key_hints _ps =
    Miaou.Core.Tui_page.
      [{key = "Esc"; help = "Back"}; {key = "?"; help = "Help"}]
end

let page : Miaou.Core.Registry.page =
  (module struct
    include Page_Impl
  end)

let register () =
  if not (Miaou.Core.Registry.exists name) then
    Miaou.Core.Registry.register name page
