(******************************************************************************)
(*                                                                            *)
(* SPDX-License-Identifier: MIT                                               *)
(* Copyright (c) 2026 Nomadic Labs <contact@nomadic-labs.com>                 *)
(*                                                                            *)
(******************************************************************************)

(** Rendering functions for the binaries management page.

    Contains the view function and section renderers for managed versions,
    registered directories, available versions, and download progress. *)

module Box = Miaou_widgets_layout.Box_widget
module Navigation = Miaou.Core.Navigation
module Style_context = Miaou_style.Style_context
open Ui_fmt
open Octez_manager_lib
open Binaries_types

(** Set the help hint based on the currently selected item. *)
let set_help_hint s =
  match List.nth_opt s.items s.selected with
  | Some RegisterAction ->
      Miaou.Core.Help_hint.set
        (Some
           "Registered directories let you use Octez binaries from other \
            locations (dev builds, system installs, custom versions). Press \
            Enter to browse for a directory.")
  | Some (RegisteredDir (_, count)) ->
      if count > 0 then
        Miaou.Core.Help_hint.set
          (Some
             "Press Enter to expand/collapse instances using this directory. \
              Press ? for help.")
      else
        Miaou.Core.Help_hint.set
          (Some "Press Enter to unregister this directory. Press ? for help.")
  | Some (ManagedVersion (_, _, _, count)) ->
      if count > 0 then
        Miaou.Core.Help_hint.set
          (Some
             "Press Enter to expand/collapse instances using this version. \
              Press ? for help.")
      else
        Miaou.Core.Help_hint.set
          (Some "Press Enter to remove this version. Press ? for help.")
  | Some (AvailableVersion (_, _)) | Some (AvailableSignatoryVersion _) ->
      Miaou.Core.Help_hint.set
        (Some "Press Enter to download this version. Press ? for help.")
  | Some (AvailableMajorGroup (_, _)) ->
      Miaou.Core.Help_hint.set
        (Some
           "Press Enter or Tab to expand/collapse version group. Press ? for \
            help.")
  | Some (ManagedGroup (_, _)) | Some (AvailableGroup (_, _)) ->
      Miaou.Core.Help_hint.set
        (Some
           "Press Enter or Tab to expand/collapse binary group. Press ? for \
            help.")
  | None -> Miaou.Core.Help_hint.clear ()

(** Render the managed versions section with nested groups. *)
let render_managed_versions s =
  if s.managed_octez_versions = [] && s.managed_signatory_versions = [] then
    muted "No managed versions installed"
  else
    let lines =
      List.concat_map
        (fun item ->
          match item with
          | ManagedGroup (kind, is_expanded) ->
              let is_selected =
                match List.nth_opt s.items s.selected with
                | Some (ManagedGroup (k, _)) when k = kind -> true
                | _ -> false
              in
              let kind_name =
                match kind with Octez -> "Octez" | Signatory -> "Signatory"
              in
              let prefix = if is_selected then "\xe2\x9e\xa4 " else "  " in
              let expand_icon =
                if is_expanded then "\xe2\x96\xbc" else "\xe2\x96\xb6"
              in
              let fmt = if is_selected then bold else text in
              [fmt "%s%s %s" prefix expand_icon kind_name]
          | ManagedVersion (kind, version, size, count) ->
              let is_selected =
                match List.nth_opt s.items s.selected with
                | Some (ManagedVersion (k, v, _, _))
                  when k = kind && v = version ->
                    true
                | _ -> false
              in
              let prefix =
                if is_selected then "    \xe2\x9e\xa4 " else "      "
              in
              let size_str =
                match size with
                | Some s -> String_utils.format_size s
                | None -> "unknown"
              in
              let usage =
                if count = 0 then "unused"
                else if count = 1 then "1 instance"
                else raw "%d instances" count
              in
              let expansion_indicator =
                if count > 0 then
                  if
                    List.mem version s.expanded_managed_octez_items
                    && kind = Octez
                  then " \xe2\x96\xbc"
                  else " \xe2\x96\xb6"
                else ""
              in
              let fmt =
                if is_selected then bold else if count = 0 then muted else text
              in
              let main_line =
                fmt
                  "%sv%-15s  %10s  %s%s"
                  prefix
                  version
                  size_str
                  usage
                  expansion_indicator
              in
              if List.mem version s.expanded_managed_octez_items && kind = Octez
              then
                let bin_source =
                  match kind with
                  | Octez -> Binary_registry.Managed_octez_version version
                  | Signatory ->
                      Binary_registry.Managed_signatory_version version
                in
                let instances =
                  Service_registry.get_instances_using bin_source
                in
                let instance_lines =
                  List.map
                    (fun inst -> muted "        \xe2\x86\x92 %s" inst)
                    instances
                in
                main_line :: instance_lines
              else [main_line]
          | _ -> [])
        s.items
    in
    String.concat "\n" lines

(** Render the registered directories section. *)
let render_registered_dirs s =
  let header_lines =
    [
      muted "Register Octez binaries from development builds or custom locations";
      "";
    ]
  in
  let dir_lines =
    if s.registered_dirs = [] then [muted "No registered directories"]
    else
      List.concat_map
        (fun (ld, count) ->
          let is_selected =
            match List.nth_opt s.items s.selected with
            | Some (RegisteredDir (ld2, _))
              when ld.Binary_registry.alias = ld2.Binary_registry.alias ->
                true
            | _ -> false
          in
          let prefix = if is_selected then "\xe2\x9e\xa4 " else "  " in
          let usage =
            if count = 0 then "unused"
            else if count = 1 then "1 instance"
            else raw "%d instances" count
          in
          let expansion_indicator =
            if count > 0 then
              if List.mem ld.Binary_registry.alias s.expanded_registered then
                " \xe2\x96\xbc"
              else " \xe2\x96\xb6"
            else ""
          in
          let fmt =
            if is_selected then bold else if count = 0 then muted else text
          in
          let main_line =
            fmt
              "%s%-20s  %s  %s%s"
              prefix
              ld.alias
              (muted "%s" ld.path)
              usage
              expansion_indicator
          in
          if List.mem ld.Binary_registry.alias s.expanded_registered then
            let instances =
              Service_registry.get_instances_using
                (Binary_registry.Registered_alias ld.Binary_registry.alias)
            in
            let instance_lines =
              List.map
                (fun inst -> muted "      \xe2\x86\x92 %s" inst)
                instances
            in
            main_line :: instance_lines
          else [main_line])
        s.registered_dirs
  in
  (* Register directory button *)
  let link_action_selected =
    match List.nth_opt s.items s.selected with
    | Some RegisterAction -> true
    | _ -> false
  in
  let button_line =
    let prefix = if link_action_selected then "\xe2\x9e\xa4 " else "  " in
    if link_action_selected then bold "%s[+ Register a directory...]" prefix
    else accent "%s[+ Register a directory...]" prefix
  in
  String.concat "\n" (header_lines @ dir_lines @ [button_line])

(** Render the available-for-download versions section with nested groups. *)
let render_available_versions s =
  if s.available_octez_versions = [] && s.available_signatory_versions = [] then
    muted "No versions available (or all installed)"
  else
    let lines =
      List.concat_map
        (fun item ->
          match item with
          | AvailableGroup (kind, is_expanded) ->
              let is_selected =
                match List.nth_opt s.items s.selected with
                | Some (AvailableGroup (k, _)) when k = kind -> true
                | _ -> false
              in
              let kind_name =
                match kind with Octez -> "Octez" | Signatory -> "Signatory"
              in
              let prefix = if is_selected then "\xe2\x9e\xa4 " else "  " in
              let expand_icon =
                if is_expanded then "\xe2\x96\xbc" else "\xe2\x96\xb6"
              in
              let fmt = if is_selected then bold else text in
              [fmt "%s%s %s" prefix expand_icon kind_name]
          | AvailableMajorGroup (major, versions) ->
              let is_group_selected =
                match List.nth_opt s.items s.selected with
                | Some (AvailableMajorGroup (m, _)) when m = major -> true
                | _ -> false
              in
              let is_expanded = List.mem major s.expanded_octez_majors in
              let expand_icon = if is_expanded then "\xe2\x88\x92" else "+" in
              let prefix =
                if is_group_selected then "    \xe2\x9e\xa4 " else "      "
              in
              let version_count = List.length versions in
              let fmt = if is_group_selected then bold else text in
              let main_line =
                fmt
                  "%s%s v%d  (%d version%s)"
                  prefix
                  expand_icon
                  major
                  version_count
                  (if version_count = 1 then "" else "s")
              in
              if is_expanded then
                let version_lines =
                  List.map
                    (fun (vi : Binary_downloader.version_info) ->
                      let is_version_selected =
                        match List.nth_opt s.items s.selected with
                        | Some (AvailableVersion (Octez, vi2))
                          when vi.Binary_downloader.version
                               = vi2.Binary_downloader.version ->
                            true
                        | _ -> false
                      in
                      let v_prefix =
                        if is_version_selected then "      \xe2\x9e\xa4 "
                        else "        "
                      in
                      let date_str =
                        match vi.Binary_downloader.release_date with
                        | Some d -> " - " ^ d
                        | None -> ""
                      in
                      let fmt = if is_version_selected then bold else text in
                      fmt
                        "  %sv%s%s"
                        v_prefix
                        vi.Binary_downloader.version
                        date_str)
                    versions
                in
                main_line :: version_lines
              else [main_line]
          | AvailableSignatoryVersion vi ->
              let is_selected =
                match List.nth_opt s.items s.selected with
                | Some (AvailableSignatoryVersion vi2)
                  when vi.Signatory_downloader.version
                       = vi2.Signatory_downloader.version ->
                    true
                | _ -> false
              in
              let prefix =
                if is_selected then "    \xe2\x9e\xa4 " else "      "
              in
              let date_str =
                match vi.Signatory_downloader.release_date with
                | Some d -> " - " ^ d
                | None -> ""
              in
              let fmt = if is_selected then bold else text in
              [fmt "  %sv%s%s" prefix vi.Signatory_downloader.version date_str]
          | _ -> [])
        s.items
    in
    String.concat "\n" lines

(** Render download progress indicators. *)
let render_progress () =
  let multi_progress_lines = Context.render_multi_progress ~cols:80 in
  if String.trim multi_progress_lines <> "" then multi_progress_lines
  else
    let progress_line = Context.render_progress ~cols:80 in
    if String.trim progress_line <> "" then progress_line else ""

(** Footer key hints for the binaries page *)
let footer_hints =
  [
    ("Esc", "Back");
    ("r", "Refresh");
    ("d", "Download latest");
    ("l", "Register directory");
    ("p", "Prune unused");
    ("Enter", "Action");
    ("Tab", "Expand/Collapse");
    ("\xe2\x86\x91/\xe2\x86\x93", "Navigate");
    ("?", "Help");
  ]

(** Main view function for the binaries page. Assembles all sections. *)
let view ps ~focus:_ ~size =
  let s = ps.Navigation.s in
  let cols = size.LTerm_geom.cols in
  let box_width = min 78 (cols - 2) in
  set_help_hint s ;
  let render_body _ =
    let sections =
      [
        Style_context.with_child_context
          ~widget_name:"binaries-section-managed"
          (fun () ->
            Box.render
              ~title:"Managed Versions"
              ~style:Rounded
              ~width:box_width
              (render_managed_versions s));
        Style_context.with_child_context
          ~widget_name:"binaries-section-registered"
          (fun () ->
            Box.render
              ~title:"Registered Directories"
              ~style:Rounded
              ~width:box_width
              (render_registered_dirs s));
        Style_context.with_child_context
          ~widget_name:"binaries-section-available"
          (fun () ->
            Box.render
              ~title:"Available for Download"
              ~style:Rounded
              ~width:box_width
              (render_available_versions s));
      ]
    in
    let progress = render_progress () in
    let sections = if progress = "" then sections else sections @ [progress] in
    String.concat "\n" sections
  in
  let footer = Themed_page.render_themed_footer ~cols footer_hints in
  Themed_page.render_layout ~size ~header:[] ~footer ~child:render_body
