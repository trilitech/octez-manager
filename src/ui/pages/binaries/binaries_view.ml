(******************************************************************************)
(*                                                                            *)
(* SPDX-License-Identifier: MIT                                               *)
(* Copyright (c) 2026 Nomadic Labs <contact@nomadic-labs.com>                 *)
(*                                                                            *)
(******************************************************************************)

(** Rendering functions for the binaries management page.

    Contains the view function and section renderers for managed versions,
    registered directories, available versions, and download progress. *)

module Widgets = Miaou_widgets_display.Widgets
module Box = Miaou_widgets_layout.Box_widget
module Navigation = Miaou.Core.Navigation
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
  | Some (ManagedVersion (_, _, count)) ->
      if count > 0 then
        Miaou.Core.Help_hint.set
          (Some
             "Press Enter to expand/collapse instances using this version. \
              Press ? for help.")
      else
        Miaou.Core.Help_hint.set
          (Some "Press Enter to remove this version. Press ? for help.")
  | Some (AvailableVersion _) ->
      Miaou.Core.Help_hint.set
        (Some "Press Enter to download this version. Press ? for help.")
  | Some (AvailableMajorGroup _) ->
      Miaou.Core.Help_hint.set
        (Some
           "Press Enter or Tab to expand/collapse version group. Press ? for \
            help.")
  | None -> Miaou.Core.Help_hint.clear ()

(** Render the managed versions section. *)
let render_managed_versions s =
  if s.managed_versions = [] then Widgets.dim "No managed versions installed"
  else
    let lines =
      List.concat_map
        (fun (version, size, count) ->
          let is_selected =
            match List.nth_opt s.items s.selected with
            | Some (ManagedVersion (v, _, _)) when v = version -> true
            | _ -> false
          in
          let prefix = if is_selected then "\xe2\x9e\xa4 " else "  " in
          let size_str =
            match size with
            | Some s -> String_utils.format_size s
            | None -> "unknown"
          in
          let usage =
            if count = 0 then Widgets.dim "unused"
            else if count = 1 then "1 instance"
            else Printf.sprintf "%d instances" count
          in
          let expansion_indicator =
            if count > 0 then
              if List.mem version s.expanded_managed then " \xe2\x96\xbc"
              else " \xe2\x96\xb6"
            else ""
          in
          let line =
            Printf.sprintf
              "%sv%-15s  %10s  %s%s"
              prefix
              version
              size_str
              usage
              expansion_indicator
          in
          let main_line = if is_selected then Widgets.bold line else line in
          if List.mem version s.expanded_managed then
            let instances =
              Service_registry.get_instances_using
                (Binary_registry.Managed_version version)
            in
            let instance_lines =
              List.map
                (fun inst ->
                  Widgets.dim (Printf.sprintf "      \xe2\x86\x92 %s" inst))
                instances
            in
            main_line :: instance_lines
          else [main_line])
        s.managed_versions
    in
    String.concat "\n" lines

(** Render the registered directories section. *)
let render_registered_dirs s =
  let header_lines =
    [
      Widgets.dim
        "Register Octez binaries from development builds or custom locations";
      "";
    ]
  in
  let dir_lines =
    if s.registered_dirs = [] then [Widgets.dim "No registered directories"]
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
            if count = 0 then Widgets.dim "unused"
            else if count = 1 then "1 instance"
            else Printf.sprintf "%d instances" count
          in
          let expansion_indicator =
            if count > 0 then
              if List.mem ld.Binary_registry.alias s.expanded_registered then
                " \xe2\x96\xbc"
              else " \xe2\x96\xb6"
            else ""
          in
          let line =
            Printf.sprintf
              "%s%-20s  %s  %s%s"
              prefix
              ld.alias
              (Widgets.dim ld.path)
              usage
              expansion_indicator
          in
          let main_line = if is_selected then Widgets.bold line else line in
          if List.mem ld.Binary_registry.alias s.expanded_registered then
            let instances =
              Service_registry.get_instances_using
                (Binary_registry.Registered_alias ld.Binary_registry.alias)
            in
            let instance_lines =
              List.map
                (fun inst ->
                  Widgets.dim (Printf.sprintf "      \xe2\x86\x92 %s" inst))
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
  let link_button =
    let prefix = if link_action_selected then "\xe2\x9e\xa4 " else "  " in
    Printf.sprintf "%s%s" prefix (Widgets.fg 10 "[+ Register a directory...]")
  in
  let button_line =
    if link_action_selected then Widgets.bold link_button else link_button
  in
  String.concat "\n" (header_lines @ dir_lines @ [button_line])

(** Render the available-for-download versions section. *)
let render_available_versions s =
  if s.available_versions = [] then
    Widgets.dim "No versions available (or all installed)"
  else
    let lines =
      List.concat_map
        (fun item ->
          match item with
          | AvailableMajorGroup (major, versions) ->
              let is_group_selected =
                match List.nth_opt s.items s.selected with
                | Some (AvailableMajorGroup (m, _)) when m = major -> true
                | _ -> false
              in
              let is_expanded = List.mem major s.expanded_majors in
              let expand_icon = if is_expanded then "\xe2\x88\x92" else "+" in
              let prefix =
                if is_group_selected then "\xe2\x9e\xa4 " else "  "
              in
              let version_count = List.length versions in
              let group_line =
                Printf.sprintf
                  "%s%s v%d  (%d version%s)"
                  prefix
                  expand_icon
                  major
                  version_count
                  (if version_count = 1 then "" else "s")
              in
              let main_line =
                if is_group_selected then Widgets.bold group_line
                else group_line
              in
              if is_expanded then
                let version_lines =
                  List.map
                    (fun (vi : Binary_downloader.version_info) ->
                      let is_version_selected =
                        match List.nth_opt s.items s.selected with
                        | Some (AvailableVersion vi2)
                          when vi.Binary_downloader.version
                               = vi2.Binary_downloader.version ->
                            true
                        | _ -> false
                      in
                      let v_prefix =
                        if is_version_selected then "  \xe2\x9e\xa4 "
                        else "    "
                      in
                      let date_str =
                        match vi.Binary_downloader.release_date with
                        | Some d -> Printf.sprintf " - %s" d
                        | None -> ""
                      in
                      let line =
                        Printf.sprintf
                          "  %sv%s%s"
                          v_prefix
                          vi.Binary_downloader.version
                          date_str
                      in
                      if is_version_selected then Widgets.bold line else line)
                    versions
                in
                main_line :: version_lines
              else [main_line]
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

(** Main view function for the binaries page. Assembles all sections. *)
let view ps ~focus:_ ~size =
  let s = ps.Navigation.s in
  let box_width = min 78 (size.LTerm_geom.cols - 2) in
  set_help_hint s ;
  let sections =
    [
      Box.render
        ~title:"Managed Versions"
        ~style:Rounded
        ~color:14
        ~width:box_width
        (render_managed_versions s);
      Box.render
        ~title:"Registered Directories"
        ~style:Rounded
        ~color:13
        ~width:box_width
        (render_registered_dirs s);
      Box.render
        ~title:"Available for Download"
        ~style:Rounded
        ~color:10
        ~width:box_width
        (render_available_versions s);
    ]
  in
  let progress = render_progress () in
  let sections = if progress = "" then sections else sections @ [progress] in
  String.concat "\n" sections
