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
let render_managed_versions s add =
  add
    (Widgets.fg
       14
       (Widgets.bold
          "\xe2\x94\x81\xe2\x94\x81\xe2\x94\x81 Managed Versions \
           \xe2\x94\x81\xe2\x94\x81\xe2\x94\x81")) ;
  add "" ;
  if s.managed_versions = [] then
    add (Widgets.dim "  No managed versions installed")
  else
    List.iteri
      (fun _idx (version, size, count) ->
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
        add (if is_selected then Widgets.bold line else line) ;
        if List.mem version s.expanded_managed then
          let instances =
            Service_registry.get_instances_using
              (Binary_registry.Managed_version version)
          in
          List.iter
            (fun inst ->
              add (Widgets.dim (Printf.sprintf "      \xe2\x86\x92 %s" inst)))
            instances)
      s.managed_versions

(** Render the registered directories section. *)
let render_registered_dirs s add =
  add "" ;
  add
    (Widgets.fg
       13
       (Widgets.bold
          "\xe2\x94\x81\xe2\x94\x81\xe2\x94\x81 Registered Directories \
           \xe2\x94\x81\xe2\x94\x81\xe2\x94\x81")) ;
  add
    (Widgets.dim
       "Register Octez binaries from development builds or custom locations") ;
  add "" ;
  if s.registered_dirs = [] then add (Widgets.dim "  No registered directories")
  else
    List.iter
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
        add (if is_selected then Widgets.bold line else line) ;
        if List.mem ld.Binary_registry.alias s.expanded_registered then
          let instances =
            Service_registry.get_instances_using
              (Binary_registry.Registered_alias ld.Binary_registry.alias)
          in
          List.iter
            (fun inst ->
              add (Widgets.dim (Printf.sprintf "      \xe2\x86\x92 %s" inst)))
            instances)
      s.registered_dirs ;
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
  add (if link_action_selected then Widgets.bold link_button else link_button)

(** Render the available-for-download versions section. *)
let render_available_versions s add =
  add "" ;
  add
    (Widgets.fg
       10
       (Widgets.bold
          "\xe2\x94\x81\xe2\x94\x81\xe2\x94\x81 Available for Download \
           \xe2\x94\x81\xe2\x94\x81\xe2\x94\x81")) ;
  add "" ;
  if s.available_versions = [] then
    add (Widgets.dim "  No versions available (or all installed)")
  else
    List.iter
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
            let prefix = if is_group_selected then "\xe2\x9e\xa4 " else "  " in
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
            add
              (if is_group_selected then Widgets.bold group_line else group_line) ;
            if is_expanded then
              List.iter
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
                    if is_version_selected then "  \xe2\x9e\xa4 " else "    "
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
                  add (if is_version_selected then Widgets.bold line else line))
                versions
        | _ -> ())
      s.items

(** Render download progress indicators. *)
let render_progress add =
  let multi_progress_lines = Context.render_multi_progress ~cols:80 in
  if String.trim multi_progress_lines <> "" then (
    add "" ;
    add multi_progress_lines)
  else (
    add "" ;
    let progress_line = Context.render_progress ~cols:80 in
    if String.trim progress_line <> "" then add progress_line)

(** Main view function for the binaries page. Assembles all sections. *)
let view ps ~focus:_ ~size:_ =
  let s = ps.Navigation.s in
  let buf = Buffer.create 2048 in
  let add line =
    if Buffer.length buf > 0 then Buffer.add_char buf '\n' ;
    Buffer.add_string buf line
  in
  set_help_hint s ;
  render_managed_versions s add ;
  render_registered_dirs s add ;
  render_available_versions s add ;
  render_progress add ;
  Buffer.contents buf
