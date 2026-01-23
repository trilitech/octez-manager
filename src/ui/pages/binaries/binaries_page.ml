(******************************************************************************)
(*                                                                            *)
(* SPDX-License-Identifier: MIT                                               *)
(* Copyright (c) 2026 Nomadic Labs <contact@nomadic-labs.com>                 *)
(*                                                                            *)
(******************************************************************************)

(** Binaries management page *)

module Widgets = Miaou_widgets_display.Widgets
module Keys = Miaou.Core.Keys
module Navigation = Miaou.Core.Navigation
open Octez_manager_lib

let name = "binaries"

type item_type =
  | ManagedVersion of
      string * int64 option * int (* version, size, instance_count *)
  | LinkedDir of
      Binary_registry.linked_dir * int (* linked_dir, instance_count *)
  | AvailableVersion of Binary_downloader.version_info
  | AvailableMajorGroup of int * Binary_downloader.version_info list
(* major version, list of minor versions *)

type state = {
  managed_versions : (string * int64 option * int) list;
  linked_dirs : (Binary_registry.linked_dir * int) list;
  available_versions : Binary_downloader.version_info list;
  items : item_type list;
  selected : int;
  loading_remote : bool;
  expanded_majors : int list; (* list of expanded major versions *)
}

type msg = unit

type pstate = state Navigation.t

(** Helper functions *)

let get_dir_size path =
  try
    match Common.run_out ["du"; "-sb"; path] with
    | Ok output -> (
        match String.split_on_char '\t' output with
        | size_str :: _ -> (
            try Some (Int64.of_string (String.trim size_str)) with _ -> None)
        | _ -> None)
    | Error _ -> None
  with _ -> None

let count_instances_using bin_source =
  match Service_registry.list () with
  | Error _ -> 0
  | Ok services ->
      List.filter
        (fun svc ->
          match Service.get_bin_source svc with
          | bs when bs = bin_source -> true
          | _ -> false)
        services
      |> List.length

let load_managed_versions () =
  match Binary_registry.list_managed_versions () with
  | Error _ -> []
  | Ok versions ->
      List.map
        (fun version ->
          let path = Binary_registry.managed_version_path version in
          let size = get_dir_size path in
          let count =
            count_instances_using (Binary_registry.Managed_version version)
          in
          (version, size, count))
        versions

let load_linked_dirs () =
  match Binary_registry.load_linked_dirs () with
  | Error _ -> []
  | Ok dirs ->
      List.map
        (fun (ld : Binary_registry.linked_dir) ->
          let count =
            count_instances_using
              (Binary_registry.Linked_alias ld.Binary_registry.alias)
          in
          (ld, count))
        dirs

(** Filter versions to only keep the N latest major versions.
    Returns versions from the N most recent major version families. *)
let filter_latest_n_major_versions n versions =
  let extract_major version_str =
    try
      match String.split_on_char '.' version_str with
      | major :: _ -> int_of_string major
      | [] -> 0
    with _ -> 0
  in
  (* Group versions by major version *)
  let major_versions = Hashtbl.create 5 in
  List.iter
    (fun (v : Binary_downloader.version_info) ->
      let major = extract_major v.version in
      let existing = Hashtbl.find_opt major_versions major in
      Hashtbl.replace
        major_versions
        major
        (v :: Option.value ~default:[] existing))
    versions ;
  (* Get the N latest major versions *)
  let all_majors =
    Hashtbl.to_seq_keys major_versions |> List.of_seq |> List.sort compare
  in
  let latest_n_majors =
    List.rev all_majors |> fun l -> List.filteri (fun i _ -> i < n) l
  in
  List.concat_map
    (fun major ->
      Option.value ~default:[] (Hashtbl.find_opt major_versions major))
    latest_n_majors

let load_available_versions () =
  match Versions_scheduler.get_cached () with
  | None -> []
  | Some versions ->
      (* Filter to only the 2 latest major versions *)
      let filtered_versions = filter_latest_n_major_versions 2 versions in
      (* Filter out already installed versions *)
      let managed =
        match Binary_registry.list_managed_versions () with
        | Ok v -> v
        | Error _ -> []
      in
      List.filter
        (fun (v : Binary_downloader.version_info) ->
          not (List.mem v.version managed))
        filtered_versions

let build_items managed linked available expanded_majors =
  let items = ref [] in
  List.iter
    (fun (v, s, c) -> items := ManagedVersion (v, s, c) :: !items)
    managed ;
  List.iter (fun (ld, c) -> items := LinkedDir (ld, c) :: !items) linked ;

  (* Group available versions by major version *)
  let major_groups = Hashtbl.create 10 in
  List.iter
    (fun (v : Binary_downloader.version_info) ->
      (* Parse major version from "24.0" or "23.1-rc1" *)
      match String.split_on_char '.' v.version with
      | major_str :: _ -> (
          try
            let major = int_of_string major_str in
            let existing =
              Hashtbl.find_opt major_groups major |> Option.value ~default:[]
            in
            Hashtbl.replace major_groups major (v :: existing)
          with _ -> ())
      | _ -> ())
    available ;

  (* Add major groups in descending order, with sub-items if expanded *)
  let majors =
    Hashtbl.to_seq_keys major_groups
    |> List.of_seq
    |> List.sort (fun a b -> compare b a)
  in
  List.iter
    (fun major ->
      let versions = Hashtbl.find major_groups major |> List.rev in
      items := AvailableMajorGroup (major, versions) :: !items ;
      (* If expanded, add individual version items *)
      if List.mem major expanded_majors then
        List.iter (fun v -> items := AvailableVersion v :: !items) versions)
    majors ;

  List.rev !items

let init () =
  let managed = load_managed_versions () in
  let linked = load_linked_dirs () in
  let available = load_available_versions () in
  let expanded_majors = [] in
  let items = build_items managed linked available expanded_majors in
  Navigation.make
    {
      managed_versions = managed;
      linked_dirs = linked;
      available_versions = available;
      items;
      selected = 0;
      loading_remote = false;
      expanded_majors;
    }

let update ps _ = ps

let refresh_data s =
  let managed = load_managed_versions () in
  let linked = load_linked_dirs () in
  let available = load_available_versions () in
  let items = build_items managed linked available s.expanded_majors in
  let selected = min s.selected (max 0 (List.length items - 1)) in
  {
    managed_versions = managed;
    linked_dirs = linked;
    available_versions = available;
    items;
    selected;
    loading_remote = false;
    expanded_majors = s.expanded_majors;
  }

let refresh ps = Navigation.update refresh_data ps

let auto_refresh ps =
  (* Auto-refresh if data has been marked dirty (e.g., after remove/download) *)
  if Context.consume_instances_dirty () then refresh ps else ps

let toggle_major_expansion s major =
  let expanded_majors =
    if List.mem major s.expanded_majors then
      List.filter (( <> ) major) s.expanded_majors
    else major :: s.expanded_majors
  in
  (* Rebuild items with new expansion state *)
  let items =
    build_items
      s.managed_versions
      s.linked_dirs
      s.available_versions
      expanded_majors
  in
  {s with expanded_majors; items}

let move_up s =
  let selected = if s.selected > 0 then s.selected - 1 else s.selected in
  {s with selected}

let move_down s =
  let max_idx = List.length s.items - 1 in
  let selected = if s.selected < max_idx then s.selected + 1 else s.selected in
  {s with selected}

let move_selection ps dir =
  match dir with
  | `Up -> Navigation.update move_up ps
  | `Down -> Navigation.update move_down ps
  | _ -> ps

let move ps _ = ps

let service_select ps _ = ps

let service_cycle ps _ = refresh ps

let back ps = Navigation.back ps

(** Action handlers *)

let remove_version version =
  let bin_source = Binary_registry.Managed_version version in
  let count = count_instances_using bin_source in
  if count > 0 then
    Modal_helpers.show_error
      ~title:"Version In Use"
      (Printf.sprintf
         "Version v%s is used by %d instance(s). Cannot remove."
         version
         count)
  else
    Modal_helpers.confirm_modal
      ~title:(Printf.sprintf "Remove managed version v%s?" version)
      ~message:""
      ~on_result:(fun confirmed ->
        if confirmed then
          (* Run removal in background to avoid blocking UI *)
          Background_runner.enqueue (fun () ->
              match Binary_downloader.remove_version version with
              | Ok () ->
                  Context.toast_success (Printf.sprintf "Removed v%s" version) ;
                  Context.mark_instances_dirty ()
              | Error (`Msg msg) ->
                  Context.toast_error (Printf.sprintf "Remove failed: %s" msg)))
      ()

let unlink_directory ld =
  let count =
    count_instances_using
      (Binary_registry.Linked_alias ld.Binary_registry.alias)
  in
  if count > 0 then
    Modal_helpers.show_error
      ~title:"Directory In Use"
      (Printf.sprintf
         "Linked directory '%s' is used by %d instance(s). Cannot unlink."
         ld.alias
         count)
  else
    Modal_helpers.confirm_modal
      ~title:(Printf.sprintf "Unlink directory '%s'?" ld.alias)
      ~message:""
      ~on_result:(fun confirmed ->
        if confirmed then
          (* Run unlink in background to avoid blocking UI *)
          Background_runner.enqueue (fun () ->
              match Binary_registry.remove_linked_dir ld.alias with
              | Ok () ->
                  Context.toast_success
                    (Printf.sprintf "Unlinked '%s'" ld.alias) ;
                  Context.mark_instances_dirty ()
              | Error (`Msg msg) ->
                  Context.toast_error (Printf.sprintf "Unlink failed: %s" msg)))
      ()

let download_version (version_info : Binary_downloader.version_info) =
  (* Start download in background *)
  Background_runner.enqueue (fun () ->
      let version = version_info.Binary_downloader.version in
      Context.toast_info (Printf.sprintf "Downloading v%s..." version) ;

      (* Initialize multi-progress with list of binaries *)
      Context.multi_progress_start
        ~version
        ~binaries:
          ["octez-node"; "octez-client"; "octez-baker"; "octez-dal-node"] ;

      (* Multi-progress callback *)
      let multi_progress (mp : Binary_downloader.multi_progress_state) =
        Context.multi_progress_update
          ~binary:mp.current_file
          ~downloaded:mp.downloaded
          ~total:mp.total
      in

      let result =
        Binary_downloader.download_version
          ~version
          ~verify_checksums:true
          ~multi_progress
          ()
      in

      (* Handle checksums *)
      match result with
      | Ok res ->
          Context.multi_progress_checksum "Verifying checksums..." ;
          Unix.sleepf 0.5 ;
          (match res.Binary_downloader.checksum_status with
          | Binary_downloader.Verified ->
              Context.multi_progress_checksum
                "\xe2\x9c\x93 All checksums verified"
          | Binary_downloader.Skipped ->
              Context.multi_progress_checksum
                "\xe2\x9a\xa0 Checksum verification skipped"
          | Binary_downloader.Failed reason ->
              Context.multi_progress_checksum
                (Printf.sprintf "\xe2\x9c\x97 Failed: %s" reason)) ;
          Unix.sleepf 2.0 ;
          (* Linger to show final status *)
          Context.multi_progress_finish () ;
          Context.toast_success (Printf.sprintf "Downloaded v%s" version) ;
          Context.mark_instances_dirty ()
      | Error (`Msg msg) ->
          Context.multi_progress_finish () ;
          Context.toast_error (Printf.sprintf "Download failed: %s" msg))

let link_directory () =
  Modal_helpers.open_file_browser_modal
    ~dirs_only:true
    ~require_writable:false
    ~on_select:(fun path ->
      let alias = Filename.basename path in
      match Binary_registry.add_linked_dir ~alias ~path with
      | Ok () ->
          Context.toast_success (Printf.sprintf "Linked '%s'" alias) ;
          Context.mark_instances_dirty ()
      | Error (`Msg msg) -> Modal_helpers.show_error ~title:"Link Failed" msg)
    ()

let prune_unused s =
  let unused =
    List.filter (fun (_v, _s, count) -> count = 0) s.managed_versions
  in
  if unused = [] then (
    Modal_helpers.show_error ~title:"Prune" "No unused versions to prune." ;
    s)
  else
    (* Calculate total size and build detailed message *)
    let version_details = ref [] in
    let total_bytes = ref 0L in
    List.iter
      (fun (v, _, _) ->
        match Binary_downloader.get_version_size v with
        | Ok (bytes, formatted) ->
            version_details := (v, formatted) :: !version_details ;
            total_bytes := Int64.add !total_bytes bytes
        | Error _ -> version_details := (v, "unknown size") :: !version_details)
      unused ;
    let details_lines =
      List.map
        (fun (v, size) -> Printf.sprintf "  • v%s (%s)" v size)
        (List.rev !version_details)
    in
    let total_formatted = Binary_downloader.format_size_bytes !total_bytes in
    let message =
      String.concat
        "\n"
        (["The following versions will be removed:"; ""]
        @ details_lines
        @ [""; Printf.sprintf "Total space to free: %s" total_formatted])
    in
    Modal_helpers.confirm_modal
      ~title:(Printf.sprintf "Prune %d unused version(s)?" (List.length unused))
      ~message
      ~on_result:(fun confirmed ->
        if confirmed then (
          let success_count = ref 0 in
          let fail_count = ref 0 in
          List.iter
            (fun (v, _, _) ->
              match Binary_downloader.remove_version v with
              | Ok () -> incr success_count
              | Error _ -> incr fail_count)
            unused ;
          if !fail_count = 0 then
            Context.toast_info
              (Printf.sprintf
                 "Removed %d version(s), freed %s"
                 !success_count
                 total_formatted)
          else
            Context.toast_error
              (Printf.sprintf
                 "Removed %d version(s), %d failed"
                 !success_count
                 !fail_count) ;
          Context.mark_instances_dirty ()))
      () ;
    s

let handle_action s =
  if s.items = [] then s
  else
    let item = List.nth s.items s.selected in
    match item with
    | ManagedVersion (version, _, _) ->
        remove_version version ;
        s
    | LinkedDir (ld, _) ->
        unlink_directory ld ;
        s
    | AvailableVersion vi ->
        download_version vi ;
        s
    | AvailableMajorGroup (major, _) ->
        (* Toggle expansion on Enter *)
        toggle_major_expansion s major

let toggle_current_group s =
  if s.items = [] then s
  else
    let item = List.nth s.items s.selected in
    match item with
    | AvailableMajorGroup (major, _) -> toggle_major_expansion s major
    | _ -> s

(** View *)

let format_size bytes =
  let kb = Int64.div bytes 1024L in
  let mb = Int64.div kb 1024L in
  let gb = Int64.div mb 1024L in
  if gb > 0L then Printf.sprintf "%Ld GB" gb
  else if mb > 0L then Printf.sprintf "%Ld MB" mb
  else if kb > 0L then Printf.sprintf "%Ld KB" kb
  else Printf.sprintf "%Ld B" bytes

let view ps ~focus:_ ~size:_ =
  let s = ps.Navigation.s in
  let lines = ref [] in
  let add line = lines := line :: !lines in

  (* Set help hint based on selected item *)
  (match List.nth_opt s.items s.selected with
  | Some LinkAction ->
      Miaou.Core.Help_hint.set
        (Some
           "Linked directories let you use Octez binaries from other locations \
            (dev builds, system installs, custom versions). Press Enter to \
            browse for a directory.")
  | Some (LinkedDir _) ->
      Miaou.Core.Help_hint.set
        (Some "Press Enter to unlink this directory. Press ? for help.")
  | Some (ManagedVersion _) ->
      Miaou.Core.Help_hint.set
        (Some "Press Enter to remove this version. Press ? for help.")
  | Some (AvailableVersion _) ->
      Miaou.Core.Help_hint.set
        (Some "Press Enter to download this version. Press ? for help.")
  | None -> Miaou.Core.Help_hint.clear ()) ;

  (* Header *)
  add (Widgets.fg 14 (Widgets.bold "━━━ Managed Versions ━━━")) ;
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
        let prefix = if is_selected then "➤ " else "  " in
        let size_str =
          match size with Some s -> format_size s | None -> "unknown"
        in
        let usage =
          if count = 0 then Widgets.dim "unused"
          else if count = 1 then "1 instance"
          else Printf.sprintf "%d instances" count
        in
        let line =
          Printf.sprintf "%sv%-15s  %10s  %s" prefix version size_str usage
        in
        add (if is_selected then Widgets.bold line else line))
      s.managed_versions ;

  add "" ;
  add (Widgets.fg 13 (Widgets.bold "━━━ Linked Directories ━━━")) ;
  add "" ;

  if s.linked_dirs = [] then add (Widgets.dim "  No linked directories")
  else
    List.iter
      (fun (ld, count) ->
        let is_selected =
          match List.nth_opt s.items s.selected with
          | Some (LinkedDir (ld2, _))
            when ld.Binary_registry.alias = ld2.Binary_registry.alias ->
              true
          | _ -> false
        in
        let prefix = if is_selected then "➤ " else "  " in
        let usage =
          if count = 0 then Widgets.dim "unused"
          else if count = 1 then "1 instance"
          else Printf.sprintf "%d instances" count
        in
        let line =
          Printf.sprintf
            "%s%-20s  %s  %s"
            prefix
            ld.alias
            (Widgets.dim ld.path)
            usage
        in
        add (if is_selected then Widgets.bold line else line))
      s.linked_dirs ;

  add "" ;
  add (Widgets.fg 10 (Widgets.bold "━━━ Available for Download ━━━")) ;
  add "" ;

  if s.available_versions = [] then
    add (Widgets.dim "  No versions available (or all installed)")
  else
    (* Render major version groups *)
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
            let expand_icon = if is_expanded then "−" else "+" in
            let prefix = if is_group_selected then "➤ " else "  " in
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
            (* If expanded, show all minor versions *)
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
                    if is_version_selected then "  ➤ " else "    "
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
      s.items ;

  (* Add multi-progress display if active, fallback to single progress *)
  let multi_progress_lines = Context.render_multi_progress ~cols:80 in
  if String.trim multi_progress_lines <> "" then (
    add "" ;
    add multi_progress_lines)
  else (
    add "" ;
    let progress_line = Context.render_progress ~cols:80 in
    if String.trim progress_line <> "" then add progress_line) ;

  String.concat "\n" (List.rev !lines)

let handle_modal_key ps key ~size:_ =
  Miaou.Core.Modal_manager.handle_key key ;
  ps

let handle_key ps key ~size:_ =
  if Miaou.Core.Modal_manager.has_active () then (
    Miaou.Core.Modal_manager.handle_key key ;
    ps)
  else
    match Keys.of_string key with
    | Some (Keys.Char "Esc") -> back ps
    | Some (Keys.Char "r") -> refresh ps
    | Some (Keys.Char "d") ->
        Navigation.update
          (fun s ->
            if s.available_versions <> [] then
              download_version (List.hd s.available_versions) ;
            s)
          ps
    | Some (Keys.Char "l") ->
        link_directory () ;
        ps
    | Some (Keys.Char "p") -> Navigation.update prune_unused ps
    | Some Keys.Enter -> Navigation.update handle_action ps
    | Some Keys.Tab -> Navigation.update toggle_current_group ps
    | Some Keys.Up -> move_selection ps `Up
    | Some Keys.Down -> move_selection ps `Down
    | _ -> ps

let has_modal _ = Miaou.Core.Modal_manager.has_active ()

let handled_keys () = Miaou.Core.Keys.[Escape]

let keymap _ =
  let noop ps = ps in
  let kb key action help =
    {Miaou.Core.Tui_page.key; action; help; display_only = false}
  in
  [
    kb "Esc" back "Back";
    kb "r" refresh "Refresh";
    kb
      "d"
      (fun ps ->
        Navigation.update
          (fun s ->
            download_version (List.hd s.available_versions) ;
            s)
          ps)
      "Download latest";
    kb
      "l"
      (fun ps ->
        link_directory () ;
        ps)
      "Link directory";
    kb "p" (fun ps -> Navigation.update prune_unused ps) "Prune unused";
    kb "Enter" (fun ps -> Navigation.update handle_action ps) "Action";
    kb
      "Tab"
      (fun ps -> Navigation.update toggle_current_group ps)
      "Expand/Collapse";
    kb "Up" (fun ps -> move_selection ps `Up) "Move up";
    kb "Down" (fun ps -> move_selection ps `Down) "Move down";
    {
      Miaou.Core.Tui_page.key = "?";
      action = noop;
      help = "Help";
      display_only = true;
    };
  ]

let header =
  [
    Widgets.title_highlight " Binaries Management ";
    Widgets.dim "Manage Octez binary versions and linked directories";
  ]

let footer = []

module Page_Impl :
  Miaou.Core.Tui_page.PAGE_SIG with type state = state and type msg = msg =
struct
  type nonrec state = state

  type nonrec msg = msg

  type key_binding = state Miaou.Core.Tui_page.key_binding_desc

  type nonrec pstate = pstate

  let init () = init ()

  let update = update

  let refresh = auto_refresh

  let move = move

  let service_select = service_select

  let service_cycle = service_cycle

  let back = back

  let handled_keys () = handled_keys ()

  let keymap = keymap

  let handle_key = handle_key

  let handle_modal_key = handle_modal_key

  let has_modal = has_modal

  let view = view
end

let page = (module Page_Impl : Miaou.Core.Tui_page.PAGE_SIG)

let register () =
  if not (Miaou.Core.Registry.exists name) then
    Miaou.Core.Registry.register name page

(** For testing *)
module For_tests = struct
  let filter_latest_n_major_versions = filter_latest_n_major_versions
end
