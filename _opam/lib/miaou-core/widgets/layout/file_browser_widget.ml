(*****************************************************************************)
(*                                                                           *)
(* SPDX-License-Identifier: MIT                                              *)
(* Copyright (c) 2025 Nomadic Labs <contact@nomadic-labs.com>                *)
(*                                                                           *)
(*****************************************************************************)
[@@@warning "-32-34-37-69"]

type entry = {name : string; is_dir : bool}

(* Cache for directory listings and writable status to avoid repeated filesystem calls *)
type cache = {
  mutable cached_path : string;
  mutable cached_entries : entry list;
  mutable cached_writable : (string, bool) Hashtbl.t;
  mutable cached_show_hidden : bool;
}

let make_cache () =
  {
    cached_path = "";
    cached_entries = [];
    cached_writable = Hashtbl.create 32;
    cached_show_hidden = false;
  }

(* Global cache - invalidated when path changes *)
let cache = make_cache ()

let invalidate_cache () =
  cache.cached_path <- "" ;
  cache.cached_entries <- [] ;
  Hashtbl.clear cache.cached_writable

type t = {
  current_path : string;
  cursor : int;
  cancelled : bool;
  dirs_only : bool;
  require_writable : bool;
  select_dirs : bool;
  show_hidden : bool; (* whether to show hidden files/dirs starting with '.' *)
  (* New: direct path editing *)
  mode : mode;
  path_buffer : string;
      (* kept for compatibility; source of truth is textbox when present *)
  path_error : string option;
  pending_selection : string option;
  create_dir_on_enter : bool;
  textbox : Miaou_widgets_input.Textbox_widget.t option;
  history : string list; (* most-recent-first *)
  history_idx : int option; (* index into history while editing *)
}

and mode = Browsing | EditingPath

module Textbox = Miaou_widgets_input.Textbox_widget
module Helpers = Miaou_helpers.Helpers

let textbox_create ?(width = 60) ?(initial = "") () =
  Textbox.create ~width ~initial ()

let textbox_get_text = Textbox.get_text

let textbox_set_text tb text = Textbox.set_text tb text

let textbox_handle_key tb ~key = Textbox.handle_key tb ~key

let textbox_render tb = Textbox.render tb ~focus:true

let key_hints w =
  let hidden_hint = if w.show_hidden then "hide hidden" else "show hidden" in
  [
    ("↑/↓", "navigate");
    ("PgUp/PgDn", "page");
    ("Space", "select");
    ("Enter", "open/confirm");
    ("Backspace", "parent");
    ("Tab", "edit path");
    ("h", hidden_hint);
    ("n", "new directory");
    ("Esc", "cancel");
  ]

(* Global state to handle async updates from modal callbacks *)
let pending_path_update : string option ref = ref None

(* Function to apply any pending path updates *)
let apply_pending_updates browser =
  match !pending_path_update with
  | None -> browser
  | Some path ->
      pending_path_update := None ;
      {browser with current_path = path; cursor = 0}

(* Function to schedule a path update for later application *)
let schedule_path_update path = pending_path_update := Some path

let rec normalize_start p =
  let sys = Miaou_interfaces.System.require () in
  try
    if sys.file_exists p && sys.is_directory p then
      (* Prefer an absolute, canonical path when available so that navigating
         up keeps working (e.g. avoid getting stuck at "./"). *)
      try Unix.realpath p with _ -> p
    else
      let parent = Filename.dirname p in
      if parent = p then "/" else normalize_start parent
  with _ -> "/"

let open_centered ?(path = "/") ?(dirs_only = true) ?(require_writable = true)
    ?(select_dirs = true) ?(show_hidden = false) () =
  let start = normalize_start path in
  {
    current_path = start;
    cursor = 0;
    cancelled = false;
    dirs_only;
    require_writable;
    select_dirs;
    show_hidden;
    mode = Browsing;
    path_buffer = "";
    path_error = None;
    pending_selection = None;
    create_dir_on_enter = false;
    textbox = None;
    history = [];
    history_idx = None;
  }

let clamp = List_nav.clamp

let is_writable path =
  match Hashtbl.find_opt cache.cached_writable path with
  | Some v -> v
  | None ->
      let sys = Miaou_interfaces.System.require () in
      let result =
        match sys.probe_writable ~path with Ok b -> b | Error _ -> false
      in
      Hashtbl.add cache.cached_writable path result ;
      result

let rec next_available_name ~existing ~prefix idx =
  let candidate =
    if idx = 0 then prefix else Printf.sprintf "%s_%d" prefix idx
  in
  if List.exists (fun (e : entry) -> e.name = candidate) existing then
    next_available_name ~existing ~prefix (idx + 1)
  else candidate

let is_hidden name =
  String.length name > 0 && name.[0] = '.' && name <> ".." && name <> "."

let list_entries path ~dirs_only ~show_hidden =
  let sys = Miaou_interfaces.System.require () in
  match sys.list_dir path with
  | Error e -> Error e
  | Ok items ->
      let items = List.filter (fun n -> n <> ".") items in
      let items = List.sort String.compare items in
      let mapped =
        List.fold_right
          (fun n acc ->
            (* Filter hidden files unless show_hidden is true *)
            if (not show_hidden) && is_hidden n then acc
            else
              let full = Filename.concat path n in
              let is_dir = try sys.is_directory full with _ -> false in
              if dirs_only && not is_dir then acc else {name = n; is_dir} :: acc)
          items
          []
      in
      let dirs, files = List.partition (fun e -> e.is_dir) mapped in
      Ok (dirs @ files)

(* Internal helper that returns empty list on error for backward compatibility *)
let list_entries_safe path ~dirs_only ~show_hidden =
  match list_entries path ~dirs_only ~show_hidden with
  | Ok entries -> entries
  | Error _ -> []

let list_entries_with_parent path ~dirs_only ~show_hidden =
  (* Use cached entries if path and show_hidden match *)
  if
    cache.cached_path = path
    && cache.cached_show_hidden = show_hidden
    && cache.cached_entries <> []
  then cache.cached_entries
  else begin
    (* Path or show_hidden changed - clear writable cache for fresh checks *)
    if cache.cached_path <> path then Hashtbl.clear cache.cached_writable ;
    let entries = list_entries_safe path ~dirs_only ~show_hidden in
    let parent = Filename.dirname path in
    let with_parent =
      if parent = path then entries else {name = ".."; is_dir = true} :: entries
    in
    (* Add a dot entry to allow explicitly selecting the current directory,
       directly below the parent entry when present. *)
    let result =
      match with_parent with
      | p :: rest when p.name = ".." -> p :: {name = "."; is_dir = true} :: rest
      | lst -> {name = "."; is_dir = true} :: lst
    in
    (* Update cache *)
    cache.cached_path <- path ;
    cache.cached_entries <- result ;
    cache.cached_show_hidden <- show_hidden ;
    result
  end

let is_cancelled w = w.cancelled

let reset_cancelled w = {w with cancelled = false}

let get_current_path w = w.current_path

let get_selected_entry w =
  let entries =
    list_entries_with_parent
      w.current_path
      ~dirs_only:w.dirs_only
      ~show_hidden:w.show_hidden
  in
  if entries = [] then None
  else
    let idx = clamp 0 (max 0 (List.length entries - 1)) w.cursor in
    List.nth_opt entries idx

let get_selection w =
  match w.pending_selection with
  | Some p -> Some p
  | None -> (
      match get_selected_entry w with
      | None -> Some w.current_path
      | Some e ->
          let target =
            if e.name = ".." then Filename.dirname w.current_path
            else if e.name = "." then w.current_path
            else Filename.concat w.current_path e.name
          in
          if e.is_dir && not w.select_dirs then None else Some target)

let is_editing w = match w.mode with EditingPath -> true | Browsing -> false

let can_commit w =
  match w.mode with
  | EditingPath -> (
      (* Allow commit only when we have a validated pending file selection *)
      match w.pending_selection with
      | Some _ -> true
      | None -> false)
  | Browsing -> true

let current_input w =
  match w.textbox with
  | Some tb -> textbox_get_text tb
  | None -> if w.path_buffer = "" then w.current_path else w.path_buffer

let handle_key w ~key =
  (* Apply any pending path updates first *)
  let w = apply_pending_updates w in
  let entries =
    list_entries_with_parent
      w.current_path
      ~dirs_only:w.dirs_only
      ~show_hidden:w.show_hidden
  in
  let total = List.length entries in
  match w.mode with
  | Browsing -> (
      match key with
      | "h" ->
          (* Toggle hidden files *)
          invalidate_cache () ;
          {w with show_hidden = not w.show_hidden; cursor = 0}
      | "Up" ->
          {
            w with
            cursor = List_nav.move_cursor ~total ~cursor:w.cursor ~delta:(-1);
          }
      | "Down" ->
          {
            w with
            cursor = List_nav.move_cursor ~total ~cursor:w.cursor ~delta:1;
          }
      | "PageUp" ->
          {
            w with
            cursor =
              List_nav.page_move ~total ~cursor:w.cursor ~page_size:8 ~dir:`Up;
          }
      | "PageDown" ->
          {
            w with
            cursor =
              List_nav.page_move ~total ~cursor:w.cursor ~page_size:8 ~dir:`Down;
          }
      | "Space" ->
          {
            w with
            cursor =
              List_nav.page_move ~total ~cursor:w.cursor ~page_size:8 ~dir:`Down;
          }
      | "n" ->
          if not (is_writable w.current_path) then
            {w with path_error = Some "Not writable"}
          else
            let entries =
              list_entries_safe
                w.current_path
                ~dirs_only:false
                ~show_hidden:true
            in
            let suggested =
              next_available_name ~existing:entries ~prefix:"new_directory" 0
            in
            let tb = textbox_create ~initial:suggested () in
            {
              w with
              mode = EditingPath;
              textbox = Some tb;
              create_dir_on_enter = true;
              path_error = None;
              pending_selection = None;
              history_idx = None;
            }
      | "Shift-Space" ->
          {
            w with
            cursor =
              List_nav.page_move ~total ~cursor:w.cursor ~page_size:8 ~dir:`Up;
          }
      | "Esc" -> {w with cancelled = true}
      | "Enter" -> (
          (* Navigate into selected directory *)
          match List.nth_opt entries w.cursor with
          | Some entry when entry.name = ".." ->
              let parent = Filename.dirname w.current_path in
              {w with current_path = parent; cursor = 0}
          | Some entry when entry.name = "." ->
              (* Treat dot as an explicit selection of the current directory. *)
              {w with pending_selection = Some w.current_path}
          | Some entry ->
              let target = Filename.concat w.current_path entry.name in
              let sys = Miaou_interfaces.System.require () in
              let is_dir =
                entry.is_dir || try sys.is_directory target with _ -> false
              in
              if is_dir then
                let new_path = normalize_start target in
                {w with current_path = new_path; cursor = 0; path_error = None}
              else w
          | _ -> w)
      | "Left" | "Backspace" ->
          let parent = Filename.dirname w.current_path in
          {w with current_path = parent; cursor = 0}
      | "Tab" | "C-l" ->
          (* Enter editing with textbox prefilled from current_path. *)
          let tb = textbox_create ~initial:w.current_path ~width:60 () in
          {
            w with
            mode = EditingPath;
            textbox = Some tb;
            path_error = None;
            pending_selection = None;
            history_idx = None;
          }
      | k when String.length k = 1 && (k.[0] = '/' || k.[0] = '~') ->
          let tb = textbox_create ~initial:(w.current_path ^ k) ~width:60 () in
          {
            w with
            mode = EditingPath;
            textbox = Some tb;
            path_error = None;
            pending_selection = None;
            history_idx = None;
          }
      | _ -> w)
  | EditingPath -> (
      match (key, w.textbox) with
      | "Esc", _ ->
          {
            w with
            mode = Browsing;
            path_error = None;
            pending_selection = None;
            create_dir_on_enter = false;
            history_idx = None;
          }
      | ("Up" | "Down"), Some tb ->
          let len = List.length w.history in
          if len = 0 then w
          else
            let idx =
              match (key, w.history_idx) with
              | "Up", None -> 0
              | "Down", None -> 0
              | "Up", Some i -> min (len - 1) (i + 1)
              | "Down", Some i -> max 0 (i - 1)
              | _ -> 0
            in
            let text = List.nth w.history idx in
            {
              w with
              history_idx = Some idx;
              textbox = Some (textbox_set_text tb text);
            }
      | "Tab", Some tb -> (
          (* Completion forward - always include hidden files for tab completion *)
          let buf = textbox_get_text tb in
          let dir = Filename.dirname buf in
          let base = Filename.basename buf in
          let candidates =
            list_entries_safe dir ~dirs_only:false ~show_hidden:true
          in
          let names = List.map (fun e -> e.name) candidates in
          let matches =
            List.filter (fun n -> String.starts_with ~prefix:base n) names
          in
          let choose name =
            let newp = Filename.concat dir name in
            let is_dir =
              List.exists (fun e -> e.name = name && e.is_dir) candidates
            in
            let newp = if is_dir then newp ^ "/" else newp in
            {
              w with
              textbox = Some (textbox_set_text tb newp);
              path_error = None;
            }
          in
          match matches with
          | [] -> w
          | [one] -> choose one
          | many -> choose (List.hd many))
      | "Shift-Tab", Some tb -> (
          (* Completion backward - always include hidden files for tab completion *)
          let buf = textbox_get_text tb in
          let dir = Filename.dirname buf in
          let base = Filename.basename buf in
          let candidates =
            list_entries_safe dir ~dirs_only:false ~show_hidden:true
          in
          let names = List.map (fun e -> e.name) candidates in
          let matches =
            List.filter (fun n -> String.starts_with ~prefix:base n) names
          in
          let choose name =
            let newp = Filename.concat dir name in
            let is_dir =
              List.exists (fun e -> e.name = name && e.is_dir) candidates
            in
            let newp = if is_dir then newp ^ "/" else newp in
            {
              w with
              textbox = Some (textbox_set_text tb newp);
              path_error = None;
            }
          in
          match List.rev matches with [] -> w | one :: _ -> choose one)
      | "Enter", Some tb ->
          let sys = Miaou_interfaces.System.require () in
          let p =
            let s = textbox_get_text tb in
            if s = "" then w.current_path else s
          in
          let p =
            if Filename.is_relative p then Filename.concat w.current_path p
            else p
          in
          if w.create_dir_on_enter then
            if sys.file_exists p then
              {w with path_error = Some "Already exists"}
            else
              match sys.mkdir p with
              | Ok () ->
                  (* Invalidate cache after creating directory *)
                  invalidate_cache () ;
                  {
                    w with
                    current_path = p;
                    cursor = 0;
                    mode = Browsing;
                    path_error = None;
                    pending_selection = None;
                    history =
                      (let h = List.filter (fun x -> x <> p) w.history in
                       p :: h);
                    history_idx = None;
                    create_dir_on_enter = false;
                  }
              | Error e -> {w with path_error = Some e}
          else
            let exists = sys.file_exists p in
            if not exists then {w with path_error = Some "Path not found"}
            else
              let writable_ok = (not w.require_writable) || is_writable p in
              if sys.is_directory p then
                if writable_ok then
                  {
                    w with
                    current_path = p;
                    cursor = 0;
                    mode = Browsing;
                    path_error = None;
                    pending_selection = None;
                    history =
                      (if p = "" then w.history
                       else
                         let h = List.filter (fun x -> x <> p) w.history in
                         p :: h);
                    history_idx = None;
                  }
                else {w with path_error = Some "Not writable"}
              else if writable_ok then
                {
                  w with
                  pending_selection = Some p;
                  path_error = None;
                  history =
                    (let h = List.filter (fun x -> x <> p) w.history in
                     p :: h);
                  history_idx = None;
                }
              else {w with path_error = Some "Not writable"}
      | _, Some tb -> {w with textbox = Some (textbox_handle_key tb ~key)}
      | _, None -> w)

let render_with_size w ~focus:_ ~(size : LTerm_geom.size) =
  let w = apply_pending_updates w in
  let module W = Miaou_widgets_display.Widgets in
  let human_bytes (n : int64) =
    let open Int64 in
    let f x = Int64.to_float x in
    let units = [|"B"; "KB"; "MB"; "GB"; "TB"|] in
    let rec loop v u =
      if v < 1024L || u = Array.length units - 1 then
        Printf.sprintf "%.1f%s" (f v) units.(u)
      else loop (div v 1024L) (u + 1)
    in
    loop n 0
  in
  let entries =
    list_entries_with_parent
      w.current_path
      ~dirs_only:w.dirs_only
      ~show_hidden:w.show_hidden
  in
  let total = List.length entries in
  let cursor = clamp 0 (if total = 0 then 0 else total - 1) w.cursor in
  let w = {w with cursor} in
  let rows_total = size.LTerm_geom.rows in
  (* Width sizing: compute maximum content width and helpers. *)
  let max_width = max 10 (size.LTerm_geom.cols - 2) in
  let truncate s =
    let len = W.visible_chars_count s in
    if len <= max_width then s
    else if max_width <= 1 then String.make max_width '.'
    else
      let cut = max 0 (max_width - 1) in
      String.sub s 0 cut ^ "."
  in
  let pad_to_width s =
    let v = W.visible_chars_count s in
    if v >= max_width then s else s ^ String.make (max_width - v) ' '
  in
  let module Palette = Miaou_widgets_display.Palette in
  let shorten_path_to p max_len =
    if max_len <= 5 then
      if String.length p <= max_len then p
      else String.sub p 0 (max 0 (max_len - 1)) ^ "…"
    else if String.length p <= max_len then p
    else
      let keep = (max_len - 1) / 2 in
      let first = String.sub p 0 keep in
      let last = String.sub p (String.length p - keep) keep in
      first ^ "…" ^ last
  in
  (* Path editor bar at the top *)
  let path_bar =
    match w.mode with
    | Browsing ->
        let prefix = "Path: [" in
        let suffix = "]" in
        let room =
          max 0 (max_width - (String.length prefix + String.length suffix))
        in
        let label =
          prefix ^ shorten_path_to w.current_path room ^ suffix
          |> truncate |> pad_to_width
        in
        [label]
    | EditingPath ->
        let tb =
          match w.textbox with
          | Some t -> t
          | None -> textbox_create ~initial:w.current_path ()
        in
        let prefix = "Path: " in
        let rendered = textbox_render tb in
        let line = prefix ^ rendered |> truncate |> pad_to_width in
        let line =
          match w.path_error with
          | None -> W.bg 24 (W.fg 15 line)
          | Some msg -> W.fg 9 (line ^ "  " ^ msg)
        in
        [line]
  in
  let hidden_hint = if w.show_hidden then "hide hidden" else "show hidden" in
  let header = path_bar in
  (* Vertical sizing: render exactly [rows_total] lines so parent frames don't
     crop in a way that desynchronizes cursor and viewport. *)
  let footer_pairs =
    [
      ("↑/↓", "navigate");
      ("PgUp/PgDn", "page");
      ("Space", "select");
      ("Enter", "confirm");
      ("Esc", "cancel");
      ("Backspace", "parent");
      ("Tab", "edit path");
      ("h", hidden_hint);
      ("n", "new directory");
    ]
  in
  let footer_controls =
    W.footer_hints_wrapped_capped ~cols:size.cols ~max_lines:2 footer_pairs
  in
  let footer_hint_lines = String.split_on_char '\n' footer_controls in
  let header_lines = List.length header in
  let footer_lines =
    2 + List.length footer_hint_lines
    (* blank + status + footer hint lines *)
  in
  let body_capacity = max 0 (rows_total - header_lines - footer_lines) in
  let max_shown = min total body_capacity in
  let start =
    if total <= max_shown || max_shown <= 0 then 0
    else
      let max_start = max 0 (total - max_shown) in
      let desired = w.cursor - (max_shown - 1) in
      max 0 (min desired max_start)
  in
  let slice =
    entries
    |> List.mapi (fun i v -> (i, v))
    |> List.filter (fun (i, _) -> i >= start && i < start + max_shown)
  in
  let body =
    let sys = Miaou_interfaces.System.require () in
    List.map
      (fun (i, e) ->
        let full =
          if e.name = ".." then Filename.dirname w.current_path
          else Filename.concat w.current_path e.name
        in
        let size_suffix =
          if e.name = ".." then ""
          else if e.is_dir then "/"
          else
            match sys.get_disk_usage ~path:full with
            | Ok bytes -> "  (" ^ human_bytes bytes ^ ")"
            | Error _ -> ""
        in
        let plain = if e.is_dir then e.name ^ "/" else e.name ^ size_suffix in
        let clipped = plain |> truncate in
        let colored = if e.is_dir then W.fg 12 clipped else clipped in
        let label = if w.mode = EditingPath then W.dim colored else colored in
        if i = w.cursor then Palette.selection_bg (Palette.selection_fg label)
        else label)
      slice
  in
  let body =
    if List.length body >= body_capacity then body
    else body @ List.init (body_capacity - List.length body) (fun _ -> "")
  in
  (* Show selectable status near the bottom; footer shows action keys like Enter and n. *)
  let selectable =
    match List.nth_opt entries w.cursor with
    | None -> is_writable w.current_path
    | Some e ->
        let full = Filename.concat w.current_path e.name in
        is_writable full
  in
  let status =
    let base =
      if selectable then W.fg 10 "Selectable" else W.fg 8 "Not writable"
    in
    (* If current item is a file, append size hint to status for quick preview *)
    let s =
      match List.nth_opt entries w.cursor with
      | Some e when not e.is_dir -> (
          let full = Filename.concat w.current_path e.name in
          match
            (Miaou_interfaces.System.require ()).get_disk_usage ~path:full
          with
          | Ok bytes -> base ^ W.dim (" • size " ^ human_bytes bytes)
          | Error _ -> base)
      | _ -> base
    in
    s |> truncate |> pad_to_width
  in
  let padded_footer_hints =
    footer_hint_lines |> List.map (fun l -> W.dim (truncate l |> pad_to_width))
  in
  let sections = header @ body @ ("" :: status :: padded_footer_hints) in
  Helpers.concat_lines sections

let render w ~focus =
  let default_size : LTerm_geom.size = {rows = 24; cols = 80} in
  render_with_size w ~focus ~size:default_size

let mkdir_and_cd browser dirname =
  let sys = Miaou_interfaces.System.require () in
  let new_path = Filename.concat browser.current_path dirname in
  match sys.mkdir new_path with
  | Error e -> Error e
  | Ok () -> (
      (* Invalidate cache after creating directory *)
      invalidate_cache () ;
      (* Verify we can list the new directory *)
      match
        list_entries
          new_path
          ~dirs_only:browser.dirs_only
          ~show_hidden:browser.show_hidden
      with
      | Error e -> Error e
      | Ok _entries ->
          (* Schedule the path update instead of returning it directly *)
          schedule_path_update new_path ;
          (* Return the original browser state - the update will be applied later *)
          Ok (browser, true))
