(******************************************************************************)
(*                                                                            *)
(* SPDX-License-Identifier: MIT                                               *)
(* Copyright (c) 2025-2026 Nomadic Labs <contact@nomadic-labs.com>            *)
(*                                                                            *)
(******************************************************************************)

open Rresult
open Octez_manager_lib
module Help_parser = Octez_manager_lib.Help_parser
open Help_parser
module Keys = Miaou.Core.Keys
module Widgets = Miaou_widgets_display.Widgets
module Textbox_widget = Miaou_widgets_input.Textbox_widget
module Navigation = Miaou.Core.Navigation

let ( let* ) = Result.bind

(* Mutable selection entry used inside the modal. *)
type row = {
  opt : option_entry;
  mutable value : string option;
  mutable selected : bool;
}

(* Cache for binary help output - uses long TTL since binaries rarely change *)
let cache = Cache.create_safe_keyed ~name:"binary_help" ~ttl:3600.0 ()

let set_help_hint ?short ?long () =
  Miaou.Core.Help_hint.clear () ;
  match (short, long) with
  | None, None -> ()
  | _ -> Miaou.Core.Help_hint.push ?short ?long ()

let run_help binary =
  if not (Sys.file_exists binary) then
    Error (`Msg (Printf.sprintf "Binary not found at %s" binary))
  else
    Common.run_out
      [
        "env";
        "MANPAGER=cat";
        "PAGER=cat";
        "TERM=dumb";
        binary;
        "run";
        "--help=plain";
      ]

let run_help_cmd binary cmd =
  if not (Sys.file_exists binary) then
    Error (`Msg (Printf.sprintf "Binary not found at %s" binary))
  else
    Common.run_out
      (["env"; "MANPAGER=cat"; "PAGER=cat"; "TERM=dumb"; binary] @ cmd)

let load_options ~binary =
  match Cache.get_safe_keyed_cached cache binary with
  | Some opts -> Ok opts
  | None ->
      let* output = run_help binary in
      let output = strip_ansi output in
      let opts = parse_help_node output in
      if opts = [] then Error (`Msg "No options parsed from help output")
      else (
        Cache.set_safe_keyed cache binary opts ;
        Ok opts)

let render_value = function None -> "" | Some v -> v

let truncate ~max_len s =
  if String.length s <= max_len then s else String.sub s 0 (max_len - 1) ^ "…"

let option_label entry = String.concat ", " (display_names entry.names)

let option_hint_markdown (row : row) : string option * string option =
  let names_md =
    row.opt.names |> display_names
    |> List.map (fun n -> Printf.sprintf "`%s`" n)
    |> String.concat ", "
  in
  let heading =
    match row.opt.arg with
    | None -> Printf.sprintf "### %s" names_md
    | Some arg -> Printf.sprintf "### %s `%s`" names_md arg
  in
  let doc = String.trim row.opt.doc in
  let doc_lines = if doc = "" then [] else String.split_on_char '\n' doc in
  let selection_lines =
    if not row.selected then []
    else
      match row.value with
      | Some v when String.trim v <> "" ->
          [Printf.sprintf "- Selected: `%s`" (String.trim v)]
      | _ -> ["- Selected"]
  in
  let long_lines =
    [heading]
    @ (if doc_lines = [] then [] else [""; String.concat "\n" doc_lines])
    @
    if selection_lines = [] then []
    else [""; String.concat "\n" selection_lines]
  in
  let long =
    match List.filter (fun l -> String.trim l <> "") long_lines with
    | [] -> None
    | _ -> Some (String.concat "\n" long_lines)
  in
  let short =
    match doc_lines with
    | first :: _ when String.trim first <> "" ->
        Some
          (Printf.sprintf
             "**%s** — %s"
             (option_label row.opt)
             (String.trim first))
    | _ -> (
        match selection_lines with
        | first :: _ ->
            Some
              (Printf.sprintf
                 "**%s** — %s"
                 (option_label row.opt)
                 (String.trim first))
        | [] -> None)
  in
  (short, long)

let wrap_text ~width s =
  let wrap_line s =
    let len = String.length s in
    if len <= width then [s]
    else
      let rec aux start acc =
        if start >= len then List.rev acc
        else
          let remaining = len - start in
          if remaining <= width then
            List.rev (String.sub s start remaining :: acc)
          else
            let limit = start + width in
            let end_ =
              try
                let last_space = String.rindex_from s limit ' ' in
                if last_space > start then last_space else limit
              with _ -> limit
            in
            let sub = String.sub s start (end_ - start) in
            let next_start =
              if end_ < len && s.[end_] = ' ' then end_ + 1 else end_
            in
            aux next_start (sub :: acc)
      in
      aux 0 []
  in
  String.split_on_char '\n' s |> List.concat_map wrap_line

let format_tokens rows =
  rows
  |> List.filter (fun r -> r.selected)
  |> List.concat_map (fun r ->
      let flag = primary_name r.opt.names in
      match r.value with None -> [flag] | Some v -> [flag; v])

(* Parse initial args string into (flag, value option) pairs.
   Handles formats like: --flag --flag=value --flag value *)
let parse_initial_args args_str =
  let tokens = Form_builder_common.parse_shellwords args_str in
  let rec loop acc = function
    | [] -> List.rev acc
    | token :: rest ->
        if String.length token > 0 && token.[0] = '-' then
          (* It's a flag *)
          if String.contains token '=' then
            (* --flag=value format *)
            let idx = String.index token '=' in
            let flag = String.sub token 0 idx in
            let value =
              String.sub token (idx + 1) (String.length token - idx - 1)
            in
            loop ((flag, Some value) :: acc) rest
          else
            (* Check if next token is a value (doesn't start with -) *)
            match rest with
            | next :: rest2 when String.length next > 0 && next.[0] <> '-' ->
                loop ((token, Some next) :: acc) rest2
            | _ -> loop ((token, None) :: acc) rest
        else
          (* Skip non-flag tokens *)
          loop acc rest
  in
  loop [] tokens

(* Check if option matches a flag name *)
let option_matches_flag opt flag =
  List.exists (fun name -> String.equal name flag) opt.names

(* Initialize rows from options, pre-selecting based on initial_args *)
let init_rows_from_args options initial_args =
  let parsed = parse_initial_args initial_args in
  List.map
    (fun opt ->
      match
        List.find_opt (fun (flag, _) -> option_matches_flag opt flag) parsed
      with
      | Some (_, value) -> {opt; value; selected = true}
      | None -> {opt; value = None; selected = false})
    options

let open_modal ~title ~options ~initial_args ~on_apply =
  let rows = Array.of_list (init_rows_from_args options initial_args) in
  let module Modal = struct
    type state = {
      rows : row array;
      mutable cursor : int;
      mutable scroll_offset : int;
      mutable last_key : string;
    }

    type msg = unit

    type key_binding = state Miaou.Core.Tui_page.key_binding_desc

    type pstate = state Navigation.t

    let update_help_hint s =
      match s.rows |> Array.to_list |> fun lst -> List.nth_opt lst s.cursor with
      | None -> set_help_hint ()
      | Some row ->
          let short, long = option_hint_markdown row in
          set_help_hint ?short ?long ()

    let init () =
      Navigation.make {rows; cursor = 0; scroll_offset = 0; last_key = ""}

    let update ps _ = ps

    let refresh ps = ps

    let adjust_scroll s ~visible_rows =
      if s.cursor < s.scroll_offset then s.scroll_offset <- s.cursor
      else if s.cursor > s.scroll_offset + visible_rows - 1 then
        s.scroll_offset <- s.cursor - visible_rows + 1

    let move_state s delta =
      let len = Array.length s.rows in
      let next = max 0 (min (len - 1) (s.cursor + delta)) in
      s.cursor <- next ;
      update_help_hint s ;
      s

    let move ps delta = Navigation.update (fun s -> move_state s delta) ps

    let current_row s =
      if Array.length s.rows = 0 then None else Some s.rows.(s.cursor)

    let open_toggle_modal row =
      let doc_lines = wrap_text ~width:68 row.opt.doc in
      Modal_helpers.open_choice_modal_with_hint
        ~title:(option_label row.opt)
        ~items:[true; false]
        ~to_string:(function true -> "Enable" | false -> "Disable")
        ~describe:(fun _ -> doc_lines)
        ~hint:(fun _ ->
          Modal_helpers.open_text_modal
            ~title:(option_label row.opt)
            ~lines:doc_lines)
        ~on_select:(fun v ->
          row.selected <- v ;
          if not v then row.value <- None)
        ()

    let open_value_modal row placeholder =
      let doc_lines = wrap_text ~width:68 row.opt.doc in
      let title = option_label row.opt in
      let initial_value = match row.value with Some v -> v | None -> "" in
      let module Modal = struct
        type state = {textbox : Textbox_widget.t; doc_lines : string list}

        type msg = unit

        type key_binding = state Miaou.Core.Tui_page.key_binding_desc

        type pstate = state Navigation.t

        let init () =
          Navigation.make
            {
              textbox =
                Textbox_widget.open_centered
                  ~title:placeholder
                  ~width:60
                  ~initial:initial_value
                  ();
              doc_lines;
            }

        let update ps _ = ps

        let view ps ~focus ~size:_ =
          let s = ps.Navigation.s in
          let doc_block = String.concat "\n" s.doc_lines in
          let input_block = Textbox_widget.render s.textbox ~focus in
          let hint = Widgets.dim "Enter: confirm · Esc: cancel" in
          doc_block ^ "\n\n" ^ input_block ^ "\n\n" ^ hint

        let move ps _ = ps

        let refresh ps = ps

        let service_select ps _ = ps

        let service_cycle ps _ = ps

        let back ps = ps

        let keymap _ = []

        let handled_keys () = []

        let handle_modal_key ps key ~size:_ =
          let s = ps.Navigation.s in
          let mapped =
            match Keys.of_string key with
            | Some Keys.Enter -> "Enter"
            | Some Keys.Backspace -> "Backspace"
            | Some (Keys.Char "Esc") | Some (Keys.Char "Escape") -> "Esc"
            | _ -> key
          in
          if mapped = "Enter" then (
            let value = Textbox_widget.get_text s.textbox in
            if String.trim value <> "" then (
              row.selected <- true ;
              row.value <- Some value) ;
            Miaou.Core.Modal_manager.set_consume_next_key () ;
            Miaou.Core.Modal_manager.close_top `Commit ;
            ps)
          else if mapped = "Esc" then (
            Miaou.Core.Modal_manager.set_consume_next_key () ;
            Miaou.Core.Modal_manager.close_top `Cancel ;
            ps)
          else
            Navigation.update
              (fun s ->
                {s with textbox = Textbox_widget.handle_key s.textbox ~key})
              ps

        let handle_key = handle_modal_key

        let has_modal _ = true
      end in
      let ui : Miaou.Core.Modal_manager.ui =
        {title; left = None; max_width = Some (Fixed 76); dim_background = true}
      in
      Miaou.Core.Modal_manager.push
        (module Modal)
        ~init:(Modal.init ())
        ~ui
        ~commit_on:[]
        ~cancel_on:[]
        ~on_close:(fun _ _ -> ())

    let edit_value s =
      s.last_key <- "edit_value called" ;
      match current_row s with
      | None -> s.last_key <- "edit_value: no row"
      | Some row -> (
          s.last_key <-
            Printf.sprintf
              "edit_value: %s kind=%s"
              (option_label row.opt)
              (match row.opt.kind with
              | Toggle -> "toggle"
              | Value _ -> "value") ;
          match row.opt.kind with
          | Toggle -> open_toggle_modal row
          | Value _ -> (
              match row.opt.arg with
              | None -> open_value_modal row "value"
              | Some placeholder -> open_value_modal row placeholder))

    let show_hint s =
      match current_row s with
      | None -> ()
      | Some row ->
          let short, long = option_hint_markdown row in
          (* Use Help_hint so pressing '?' shows the doc without opening a modal *)
          set_help_hint ?short ?long ()

    let apply_and_close s =
      let tokens = s.rows |> Array.to_list |> format_tokens in
      on_apply tokens ;
      Miaou.Core.Modal_manager.close_top `Commit

    let view ps ~focus:_ ~size =
      let s = ps.Navigation.s in
      update_help_hint s ;
      let width = size.LTerm_geom.cols in
      let height = size.LTerm_geom.rows in
      (* Reserve space for header (3 lines) + footer (2 lines) + modal frame borders (4 lines) *)
      let visible_rows = max 1 (height - 9) in
      adjust_scroll s ~visible_rows ;
      let opt_width = max 18 (min 40 (width / 2)) in
      let arg_width = max 10 (min 24 (width / 3)) in
      let value_width = max 8 (width - opt_width - arg_width - 12) in
      let render_row idx row =
        let marker = if idx = s.cursor then Widgets.bold "➤" else " " in
        let checkbox = if row.selected then "●" else "○" in
        let opt = truncate ~max_len:opt_width (option_label row.opt) in
        let arg =
          truncate ~max_len:arg_width (Option.value ~default:"" row.opt.arg)
        in
        let value =
          match row.value with
          | None -> ""
          | Some v ->
              let trimmed = truncate ~max_len:value_width v in
              Printf.sprintf "[%s]" trimmed
        in
        let header =
          Printf.sprintf
            "%s %s %-*s %-*s %s"
            marker
            checkbox
            opt_width
            opt
            arg_width
            arg
            value
        in
        let header = if row.selected then Widgets.fg 22 header else header in
        header
      in
      let summary_tokens = format_tokens (Array.to_list s.rows) in
      let summary_line =
        match summary_tokens with
        | [] -> Widgets.dim "No flags selected"
        | ts -> Widgets.fg 22 (String.concat " " ts)
      in
      (* Only render visible rows *)
      let all_rows = Array.to_list s.rows in
      let visible =
        all_rows
        |> List.mapi (fun idx row -> (idx, row))
        |> List.filter (fun (idx, _) ->
            idx >= s.scroll_offset && idx < s.scroll_offset + visible_rows)
        |> List.map (fun (idx, row) -> render_row idx row)
      in
      let total = Array.length s.rows in
      let _scroll_indicator =
        if total > visible_rows then
          Printf.sprintf
            " [off=%d cur=%d len=%d vis=%d]"
            s.scroll_offset
            s.cursor
            total
            visible_rows
        else ""
      in
      let body = String.concat "\n" visible in
      Miaou_widgets_layout.Vsection.render
        ~size
        ~header:
          [
            Widgets.title_highlight
              (Printf.sprintf
                 "%s [%s] off=%d cur=%d"
                 title
                 s.last_key
                 s.scroll_offset
                 s.cursor);
            summary_line;
            "";
          ]
        ~content_footer:[]
        ~child:(fun _ -> body)

    let handle_modal_key ps key ~size:_ =
      let s = ps.Navigation.s in
      (* Store key for debug display *)
      s.last_key <- Printf.sprintf "%s(handler)" key ;
      (* Check raw key strings first (as sent by Miaou), then try Keys.of_string *)
      if key = "Enter" || key = "Return" then (
        edit_value s ;
        ps)
      else if key = " " || key = "Space" then (
        (match current_row s with
        | None -> ()
        | Some row -> (
            match row.opt.kind with
            | Toggle -> row.selected <- not row.selected
            | Value _ ->
                (* For value flags: if selected, toggle off; otherwise open value modal *)
                if row.selected then (
                  row.selected <- false ;
                  row.value <- None)
                else edit_value s)) ;
        update_help_hint s ;
        ps)
      else if key = "Backspace" || key = "\b" then (
        (match current_row s with
        | None -> ()
        | Some row ->
            row.selected <- false ;
            row.value <- None) ;
        update_help_hint s ;
        ps)
      else if key = "s" || key = "S" then (
        apply_and_close s ;
        ps)
      else if key = "?" then (
        show_hint s ;
        ps)
      else if key = "Esc" || key = "Escape" || key = "q" || key = "Q" then (
        Miaou.Core.Modal_manager.close_top `Cancel ;
        ps)
      else
        (* Fall back to Keys.of_string for special keys *)
        match Keys.of_string key with
        | Some Keys.Up -> move ps (-1)
        | Some Keys.Down -> move ps 1
        | Some (Keys.Char "k") -> move ps (-1)
        | Some (Keys.Char "j") -> move ps 1
        | Some Keys.Enter ->
            edit_value s ;
            ps
        | Some Keys.Backspace ->
            (match current_row s with
            | None -> ()
            | Some row ->
                row.selected <- false ;
                row.value <- None) ;
            update_help_hint s ;
            ps
        | _ -> ps

    let handle_key = handle_modal_key

    let service_select ps _ = ps

    let service_cycle ps _ = ps

    let back ps = ps

    let handled_keys () =
      Miaou.Core.Keys.[Up; Down; Enter; Char " "; Backspace; Char "s"; Char "?"]

    let keymap ps =
      let s = ps.Navigation.s in
      let kb key action help =
        {Miaou.Core.Tui_page.key; action; help; display_only = false}
      in
      [
        kb
          "Up"
          (fun ps ->
            s.last_key <- "Up(keymap)" ;
            move ps (-1))
          "Up";
        kb
          "Down"
          (fun ps ->
            s.last_key <- "Down(keymap)" ;
            move ps 1)
          "Down";
        kb
          "Enter"
          (fun ps ->
            s.last_key <- "Enter(keymap)" ;
            edit_value s ;
            ps)
          "Edit/Select";
        kb
          "Return"
          (fun ps ->
            s.last_key <- "Return(keymap)" ;
            edit_value s ;
            ps)
          "Edit/Select";
        kb
          "Space"
          (fun ps ->
            s.last_key <- "Space(keymap)" ;
            (match current_row s with
            | None -> ()
            | Some row -> (
                match row.opt.kind with
                | Toggle -> row.selected <- not row.selected
                | Value _ ->
                    if row.selected then (
                      row.selected <- false ;
                      row.value <- None)
                    else edit_value s)) ;
            ps)
          "Toggle/Edit";
        kb
          "Backspace"
          (fun ps ->
            s.last_key <- "Backspace(keymap)" ;
            (match current_row s with
            | None -> ()
            | Some row ->
                row.selected <- false ;
                row.value <- None) ;
            ps)
          "Clear";
        kb
          "s"
          (fun ps ->
            s.last_key <- "s(keymap)" ;
            apply_and_close s ;
            ps)
          "Apply";
        {
          Miaou.Core.Tui_page.key = "?";
          action =
            (fun ps ->
              s.last_key <- "?(keymap)" ;
              show_hint s ;
              ps);
          help = "Help";
          display_only = true;
        };
      ]

    let has_modal _ = true
  end in
  let ui : Miaou.Core.Modal_manager.ui =
    {title; left = None; max_width = Some (Fixed 96); dim_background = true}
  in
  (* Use push instead of push_default so we can control commit_on/cancel_on.
     We handle Enter ourselves to open nested modals, so commit_on must be empty
     to prevent the parent modal from closing when Enter is pressed. *)
  Miaou.Core.Modal_manager.push
    (module Modal)
    ~init:(Modal.init ())
    ~ui
    ~commit_on:[]
    ~cancel_on:[]
    ~on_close:(fun pstate _reason ->
      (* Apply current selections even when closing with Esc to keep extra args in sync. *)
      let tokens = pstate.Navigation.s.rows |> Array.to_list |> format_tokens in
      on_apply tokens ;
      Miaou.Core.Help_hint.clear ())

(* Options to exclude from the node flags modal:
   - Meta options (--help, --version)
   - Options already covered by form fields *)
let excluded_node_options =
  [
    "--help";
    "-help";
    "--version";
    "--data-dir";
    "-d";
    (* Form: Data Dir *)
    "--rpc-addr";
    (* Form: RPC Address *)
    "--net-addr";
    (* Form: P2P Address *)
    "--network";
    (* Form: Network *)
    "--history-mode";
    (* Form: History Mode *)
    "--log-output";
    (* Form: Logging *)
  ]

let name_matches_excluded name excluded =
  name = excluded
  || String.length name >= String.length excluded
     && String.sub name 0 (String.length excluded) = excluded

let is_excluded_option opt ~excluded =
  (* Check if any of the option's names starts with an excluded prefix *)
  List.exists
    (fun excluded ->
      List.exists (fun name -> name_matches_excluded name excluded) opt.names)
    excluded

let open_node_run_help ~app_bin_dir ~initial_args ~on_apply =
  let app_bin_dir = String.trim app_bin_dir in
  if app_bin_dir = "" then
    Modal_helpers.show_error ~title:"Node Flags" "Octez bin directory is empty"
  else
    let binary = Filename.concat app_bin_dir "octez-node" in
    match load_options ~binary with
    | Ok options ->
        let filtered =
          List.filter
            (fun opt ->
              not (is_excluded_option opt ~excluded:excluded_node_options))
            options
        in
        open_modal ~title:"Node Flags" ~options:filtered ~initial_args ~on_apply
    | Error (`Msg msg) -> Modal_helpers.show_error ~title:"Node Flags" msg

let excluded_baker_options =
  [
    "--help";
    "-help";
    "--version";
    "--base-dir";
    "--endpoint";
    "--delegate";
    "--dal-node";
    "--log-file";
    "--chain";
  ]

type baker_mode = [`Local | `Remote]

let load_baker_options ~binary ~mode =
  let cache_key =
    Printf.sprintf
      "%s:baker:%s"
      binary
      (match mode with `Local -> "local" | `Remote -> "remote")
  in
  match Cache.get_safe_keyed_cached cache cache_key with
  | Some opts -> Ok opts
  | None ->
      let try_cmd label cmd =
        match run_help_cmd binary cmd with
        | Error (`Msg msg) -> Error (`Msg (Printf.sprintf "%s: %s" label msg))
        | Ok output ->
            let opts = parse_help_baker (strip_ansi output) in
            if opts = [] then
              Error
                (`Msg
                   (Printf.sprintf
                      "%s: no options parsed from help output"
                      label))
            else Ok opts
      in
      let candidates =
        match mode with
        | `Local ->
            [
              ("local", ["run"; "with"; "local"; "node"; "/dev/null"; "--help"]);
              ( "remote-fallback",
                [
                  "run";
                  "with";
                  "remote";
                  "node";
                  "http://127.0.0.1:8732";
                  "--help";
                ] );
              ("help", ["--help"]);
              ("run-help", ["run"; "--help"]);
            ]
        | `Remote ->
            [
              ( "remote",
                [
                  "run";
                  "with";
                  "remote";
                  "node";
                  "http://127.0.0.1:8732";
                  "--help";
                ] );
              ("help", ["--help"]);
              ("run-help", ["run"; "--help"]);
              ( "local-fallback",
                ["run"; "with"; "local"; "node"; "/dev/null"; "--help"] );
            ]
      in
      let rec loop errs = function
        | [] -> Error (`Msg (String.concat " | " (List.rev errs)))
        | (label, cmd) :: rest -> (
            match try_cmd label cmd with
            | Ok opts -> Ok opts
            | Error (`Msg m) -> loop (m :: errs) rest)
      in
      let* opts = loop [] candidates in
      Cache.set_safe_keyed cache cache_key opts ;
      Ok opts

let open_baker_run_help ~app_bin_dir ~mode ~initial_args ~on_apply =
  let app_bin_dir = String.trim app_bin_dir in
  let title =
    match mode with
    | `Local -> "Baker Flags · Local Node"
    | `Remote -> "Baker Flags · Remote Node"
  in
  if app_bin_dir = "" then
    Modal_helpers.show_error ~title "Octez bin directory is empty"
  else
    let binary = Filename.concat app_bin_dir "octez-baker" in
    match load_baker_options ~binary ~mode with
    | Ok options ->
        let filtered =
          List.filter
            (fun opt ->
              not (is_excluded_option opt ~excluded:excluded_baker_options))
            options
        in
        open_modal ~title ~options:filtered ~initial_args ~on_apply
    | Error (`Msg msg) -> Modal_helpers.show_error ~title msg

let excluded_accuser_options =
  ["--help"; "-help"; "--version"; "--base-dir"; "--endpoint"]

let load_accuser_options ~binary =
  let cache_key = Printf.sprintf "%s:accuser" binary in
  match Cache.get_safe_keyed_cached cache cache_key with
  | Some opts -> Ok opts
  | None ->
      let* output = run_help_cmd binary ["run"; "accuser"; "--help"] in
      let opts = parse_help_baker (strip_ansi output) in
      if opts = [] then
        Error (`Msg "No options parsed from accuser help output")
      else (
        Cache.set_safe_keyed cache cache_key opts ;
        Ok opts)

let open_accuser_run_help ~app_bin_dir ~initial_args ~on_apply =
  let app_bin_dir = String.trim app_bin_dir in
  let title = "Accuser Flags" in
  if app_bin_dir = "" then
    Modal_helpers.show_error ~title "Octez bin directory is empty"
  else
    let binary = Filename.concat app_bin_dir "octez-baker" in
    match load_accuser_options ~binary with
    | Ok options ->
        let filtered =
          List.filter
            (fun opt ->
              not (is_excluded_option opt ~excluded:excluded_accuser_options))
            options
        in
        open_modal ~title ~options:filtered ~initial_args ~on_apply
    | Error (`Msg msg) -> Modal_helpers.show_error ~title msg

let excluded_dal_options =
  [
    "--help";
    "-help";
    "--version";
    "--endpoint";
    "--data-dir";
    "--rpc-addr";
    "--net-addr";
  ]

let load_dal_options ~binary =
  let cache_key = Printf.sprintf "%s:dal" binary in
  match Cache.get_safe_keyed_cached cache cache_key with
  | Some opts -> Ok opts
  | None ->
      let* output = run_help_cmd binary ["run"; "--help"] in
      let opts = parse_help_baker (strip_ansi output) in
      if opts = [] then Error (`Msg "No options parsed from DAL help output")
      else (
        Cache.set_safe_keyed cache cache_key opts ;
        Ok opts)

let open_dal_run_help ~app_bin_dir ~initial_args ~on_apply =
  let app_bin_dir = String.trim app_bin_dir in
  let title = "DAL Flags" in
  if app_bin_dir = "" then
    Modal_helpers.show_error ~title "Octez bin directory is empty"
  else
    let binary = Filename.concat app_bin_dir "octez-dal-node" in
    match load_dal_options ~binary with
    | Ok options ->
        let filtered =
          List.filter
            (fun opt ->
              not (is_excluded_option opt ~excluded:excluded_dal_options))
            options
        in
        open_modal ~title ~options:filtered ~initial_args ~on_apply
    | Error (`Msg msg) -> Modal_helpers.show_error ~title msg

module For_tests = struct
  let parse_help = parse_help_node

  let parse_baker_help = parse_help_baker

  let arg_kind_to_string = function
    | Toggle -> "toggle"
    | Value Addr_port -> "addr_port"
    | Value Port -> "port"
    | Value Addr -> "addr"
    | Value File -> "file"
    | Value Dir -> "dir"
    | Value Path -> "path"
    | Value Number -> "number"
    | Value Float -> "float"
    | Value Text -> "text"

  let parse_initial_args = parse_initial_args
end
