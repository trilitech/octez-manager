(******************************************************************************)
(*                                                                            *)
(* SPDX-License-Identifier: MIT                                               *)
(* Copyright (c) 2025-2026 Nomadic Labs <contact@nomadic-labs.com>            *)
(*                                                                            *)
(******************************************************************************)

open Octez_manager_lib
module Pager = Miaou_widgets_display.Pager_widget
module Select_widget = Miaou_widgets_input.Select_widget
module Textbox_widget = Miaou_widgets_input.Textbox_widget
module Widgets = Miaou_widgets_display.Widgets
module Navigation = Miaou.Core.Navigation

let first_nonempty_line lines =
  List.find_opt (fun l -> String.trim l <> "") lines

let set_markdown_hint ?short ?long () =
  Miaou.Core.Help_hint.clear () ;
  match (short, long) with
  | None, None -> ()
  | _ -> Miaou.Core.Help_hint.push ?short ?long ()

let open_text_modal ~title ~lines =
  let module Modal = struct
    type state = Pager.t

    type msg = unit

    type key_binding = state Miaou.Core.Tui_page.key_binding_desc

    type pstate = state Navigation.t

    let init () = Navigation.make (Pager.open_lines ~title:"" lines)

    let update ps _ = ps

    let view ps ~focus ~size =
      let s = ps.Navigation.s in
      let rows = max 1 (size.LTerm_geom.rows - 4) in
      (* Clamp columns so rendered content never exceeds the modal's inner width *)
      let cols =
        let inner = max 1 (size.LTerm_geom.cols - 2) in
        min inner 72
      in
      Pager.render ~win:rows ~cols s ~focus

    let move ps _ = ps

    let refresh ps = ps

    let service_select ps _ = ps

    let service_cycle ps _ = ps

    let back ps = ps

    let keymap _ = []

    let handled_keys () = []

    let handle_modal_key ps key ~size =
      let s = ps.Navigation.s in
      (* Check if pager is in input mode (search/lookup/help) *)
      let pager_in_input_mode =
        match s.Pager.input_mode with
        | `Search_edit | `Lookup | `Help -> true
        | `None -> false
      in

      let key =
        match Miaou.Core.Keys.of_string key with
        | Some Miaou.Core.Keys.Up -> "Up"
        | Some Miaou.Core.Keys.Down -> "Down"
        | Some (Miaou.Core.Keys.Char "k") -> "Up"
        | Some (Miaou.Core.Keys.Char "j") -> "Down"
        | Some (Miaou.Core.Keys.Char "Page_up") -> "Page_up"
        | Some (Miaou.Core.Keys.Char "Page_down") -> "Page_down"
        | Some (Miaou.Core.Keys.Char "Home") -> "g"
        | Some (Miaou.Core.Keys.Char "End") -> "G"
        | Some (Miaou.Core.Keys.Char "Esc")
        | Some (Miaou.Core.Keys.Char "Escape")
        | Some (Miaou.Core.Keys.Char "q") ->
            "Esc"
        | _ -> key
      in
      (* Don't call close_top here - Modal_manager.handle_key handles it via
         cancel_on. Just return state unchanged for Esc when not in input mode. *)
      if key = "Esc" && not pager_in_input_mode then ps
      else
        let rows = max 1 (size.LTerm_geom.rows - 4) in
        let win = rows in
        let pager, _ = Pager.handle_key ~win s ~key in
        Navigation.update (fun _ -> pager) ps

    let handle_key = handle_modal_key

    let has_modal _ = true
  end in
  let ui : Miaou.Core.Modal_manager.ui =
    (* Limit modal width so header/separator stay on a single line *)
    {title; left = None; max_width = Some (Fixed 76); dim_background = true}
  in
  Miaou.Core.Modal_manager.push_default
    (module Modal)
    ~init:(Modal.init ())
    ~ui
    ~on_close:(fun _ _ -> ())

let open_choice_modal (type choice) ~title ~(items : choice list) ~to_string
    ?on_tick ~on_select () =
  let module Modal = struct
    type state = choice Select_widget.t

    type msg = unit

    type key_binding = state Miaou.Core.Tui_page.key_binding_desc

    type pstate = state Navigation.t

    let init () = failwith "choice modal init provided by caller"

    let update ps _ = ps

    let view ps ~focus ~size =
      Select_widget.render_with_size ps.Navigation.s ~focus ~size

    let move ps _ = ps

    let refresh ps = ps

    let service_select ps _ = ps

    let service_cycle ps _ =
      match on_tick with
      | Some f ->
          f () ;
          (* Force redraw for animation *)
          Navigation.update (fun s -> s) ps
      | None -> ps

    let back ps = ps

    let keymap _ = []

    let handled_keys () = []

    let handle_modal_key ps key ~size:_ =
      let s = ps.Navigation.s in
      let key =
        match Miaou.Core.Keys.of_string key with
        | Some Miaou.Core.Keys.Up -> "Up"
        | Some Miaou.Core.Keys.Down -> "Down"
        | Some (Miaou.Core.Keys.Char "k") -> "Up"
        | Some (Miaou.Core.Keys.Char "j") -> "Down"
        | Some (Miaou.Core.Keys.Char "Page_up") -> "PageUp"
        | Some (Miaou.Core.Keys.Char "Page_down") -> "PageDown"
        | Some (Miaou.Core.Keys.Char "Home") -> "Home"
        | Some (Miaou.Core.Keys.Char "End") -> "End"
        | Some Miaou.Core.Keys.Enter -> "Enter"
        | Some (Miaou.Core.Keys.Char "Esc")
        | Some (Miaou.Core.Keys.Char "Escape")
        | Some (Miaou.Core.Keys.Char "q") ->
            "Esc"
        | _ -> key
      in
      (* Don't call close_top here - Modal_manager.handle_key already handles
         commit/cancel based on commit_on/cancel_on keys. Only handle selection
         navigation here. *)
      if key = "Enter" || key = "Esc" then ps
      else Navigation.update (fun _ -> Select_widget.handle_key s ~key) ps

    let handle_key = handle_modal_key

    let has_modal _ = true
  end in
  let widget = Select_widget.open_centered ~title:"" ~items ~to_string () in
  let ui : Miaou.Core.Modal_manager.ui =
    {title; left = None; max_width = Some (Fixed 80); dim_background = true}
  in
  Miaou.Core.Modal_manager.push_default
    (module Modal)
    ~init:(Navigation.make widget)
    ~ui
    ~on_close:(fun pstate -> function
      | `Commit -> (
          match Select_widget.get_selection pstate.Navigation.s with
          | Some choice -> on_select choice
          | None -> ())
      | `Cancel -> ())

let open_choice_modal_with_hint (type choice) ~title ~(items : choice list)
    ~to_string ~hint ~describe ~on_select () =
  let describe_fn = describe in
  (* Helper to update the Miaou Help_hint based on selected item *)
  let update_help_hint s =
    let doc_lines =
      match Select_widget.get_selection s with
      | Some choice -> describe_fn choice
      | None -> ( match items with hd :: _ -> describe_fn hd | [] -> [])
    in
    let short_text =
      match first_nonempty_line doc_lines with
      | None -> None
      | Some first -> Some (Printf.sprintf "**%s** — %s" title first)
    in
    let long_text =
      match doc_lines with
      | [] -> None
      | lines ->
          Some (Printf.sprintf "### %s\n\n%s" title (String.concat "\n" lines))
    in
    set_markdown_hint ?short:short_text ?long:long_text ()
  in
  let module Modal = struct
    type state = choice Select_widget.t

    type msg = unit

    type key_binding = state Miaou.Core.Tui_page.key_binding_desc

    type pstate = state Navigation.t

    let init () = failwith "choice modal init provided by caller"

    let update ps _ = ps

    let view ps ~focus ~size =
      let s = ps.Navigation.s in
      (* Update Help_hint whenever rendering so ? shows current selection's doc *)
      update_help_hint s ;
      Select_widget.render_with_size s ~focus ~size

    let move ps _ = ps

    let refresh ps = ps

    let service_select ps _ = ps

    let service_cycle ps _ = ps

    let back ps = ps

    let keymap ps =
      let noop ps = ps in
      let kb key help =
        {Miaou.Core.Tui_page.key; action = noop; help; display_only = true}
      in
      let s = ps.Navigation.s in
      let doc_lines =
        match Select_widget.get_selection s with
        | Some choice -> describe_fn choice
        | None -> ( match items with hd :: _ -> describe_fn hd | [] -> [])
      in
      let doc_entries =
        doc_lines
        |> List.filter (fun l -> String.trim l <> "")
        |> List.mapi (fun idx line ->
            kb (Printf.sprintf "doc%d" (idx + 1)) line)
      in
      (* Keep keymap mostly for displaying help; handlers are no-op to avoid
         interfering with handle_modal_key. *)
      [
        kb "Enter" "Select";
        kb "Up" "Up";
        kb "Down" "Down";
        kb "PageUp" "Page up";
        kb "PageDown" "Page down";
        kb "Home" "Top";
        kb "End" "Bottom";
        kb "?" "Show description";
      ]
      @ doc_entries

    let handled_keys () = []

    let handle_modal_key ps key ~size:_ =
      let s = ps.Navigation.s in
      let mapped =
        match Miaou.Core.Keys.of_string key with
        | Some Miaou.Core.Keys.Up -> "Up"
        | Some Miaou.Core.Keys.Down -> "Down"
        | Some (Miaou.Core.Keys.Char "k") -> "Up"
        | Some (Miaou.Core.Keys.Char "j") -> "Down"
        | Some (Miaou.Core.Keys.Char "Page_up") -> "PageUp"
        | Some (Miaou.Core.Keys.Char "Page_down") -> "PageDown"
        | Some (Miaou.Core.Keys.Char "Home") -> "Home"
        | Some (Miaou.Core.Keys.Char "End") -> "End"
        | Some Miaou.Core.Keys.Enter -> "Enter"
        | Some (Miaou.Core.Keys.Char "?") -> "Hint"
        | Some (Miaou.Core.Keys.Char "Esc")
        | Some (Miaou.Core.Keys.Char "Escape")
        | Some (Miaou.Core.Keys.Char "q") ->
            "Esc"
        | _ -> key
      in
      let key = if mapped = "?" then "Hint" else mapped in
      if key = "Enter" then (
        Miaou.Core.Modal_manager.set_consume_next_key () ;
        Miaou.Core.Modal_manager.close_top `Commit ;
        ps)
      else if key = "Hint" then (
        (match Select_widget.get_selection s with
        | Some choice -> hint choice
        | None -> ( match items with hd :: _ -> hint hd | [] -> ())) ;
        ps)
      else if key = "Esc" then (
        Miaou.Core.Modal_manager.set_consume_next_key () ;
        Miaou.Core.Modal_manager.close_top `Cancel ;
        ps)
      else Navigation.update (fun _ -> Select_widget.handle_key s ~key) ps

    let handle_key = handle_modal_key

    let has_modal _ = true
  end in
  let widget = Select_widget.open_centered ~title:"" ~items ~to_string () in
  (* Set initial Help_hint for the default selection *)
  update_help_hint widget ;
  let ui : Miaou.Core.Modal_manager.ui =
    {title; left = None; max_width = Some (Fixed 80); dim_background = true}
  in
  (* Use push with empty commit_on/cancel_on since we handle Enter/Esc manually
     in handle_modal_key. This prevents double-close when used as nested modal. *)
  Miaou.Core.Modal_manager.push
    (module Modal)
    ~init:(Navigation.make widget)
    ~ui
    ~commit_on:[]
    ~cancel_on:[]
    ~on_close:(fun pstate -> function
      | `Commit -> (
          Miaou.Core.Help_hint.clear () ;
          match Select_widget.get_selection pstate.Navigation.s with
          | Some choice -> on_select choice
          | None -> ())
      | `Cancel -> Miaou.Core.Help_hint.clear ())

let prompt_text_modal ?title ?(width = 60) ?initial ?placeholder ~on_submit () =
  let module Modal = struct
    type state = Textbox_widget.t

    type msg = unit

    type key_binding = state Miaou.Core.Tui_page.key_binding_desc

    type pstate = state Navigation.t

    let init () = failwith "textbox modal init provided by caller"

    let update ps _ = ps

    let view ps ~focus ~size:_ = Textbox_widget.render ps.Navigation.s ~focus

    let move ps _ = ps

    let refresh ps = ps

    let service_select ps _ = ps

    let service_cycle ps _ = ps

    let back ps = ps

    let keymap _ = []

    let handled_keys () = []

    let handle_modal_key ps key ~size:_ =
      let s = ps.Navigation.s in
      if key = "Enter" then (
        Miaou.Core.Modal_manager.close_top `Commit ;
        ps)
      else if key = "Esc" || key = "Escape" then (
        Miaou.Core.Modal_manager.close_top `Cancel ;
        ps)
      else Navigation.update (fun _ -> Textbox_widget.handle_key s ~key) ps

    let handle_key = handle_modal_key

    let has_modal _ = true
  end in
  let widget =
    Textbox_widget.open_centered ?title ~width ?initial ?placeholder ()
  in
  let modal_title = Option.value ~default:"Input" title in
  Miaou.Core.Modal_manager.prompt
    (module Modal)
    ~init:(Navigation.make widget)
    ~title:modal_title
    ~extract:(fun pstate -> Some (Textbox_widget.get_text pstate.Navigation.s))
    ~on_result:(function Some text -> on_submit text | None -> ())
    ()

let open_multiselect_modal (type choice) ~title ~(items : unit -> choice list)
    ~to_string ~on_select =
  let module Modal = struct
    type state = choice Select_widget.t

    type msg = unit

    type key_binding = state Miaou.Core.Tui_page.key_binding_desc

    type pstate = state Navigation.t

    let init () = failwith "multiselect modal init provided by caller"

    let update ps _ = ps

    let view ps ~focus ~size =
      Select_widget.render_with_size ps.Navigation.s ~focus ~size

    let move ps _ = ps

    let refresh ps = ps

    let service_select ps _ = ps

    let service_cycle ps _ = ps

    let back ps = ps

    let keymap _ = []

    let handled_keys () = []

    let handle_modal_key ps key ~size:_ =
      let s = ps.Navigation.s in
      let key =
        match Miaou.Core.Keys.of_string key with
        | Some Miaou.Core.Keys.Up -> "Up"
        | Some Miaou.Core.Keys.Down -> "Down"
        | Some (Miaou.Core.Keys.Char "k") -> "Up"
        | Some (Miaou.Core.Keys.Char "j") -> "Down"
        | Some (Miaou.Core.Keys.Char "Page_up") -> "PageUp"
        | Some (Miaou.Core.Keys.Char "Page_down") -> "PageDown"
        | Some (Miaou.Core.Keys.Char "Home") -> "Home"
        | Some (Miaou.Core.Keys.Char "End") -> "End"
        | Some Miaou.Core.Keys.Enter -> "Enter"
        | Some (Miaou.Core.Keys.Char "Esc")
        | Some (Miaou.Core.Keys.Char "Escape")
        | Some (Miaou.Core.Keys.Char "q") ->
            "Esc"
        | _ -> key
      in
      if key = "Enter" then
        match Select_widget.get_selection s with
        | Some choice -> (
            match on_select choice with
            | `KeepOpen ->
                (* Rebuild widget with updated items from the callback *)
                let updated_items = items () in
                Navigation.update
                  (fun _ ->
                    Select_widget.open_centered
                      ~title:""
                      ~items:updated_items
                      ~to_string
                      ())
                  ps
            | `Close ->
                (* Close modal *)
                Miaou.Core.Modal_manager.close_top `Commit ;
                ps)
        | None -> ps
      else if key = "Esc" then (
        Miaou.Core.Modal_manager.close_top `Cancel ;
        ps)
      else Navigation.update (fun _ -> Select_widget.handle_key s ~key) ps

    let handle_key = handle_modal_key

    let has_modal _ = true
  end in
  let widget =
    Select_widget.open_centered ~title:"" ~items:(items ()) ~to_string ()
  in
  let ui : Miaou.Core.Modal_manager.ui =
    {title; left = None; max_width = Some (Fixed 80); dim_background = true}
  in
  (* Use push with empty commit_on/cancel_on since we handle Enter/Esc manually
     in handle_modal_key. This prevents the modal from auto-closing on Enter. *)
  Miaou.Core.Modal_manager.push
    (module Modal)
    ~init:(Navigation.make widget)
    ~ui
    ~commit_on:[]
    ~cancel_on:[]
    ~on_close:(fun _ _ -> ())

let confirm_modal ?title ~message ~on_result () =
  let label = Option.value ~default:"Confirm" title in
  let select_title =
    if String.trim message = "" then label
    else Printf.sprintf "%s\n%s" label message
  in
  open_choice_modal
    ~title:select_title
    ~items:[true; false]
    ~to_string:(function true -> "Yes" | false -> "No")
    ~on_select:on_result
    ()

let prompt_validated_text_modal ?title ?(width = 60) ?initial ?placeholder
    ~validator ~on_submit () =
  let module Modal = struct
    type state = unit Miaou_widgets_input.Validated_textbox_widget.t

    type msg = unit

    type key_binding = state Miaou.Core.Tui_page.key_binding_desc

    type pstate = state Navigation.t

    let init () = failwith "validated textbox modal init provided by caller"

    let update ps _ = ps

    let view ps ~focus ~size:_ =
      Miaou_widgets_input.Validated_textbox_widget.render ps.Navigation.s ~focus

    let move ps _ = ps

    let refresh ps = ps

    let service_select ps _ = ps

    let service_cycle ps _ =
      (* Tick debounced validation *)
      Navigation.update
        (fun s -> Miaou_widgets_input.Validated_textbox_widget.tick s)
        ps

    let back ps = ps

    let keymap _ = []

    let handled_keys () = []

    let handle_modal_key ps key ~size:_ =
      let s = ps.Navigation.s in
      if key = "Enter" then
        (* Flush any pending validation before checking validity *)
        let s =
          Miaou_widgets_input.Validated_textbox_widget.flush_validation s
        in
        if Miaou_widgets_input.Validated_textbox_widget.is_valid s then (
          Miaou.Core.Modal_manager.close_top `Commit ;
          Navigation.update (fun _ -> s) ps)
        else Navigation.update (fun _ -> s) ps
      else if key = "Esc" || key = "Escape" then (
        Miaou.Core.Modal_manager.close_top `Cancel ;
        ps)
      else
        (* Process key and flush validation to avoid stale error messages *)
        let s =
          Miaou_widgets_input.Validated_textbox_widget.handle_key s ~key
        in
        let s =
          Miaou_widgets_input.Validated_textbox_widget.flush_validation s
        in
        Navigation.update (fun _ -> s) ps

    let handle_key = handle_modal_key

    let has_modal _ = true
  end in
  (* Adapt validator from (string -> (unit, string) result) to Miaou's validation_result *)
  let miaou_validator text =
    match validator text with
    | Ok () -> Miaou_widgets_input.Validated_textbox_widget.Valid ()
    | Error msg -> Miaou_widgets_input.Validated_textbox_widget.Invalid msg
  in
  let widget =
    Miaou_widgets_input.Validated_textbox_widget.open_centered
      ?title
      ~width
      ?initial
      ?placeholder
      ~validator:miaou_validator
      ()
  in
  let modal_title = Option.value ~default:"Input" title in
  Miaou.Core.Modal_manager.prompt
    (module Modal)
    ~init:(Navigation.make widget)
    ~title:modal_title
    ~extract:(fun pstate ->
      Some
        (Miaou_widgets_input.Validated_textbox_widget.value pstate.Navigation.s))
    ~on_result:(function Some text -> on_submit text | None -> ())
    ()

let wrap_text ~width s =
  let wrap_line s =
    let len = String.length s in
    if len <= width then [s]
    else
      let rec aux start =
        if start >= len then []
        else
          let remaining = len - start in
          if remaining <= width then [String.sub s start remaining]
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
            sub :: aux next_start
      in
      aux 0
  in
  String.split_on_char '\n' s |> List.map wrap_line |> List.flatten

let show_success ~title message =
  open_text_modal ~title ~lines:["Success"; ""; message]

let show_error ~title message =
  let lines = wrap_text ~width:50 message in
  open_text_modal ~title ~lines:(["Error"; ""] @ lines)

let open_file_browser_modal ?initial_path ~dirs_only ~require_writable
    ~on_select () =
  let module File_browser = Miaou_widgets_layout.File_browser_widget in
  let default_path = if Common.is_root () then "/" else Common.home_dir () in
  let start_path = Option.value initial_path ~default:default_path in
  let module Modal = struct
    type state = File_browser.t

    type msg = unit

    type key_binding = state Miaou.Core.Tui_page.key_binding_desc

    type pstate = state Navigation.t

    let init () =
      Navigation.make
        (File_browser.open_centered
           ~path:start_path
           ~dirs_only
           ~require_writable
           ())

    let update ps _ = ps

    let view ps ~focus ~size =
      File_browser.render_with_size ps.Navigation.s ~focus ~size

    let move ps _ = ps

    let refresh ps = ps

    let service_select ps _ = ps

    let service_cycle ps _ = ps

    let back ps = ps

    let keymap ps =
      let noop ps = ps in
      let hints = File_browser.key_hints ps.Navigation.s in
      let kb key help =
        {Miaou.Core.Tui_page.key; action = noop; help; display_only = true}
      in
      List.map (fun (key, help) -> kb key help) hints @ [kb "?" "Help"]

    let handled_keys () =
      Miaou.Core.Keys.
        [
          Up;
          Down;
          Left;
          Right;
          PageUp;
          PageDown;
          Char " ";
          Enter;
          Escape;
          Backspace;
          Tab;
          Char "h";
          Char "j";
          Char "k";
          Char "l";
          Char "n";
          Char "s";
        ]

    let handle_modal_key ps key ~size:_ =
      Navigation.update
        (fun s ->
          (* Remap keys for navigation *)
          let key =
            match Miaou.Core.Keys.of_string key with
            | Some Miaou.Core.Keys.Right -> "Right"
            | Some Miaou.Core.Keys.Left -> "Left"
            | _ -> key
          in

          let s' = File_browser.handle_key s ~key in
          let s'' = File_browser.apply_pending_updates s' in
          (* Check for cancel *)
          if File_browser.is_cancelled s'' then (
            Miaou.Core.Modal_manager.close_top `Cancel ;
            s'' (* Commit on Enter for files or "." directory *))
          else
            match File_browser.get_pending_selection s'' with
            | Some _ ->
                Miaou.Core.Modal_manager.close_top `Commit ;
                s''
            | None ->
                if key = "s" && File_browser.can_commit s'' then (
                  Miaou.Core.Modal_manager.close_top `Commit ;
                  s'')
                else s'')
        ps

    let handle_key = handle_modal_key

    let has_modal _ = true
  end in
  let ui : Miaou.Core.Modal_manager.ui =
    {
      title =
        "Browse Directory (Space selects, Enter opens/Selects leaf, Esc to \
         cancel)";
      left = None;
      max_width = Some (Fixed 100);
      dim_background = true;
    }
  in
  (* Space commits via Miaou's commit_on, Enter/s handled manually in handle_key *)
  Miaou.Core.Modal_manager.push
    (module Modal)
    ~init:(Modal.init ())
    ~ui
    ~commit_on:["Space"; " "]
    ~cancel_on:["Esc"; "Escape"]
    ~on_close:(fun pstate -> function
      | `Commit -> (
          match
            Miaou_widgets_layout.File_browser_widget.get_selection
              pstate.Navigation.s
          with
          | Some path -> on_select path
          | None -> ())
      | `Cancel -> ())

type dir_choice =
  | Existing_dir of Octez_manager_lib.Directory_registry.directory_entry
  | Browse_new_dir

let select_directory_modal ~title ~dir_type ~on_select () =
  (* Load existing directories from registry *)
  let existing_dirs =
    match Octez_manager_lib.Directory_registry.list ~dir_type () with
    | Ok entries -> entries
    | Error _ -> []
  in

  (* Build choice items: existing dirs + browse option *)
  let items =
    List.map (fun e -> Existing_dir e) existing_dirs @ [Browse_new_dir]
  in

  let to_string = function
    | Existing_dir entry ->
        (* Show path + linked services *)
        let services_str =
          match entry.Octez_manager_lib.Directory_registry.linked_services with
          | [] -> ""
          | svcs -> "  (" ^ String.concat ", " svcs ^ ")"
        in
        entry.Octez_manager_lib.Directory_registry.path ^ services_str
    | Browse_new_dir -> "[ Browse for new directory... ]"
  in

  let on_choice_select = function
    | Existing_dir entry ->
        on_select entry.Octez_manager_lib.Directory_registry.path
    | Browse_new_dir ->
        (* Open file browser *)
        open_file_browser_modal
          ~dirs_only:true
          ~require_writable:false
          ~on_select:(fun path ->
            let trimmed = String.trim path in

            (* Validation *)
            if trimmed = "" then
              show_error ~title:"Invalid Path" "Directory path cannot be empty"
            else if Sys.file_exists trimmed && not (Sys.is_directory trimmed)
            then
              show_error
                ~title:"Invalid Path"
                "Path exists but is not a directory"
            else
              (* Create directory with error handling *)
              let is_writable_dir dir =
                try
                  Unix.access dir [Unix.W_OK; Unix.X_OK] ;
                  true
                with Unix.Unix_error _ -> false
              in
              let user, group =
                Octez_manager_lib.Common.current_user_group_names ()
              in
              let ensure_dir () =
                Octez_manager_lib.Common.ensure_dir_path
                  ~owner:user
                  ~group
                  ~mode:0o755
                  trimmed
              in
              let proceed () =
                (match
                   Octez_manager_lib.Directory_registry.add
                     ~path:trimmed
                     ~dir_type
                     ~linked_services:[]
                 with
                | Ok () -> ()
                | Error (`Msg msg) ->
                    Octez_manager_lib.Common.append_debug_log
                      ("Registry add failed: " ^ msg)) ;
                on_select trimmed
              in
              if Sys.file_exists trimmed then
                if is_writable_dir trimmed then proceed ()
                else
                  show_error
                    ~title:"Permission Denied"
                    (Printf.sprintf "Directory is not writable: %s" trimmed)
              else
                match ensure_dir () with
                | Ok () -> proceed ()
                | Error (`Msg msg) ->
                    show_error ~title:"Directory Creation Failed" msg)
          ()
  in

  open_choice_modal ~title ~items ~to_string ~on_select:on_choice_select ()

let select_node_data_dir_modal ~on_select () =
  select_directory_modal
    ~title:"Select Node Data Directory"
    ~dir_type:Octez_manager_lib.Directory_registry.Node_data_dir
    ~on_select
    ()

let select_client_base_dir_modal ~on_select () =
  select_directory_modal
    ~title:"Select Client Base Directory"
    ~dir_type:Octez_manager_lib.Directory_registry.Client_base_dir
    ~on_select
    ()

let open_download_progress_modal ~version ~on_complete =
  let module Modal = struct
    type state = unit

    type msg = unit

    type key_binding = state Miaou.Core.Tui_page.key_binding_desc

    type pstate = state Navigation.t

    let init () =
      (* Start download in background *)
      Background_runner.enqueue (fun () ->
          (* Initialize multi-progress with list of binaries *)
          Context.multi_progress_start
            ~version
            ~binaries:
              ["octez-node"; "octez-client"; "octez-baker"; "octez-dal-node"] ;

          (* Multi-progress callback *)
          let multi_progress
              (mp : Octez_manager_lib.Binary_downloader.multi_progress_state) =
            Context.multi_progress_update
              ~binary:mp.current_file
              ~downloaded:mp.downloaded
              ~total:mp.total
          in

          let result =
            Octez_manager_lib.Binary_downloader.download_version
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
              (match
                 res.Octez_manager_lib.Binary_downloader.checksum_status
               with
              | Octez_manager_lib.Binary_downloader.Verified ->
                  Context.multi_progress_checksum
                    "\xe2\x9c\x93 All checksums verified"
              | Octez_manager_lib.Binary_downloader.Skipped ->
                  Context.multi_progress_checksum
                    "\xe2\x9a\xa0 Checksum verification skipped"
              | Octez_manager_lib.Binary_downloader.Failed reason ->
                  Context.multi_progress_checksum
                    (Printf.sprintf "\xe2\x9c\x97 Failed: %s" reason)) ;
              Unix.sleepf 2.0 ;
              (* Linger to show final status *)
              Context.multi_progress_finish () ;
              on_complete true ;
              (* Close modal after successful download *)
              Miaou.Core.Modal_manager.close_top `Commit
          | Error (`Msg msg) ->
              Context.multi_progress_finish () ;
              Context.toast_error (Printf.sprintf "Download failed: %s" msg) ;
              on_complete false ;
              (* Close modal after failed download *)
              Miaou.Core.Modal_manager.close_top `Cancel) ;
      Navigation.make ()

    let update ps _ = ps

    let view _ps ~focus:_ ~size =
      (* Render multi-progress display (same as binaries page) *)
      let lines = ref [] in
      let add s = lines := s :: !lines in

      add (Printf.sprintf "Downloading Octez v%s..." version) ;
      add "" ;

      (* Add multi-progress display if active *)
      let multi_progress_lines =
        Context.render_multi_progress ~cols:(size.LTerm_geom.cols - 4)
      in
      if String.trim multi_progress_lines <> "" then add multi_progress_lines
      else add "Initializing download..." ;

      add "" ;
      add "Modal will close automatically when download completes." ;

      let content = String.concat "\n" (List.rev !lines) in
      let lines_list = String.split_on_char '\n' content in
      Pager.render
        ~win:(max 1 (size.LTerm_geom.rows - 4))
        ~cols:(max 1 (size.LTerm_geom.cols - 2))
        (Pager.open_lines ~title:"" lines_list)
        ~focus:false

    let move ps _ = ps

    let refresh ps = ps

    let service_select ps _ = ps

    let service_cycle ps _ = ps

    let keymap _ = []

    let back ps = ps

    let handled_keys _ = []

    let handle_modal_key ps _key ~size:_ = ps

    let handle_key ps _key ~size:_ = ps

    let has_modal _ = true
  end in
  let ui : Miaou.Core.Modal_manager.ui =
    {
      title = Printf.sprintf "Downloading v%s" version;
      left = None;
      max_width = Some (Fixed 76);
      dim_background = true;
    }
  in
  Miaou.Core.Modal_manager.push_default
    (module Modal)
    ~init:(Modal.init ())
    ~ui
    ~on_close:(fun _ _ -> ())

let select_app_bin_dir_modal ~on_select () =
  (* Load managed versions *)
  let managed_versions =
    match Octez_manager_lib.Binary_registry.list_managed_versions () with
    | Ok versions ->
        List.map
          (fun v -> `ManagedVersion v)
          (List.sort String.compare versions)
    | Error _ -> []
  in

  (* Load linked directories *)
  let linked_dirs =
    match Octez_manager_lib.Binary_registry.load_linked_dirs () with
    | Ok dirs ->
        List.map
          (fun (ld : Octez_manager_lib.Binary_registry.linked_dir) ->
            `LinkedDir (ld.alias, ld.path))
          dirs
    | Error _ -> []
  in

  (* Get latest uninstalled version to feature at the top *)
  let latest_uninstalled :
      Octez_manager_lib.Binary_downloader.version_info option =
    match Versions_scheduler.get_cached () with
    | None -> None
    | Some all_versions ->
        (* Get already installed versions *)
        let installed =
          match Octez_manager_lib.Binary_registry.list_managed_versions () with
          | Ok vers -> vers
          | Error _ -> []
        in
        (* Find highest installed version *)
        let highest_installed =
          List.fold_left
            (fun acc v ->
              match acc with
              | None -> Some v
              | Some prev ->
                  if
                    Octez_manager_lib.Version_checker.compare_versions v prev
                    > 0
                  then Some v
                  else Some prev)
            None
            installed
        in
        (* Sort available versions newest first *)
        let sorted_versions =
          List.sort
            (fun (a : Octez_manager_lib.Binary_downloader.version_info)
                 (b : Octez_manager_lib.Binary_downloader.version_info)
               ->
              (* Sort descending: newer versions first *)
              -Octez_manager_lib.Version_checker.compare_versions
                 a.version
                 b.version)
            all_versions
        in
        (* Find first version that is:
           1. Not already installed
           2. Newer than the highest installed version (if any) *)
        List.find_opt
          (fun (vi : Octez_manager_lib.Binary_downloader.version_info) ->
            let not_installed = not (List.mem vi.version installed) in
            let newer_than_installed =
              match highest_installed with
              | None -> true (* No installed versions, all are candidates *)
              | Some highest ->
                  Octez_manager_lib.Version_checker.compare_versions
                    vi.version
                    highest
                  > 0
            in
            not_installed && newer_than_installed)
          sorted_versions
  in

  (* Build sections with separators *)
  let items =
    if managed_versions = [] && linked_dirs = [] then
      (* No installed versions - show latest uninstalled if available *)
      match latest_uninstalled with
      | Some vi -> [`LatestVersion vi; `DownloadOther; `CustomPath]
      | None -> [`DownloadOther; `CustomPath]
    else
      (* Have installed versions - optionally show latest uninstalled at top *)
      match latest_uninstalled with
      | Some vi ->
          `LatestVersion vi
          :: (managed_versions @ linked_dirs @ [`DownloadOther; `CustomPath])
      | None -> managed_versions @ linked_dirs @ [`DownloadOther; `CustomPath]
  in

  let to_string :
      [ `ManagedVersion of string
      | `LinkedDir of string * string
      | `LatestVersion of Octez_manager_lib.Binary_downloader.version_info
      | `DownloadOther
      | `CustomPath ] ->
      string = function
    | `ManagedVersion v -> Printf.sprintf "v%s (managed)" v
    | `LinkedDir (alias, path) -> Printf.sprintf "%s  →  %s" alias path
    | `LatestVersion vi ->
        Printf.sprintf
          "Latest (v%s)  <download>"
          vi.Octez_manager_lib.Binary_downloader.version
    | `DownloadOther -> "[ Download other version... ]"
    | `CustomPath -> "[ Browse for custom directory... ]"
  in

  let on_choice_select = function
    | `ManagedVersion version ->
        let path =
          Octez_manager_lib.Binary_registry.managed_version_path version
        in
        on_select path
    | `LinkedDir (_alias, path) -> on_select path
    | `LatestVersion (vi : Octez_manager_lib.Binary_downloader.version_info) ->
        (* Directly download the latest version *)
        let version = vi.Octez_manager_lib.Binary_downloader.version in
        open_download_progress_modal ~version ~on_complete:(fun success ->
            if success then
              (* Auto-select the downloaded version *)
              let path =
                Octez_manager_lib.Binary_registry.managed_version_path version
              in
              on_select path)
    | `DownloadOther -> (
        (* Show available versions to download *)
        match Versions_scheduler.get_cached () with
        | None ->
            show_error
              ~title:"No Versions Available"
              "Could not load available versions. Try again later."
        | Some versions ->
            (* Filter to only show 2 latest major versions *)
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
              (fun (v : Octez_manager_lib.Binary_downloader.version_info) ->
                let major = extract_major v.version in
                let existing = Hashtbl.find_opt major_versions major in
                Hashtbl.replace
                  major_versions
                  major
                  (v :: Option.value ~default:[] existing))
              versions ;
            (* Get the 2 latest major versions *)
            let all_majors =
              Hashtbl.to_seq_keys major_versions
              |> List.of_seq |> List.sort compare
            in
            let latest_2_majors =
              List.rev all_majors |> fun l -> List.filteri (fun i _ -> i < 2) l
            in
            let filtered_versions =
              List.concat_map
                (fun major ->
                  Option.value
                    ~default:[]
                    (Hashtbl.find_opt major_versions major))
                latest_2_majors
            in
            (* Get already installed versions *)
            let installed =
              match
                Octez_manager_lib.Binary_registry.list_managed_versions ()
              with
              | Ok vers -> vers
              | Error _ -> []
            in
            (* Filter out already installed versions *)
            let version_items =
              List.filter
                (fun (vi : Octez_manager_lib.Binary_downloader.version_info) ->
                  not
                    (List.mem
                       vi.Octez_manager_lib.Binary_downloader.version
                       installed))
                filtered_versions
            in
            if version_items = [] then
              show_error
                ~title:"No Versions to Download"
                "All recent versions are already installed."
            else
              open_choice_modal
                ~title:"Select Version to Download"
                ~items:version_items
                ~to_string:(fun vi ->
                  Printf.sprintf
                    "v%s%s"
                    vi.Octez_manager_lib.Binary_downloader.version
                    (match
                       vi.Octez_manager_lib.Binary_downloader.release_date
                     with
                    | Some date -> Printf.sprintf "  (%s)" date
                    | None -> ""))
                ~on_select:(fun vi ->
                  let version =
                    vi.Octez_manager_lib.Binary_downloader.version
                  in
                  (* Open download progress modal *)
                  open_download_progress_modal
                    ~version
                    ~on_complete:(fun success ->
                      if success then
                        (* Auto-select the downloaded version *)
                        let path =
                          Octez_manager_lib.Binary_registry.managed_version_path
                            version
                        in
                        on_select path))
                ())
    | `CustomPath ->
        (* Open read-only file browser - no write permissions required *)
        open_file_browser_modal
          ~dirs_only:true
          ~require_writable:false
          ~on_select:(fun path ->
            let trimmed = String.trim path in

            (* Validation *)
            if trimmed = "" then
              show_error ~title:"Invalid Path" "Directory path cannot be empty"
            else if not (Sys.file_exists trimmed) then
              show_error
                ~title:"Directory Not Found"
                (Printf.sprintf "Directory does not exist: %s" trimmed)
            else if not (Sys.is_directory trimmed) then
              show_error
                ~title:"Invalid Path"
                "Path exists but is not a directory"
            else on_select trimmed)
          ()
  in

  open_choice_modal
    ~title:"Select Octez Binaries"
    ~items
    ~to_string
    ~on_select:on_choice_select
    ()

let show_help_modal () =
  let lines =
    [
      "Global shortcuts:";
      "  s  - Settings";
      "  m  - Menu";
      "  h/?- Help";
      "  Esc/q - Close modals";
      "";
      "Page-specific keys remain available when no modal is open.";
    ]
  in
  open_text_modal ~title:"Help" ~lines

let show_menu_modal () =
  let items =
    [
      ("Instances", "instances");
      ("Install node", "install_node_form_v3");
      ("Install baker", "install_baker_form_v3");
      ("Install accuser", "install_accuser_form_v3");
      ("Install DAL node", "install_dal_node_form_v3");
    ]
  in
  open_choice_modal
    ~title:"Menu"
    ~items
    ~to_string:(fun (label, _) -> label)
    ~on_select:(fun (_, target) -> Context.navigate target)
    ()
