module Pager = Miaou_widgets_display.Pager_widget
module Select_widget = Miaou_widgets_input.Select_widget
module Textbox_widget = Miaou_widgets_input.Textbox_widget
module Widgets = Miaou_widgets_display.Widgets

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

    let init () = Pager.open_lines ~title:"" lines

    let update s _ = s

    let view s ~focus ~size =
      let rows = max 1 (size.LTerm_geom.rows - 4) in
      (* Clamp columns so rendered content never exceeds the modal's inner width *)
      let cols =
        let inner = max 1 (size.LTerm_geom.cols - 2) in
        min inner 72
      in
      Pager.render ~win:rows ~cols s ~focus

    let move s _ = s

    let refresh s = s

    let enter s = s

    let service_select s _ = s

    let service_cycle s _ = s

    let back s = s

    let keymap _ = []

    let handled_keys () = []

    let handle_modal_key s key ~size =
      (* Check if pager is in input mode (search/lookup) *)
      let pager_in_input_mode =
        match s.Pager.input_mode with
        | `Search_edit | `Lookup -> true
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
      (* Only close modal on Esc/q when NOT in search/input mode *)
      if key = "Esc" && not pager_in_input_mode then (
        Miaou.Core.Modal_manager.close_top `Cancel ;
        s)
      else
        let rows = max 1 (size.LTerm_geom.rows - 4) in
        let win = rows in
        let pager, _ = Pager.handle_key ~win s ~key in
        pager

    let handle_key = handle_modal_key

    let next_page _ = None

    let has_modal _ = true
  end in
  let ui : Miaou.Core.Modal_manager.ui =
    (* Limit modal width so header/separator stay on a single line *)
    {title; left = None; max_width = Some 76; dim_background = true}
  in
  Miaou.Core.Modal_manager.push_default
    (module Modal)
    ~init:(Modal.init ())
    ~ui
    ~on_close:(fun _ _ -> ())

let open_choice_modal (type choice) ~title ~(items : choice list) ~to_string
    ~on_select =
  let module Modal = struct
    type state = choice Select_widget.t

    type msg = unit

    let init () = failwith "choice modal init provided by caller"

    let update s _ = s

    let view s ~focus ~size = Select_widget.render_with_size s ~focus ~size

    let move s _ = s

    let refresh s = s

    let enter s = s

    let service_select s _ = s

    let service_cycle s _ = s

    let back s = s

    let keymap _ = []

    let handled_keys () = []

    let handle_modal_key s key ~size:_ =
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
      if key = "Enter" then (
        Miaou.Core.Modal_manager.close_top `Commit ;
        s)
      else if key = "Esc" then (
        Miaou.Core.Modal_manager.close_top `Cancel ;
        s)
      else Select_widget.handle_key s ~key

    let handle_key = handle_modal_key

    let next_page _ = None

    let has_modal _ = true
  end in
  let widget = Select_widget.open_centered ~title ~items ~to_string () in
  let ui : Miaou.Core.Modal_manager.ui =
    {title; left = None; max_width = Some 80; dim_background = true}
  in
  Miaou.Core.Modal_manager.push_default
    (module Modal)
    ~init:widget
    ~ui
    ~on_close:(fun state -> function
      | `Commit -> (
          match Select_widget.get_selection state with
          | Some choice -> on_select choice
          | None -> ())
      | `Cancel -> ())

let open_choice_modal_with_hint (type choice) ~title ~(items : choice list)
    ~to_string ~hint ~describe ~on_select =
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

    let init () = failwith "choice modal init provided by caller"

    let update s _ = s

    let view s ~focus ~size =
      (* Update Help_hint whenever rendering so ? shows current selection's doc *)
      update_help_hint s ;
      Select_widget.render_with_size s ~focus ~size

    let move s _ = s

    let refresh s = s

    let enter s = s

    let service_select s _ = s

    let service_cycle s _ = s

    let back s = s

    let keymap s =
      let doc_lines =
        match Select_widget.get_selection s with
        | Some choice -> describe_fn choice
        | None -> ( match items with hd :: _ -> describe_fn hd | [] -> [])
      in
      let doc_entries =
        doc_lines
        |> List.filter (fun l -> String.trim l <> "")
        |> List.mapi (fun idx line ->
            (Printf.sprintf "doc%d" (idx + 1), (fun s -> s), line))
      in
      (* Keep keymap mostly for displaying help; handlers are no-op to avoid
         interfering with handle_modal_key. *)
      [
        ("Enter", (fun s -> s), "Select");
        ("Up", (fun s -> s), "Up");
        ("Down", (fun s -> s), "Down");
        ("PageUp", (fun s -> s), "Page up");
        ("PageDown", (fun s -> s), "Page down");
        ("Home", (fun s -> s), "Top");
        ("End", (fun s -> s), "Bottom");
        ("?", (fun s -> s), "Show description");
      ]
      @ doc_entries

    let handled_keys () = []

    let handle_modal_key s key ~size:_ =
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
        s)
      else if key = "Hint" then (
        (match Select_widget.get_selection s with
        | Some choice -> hint choice
        | None -> ( match items with hd :: _ -> hint hd | [] -> ())) ;
        s)
      else if key = "Esc" then (
        Miaou.Core.Modal_manager.set_consume_next_key () ;
        Miaou.Core.Modal_manager.close_top `Cancel ;
        s)
      else Select_widget.handle_key s ~key

    let handle_key = handle_modal_key

    let next_page _ = None

    let has_modal _ = true
  end in
  let widget = Select_widget.open_centered ~title ~items ~to_string () in
  (* Set initial Help_hint for the default selection *)
  update_help_hint widget ;
  let ui : Miaou.Core.Modal_manager.ui =
    {title; left = None; max_width = Some 80; dim_background = true}
  in
  (* Use push with empty commit_on/cancel_on since we handle Enter/Esc manually
     in handle_modal_key. This prevents double-close when used as nested modal. *)
  Miaou.Core.Modal_manager.push
    (module Modal)
    ~init:widget
    ~ui
    ~commit_on:[]
    ~cancel_on:[]
    ~on_close:(fun state -> function
      | `Commit -> (
          Miaou.Core.Help_hint.clear () ;
          match Select_widget.get_selection state with
          | Some choice -> on_select choice
          | None -> ())
      | `Cancel -> Miaou.Core.Help_hint.clear ())

let prompt_text_modal ?title ?(width = 60) ?initial ?placeholder ~on_submit () =
  let module Modal = struct
    type state = Textbox_widget.t

    type msg = unit

    let init () = failwith "textbox modal init provided by caller"

    let update s _ = s

    let view s ~focus ~size:_ = Textbox_widget.render s ~focus

    let move s _ = s

    let refresh s = s

    let enter s = s

    let service_select s _ = s

    let service_cycle s _ = s

    let back s = s

    let keymap _ = []

    let handled_keys () = []

    let handle_modal_key s key ~size:_ =
      if key = "Enter" then (
        Miaou.Core.Modal_manager.close_top `Commit ;
        s)
      else if key = "Esc" || key = "Escape" then (
        Miaou.Core.Modal_manager.close_top `Cancel ;
        s)
      else Textbox_widget.handle_key s ~key

    let handle_key = handle_modal_key

    let next_page _ = None

    let has_modal _ = true
  end in
  let widget =
    Textbox_widget.open_centered ?title ~width ?initial ?placeholder ()
  in
  let modal_title = Option.value ~default:"Input" title in
  Miaou.Core.Modal_manager.prompt
    (module Modal)
    ~init:widget
    ~title:modal_title
    ~extract:(fun state -> Some (Textbox_widget.get_text state))
    ~on_result:(function Some text -> on_submit text | None -> ())
    ()

let open_multiselect_modal (type choice) ~title ~(items : unit -> choice list)
    ~to_string ~on_select =
  let module Modal = struct
    type state = choice Select_widget.t

    type msg = unit

    let init () = failwith "multiselect modal init provided by caller"

    let update s _ = s

    let view s ~focus ~size = Select_widget.render_with_size s ~focus ~size

    let move s _ = s

    let refresh s = s

    let enter s = s

    let service_select s _ = s

    let service_cycle s _ = s

    let back s = s

    let keymap _ = []

    let handled_keys () = []

    let handle_modal_key s key ~size:_ =
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
      if key = "Enter" then (
        match Select_widget.get_selection s with
        | Some choice -> (
            match on_select choice with
            | `KeepOpen ->
                (* Rebuild widget with updated items from the callback *)
                let updated_items = items () in
                Select_widget.open_centered ~title ~items:updated_items ~to_string ()
            | `Close ->
                (* Close modal *)
                Miaou.Core.Modal_manager.close_top `Commit ;
                s)
        | None -> s)
      else if key = "Esc" then (
        Miaou.Core.Modal_manager.close_top `Cancel ;
        s)
      else Select_widget.handle_key s ~key

    let handle_key = handle_modal_key

    let next_page _ = None

    let has_modal _ = true
  end in
  let widget = Select_widget.open_centered ~title ~items:(items ()) ~to_string () in
  let ui : Miaou.Core.Modal_manager.ui =
    {title; left = None; max_width = Some 80; dim_background = true}
  in
  (* Use push with empty commit_on/cancel_on since we handle Enter/Esc manually
     in handle_modal_key. This prevents the modal from auto-closing on Enter. *)
  Miaou.Core.Modal_manager.push
    (module Modal)
    ~init:widget
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

let prompt_validated_text_modal ?title ?(width = 60) ?initial ?placeholder
    ~validator ~on_submit () =
  let module Modal = struct
    type state = unit Miaou_widgets_input.Validated_textbox_widget.t

    type msg = unit

    let init () = failwith "validated textbox modal init provided by caller"

    let update s _ = s

    let view s ~focus ~size:_ =
      Miaou_widgets_input.Validated_textbox_widget.render s ~focus

    let move s _ = s

    let refresh s = s

    let enter s = s

    let service_select s _ = s

    let service_cycle s _ = s

    let back s = s

    let keymap _ = []

    let handled_keys () = []

    let handle_modal_key s key ~size:_ =
      if key = "Enter" then
        if Miaou_widgets_input.Validated_textbox_widget.is_valid s then (
          Miaou.Core.Modal_manager.close_top `Commit ;
          s)
        else s
      else if key = "Esc" || key = "Escape" then (
        Miaou.Core.Modal_manager.close_top `Cancel ;
        s)
      else Miaou_widgets_input.Validated_textbox_widget.handle_key s ~key

    let handle_key = handle_modal_key

    let next_page _ = None

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
    ~init:widget
    ~title:modal_title
    ~extract:(fun state ->
      Some (Miaou_widgets_input.Validated_textbox_widget.value state))
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

let open_file_browser_modal ?initial_path ~dirs_only ~require_writable ~on_select () =
  let module File_browser = Miaou_widgets_layout.File_browser_widget in
  let module Modal = struct
    type state = File_browser.t

    type msg = unit

    let init () =
      File_browser.open_centered
        ?path:initial_path
        ~dirs_only
        ~require_writable
        ()

    let update s _ = s

    let view s ~focus ~size = File_browser.render_with_size s ~focus ~size

    let move s _ = s

    let refresh s = s

    let enter s = s

    let service_select s _ = s

    let service_cycle s _ = s

    let back s = s

    let keymap _ = []

    let handled_keys () = []

    let handle_modal_key s key ~size:_ =
      (* Let browser handle the key first and get updated state *)
      let browser' = File_browser.handle_key s ~key in
      (* Apply any pending updates from modal callbacks *)
      let browser'' = File_browser.apply_pending_updates browser' in

      (* Check for cancellation after browser processes key *)
      if File_browser.is_cancelled browser'' then (
        Miaou.Core.Modal_manager.close_top `Cancel ;
        browser'')
      (* Check for commit with 's' key (Enter is used for navigation) *)
      else if key = "s" && File_browser.can_commit browser'' then (
        Miaou.Core.Modal_manager.close_top `Commit ;
        browser'')
      (* Return the updated browser state *)
      else browser''

    let handle_key = handle_modal_key

    let next_page _ = None

    let has_modal _ = true
  end in
  let ui : Miaou.Core.Modal_manager.ui =
    {title = "Browse Directory (press 's' to select, Esc to cancel)";
     left = None;
     max_width = Some 100;
     dim_background = true}
  in
  (* Use push with empty commit_on/cancel_on since we handle keys manually *)
  Miaou.Core.Modal_manager.push
    (module Modal)
    ~init:(Modal.init ())
    ~ui
    ~commit_on:[]
    ~cancel_on:[]
    ~on_close:(fun state -> function
      | `Commit -> (
          match Miaou_widgets_layout.File_browser_widget.get_selection state with
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
    | Existing_dir entry -> on_select entry.Octez_manager_lib.Directory_registry.path
    | Browse_new_dir ->
        (* Open file browser *)
        open_file_browser_modal
          ~dirs_only:true
          ~require_writable:true
          ~on_select:(fun path ->
            let trimmed = String.trim path in

            (* Validation *)
            if trimmed = "" then
              show_error ~title:"Invalid Path" "Directory path cannot be empty"
            else if Sys.file_exists trimmed && not (Sys.is_directory trimmed)
            then
              show_error ~title:"Invalid Path"
                "Path exists but is not a directory"
            else
              (* Create directory with error handling *)
              let user, group = Octez_manager_lib.Common.current_user_group_names () in
              match
                Octez_manager_lib.Common.ensure_dir_path ~owner:user ~group ~mode:0o755 trimmed
              with
              | Ok () ->
                  (* Verify it was created *)
                  if not (Sys.file_exists trimmed) then
                    show_error ~title:"Creation Failed"
                      "Directory creation succeeded but path doesn't exist"
                  else (
                    (* Add to registry (ignore errors but log) *)
                    (match
                       Octez_manager_lib.Directory_registry.add ~path:trimmed ~dir_type
                         ~linked_services:[]
                     with
                    | Ok () -> ()
                    | Error (`Msg msg) ->
                        prerr_endline ("Registry add failed: " ^ msg)) ;
                    on_select trimmed)
              | Error (`Msg msg) ->
                  show_error ~title:"Directory Creation Failed" msg)
          ()
  in

  open_choice_modal ~title ~items ~to_string ~on_select:on_choice_select

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

let select_app_bin_dir_modal ~on_select () =
  (* Load existing app bin directories from registry *)
  let existing_dirs =
    match
      Octez_manager_lib.Directory_registry.list
        ~dir_type:Octez_manager_lib.Directory_registry.App_bin_dir
        ()
    with
    | Ok entries -> entries
    | Error _ -> []
  in

  (* Build choice items: existing dirs + browse option *)
  let items =
    List.map (fun e -> Existing_dir e) existing_dirs @ [Browse_new_dir]
  in

  let to_string = function
    | Existing_dir entry ->
        let services_str =
          match entry.Octez_manager_lib.Directory_registry.linked_services with
          | [] -> ""
          | svcs -> "  (" ^ String.concat ", " svcs ^ ")"
        in
        entry.Octez_manager_lib.Directory_registry.path ^ services_str
    | Browse_new_dir -> "[ Browse for existing directory... ]"
  in

  let on_choice_select = function
    | Existing_dir entry ->
        on_select entry.Octez_manager_lib.Directory_registry.path
    | Browse_new_dir ->
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
              show_error ~title:"Directory Not Found"
                (Printf.sprintf "Directory does not exist: %s" trimmed)
            else if not (Sys.is_directory trimmed) then
              show_error ~title:"Invalid Path" "Path exists but is not a directory"
            else
              (* Register in directory registry without creating *)
              (match
                 Octez_manager_lib.Directory_registry.add
                   ~path:trimmed
                   ~dir_type:Octez_manager_lib.Directory_registry.App_bin_dir
                   ~linked_services:[]
               with
              | Ok () -> on_select trimmed
              | Error (`Msg _) ->
                  (* If registry update fails, still allow selection *)
                  on_select trimmed))
          ()
  in

  open_choice_modal
    ~title:"Select Application Binary Directory"
    ~items
    ~to_string
    ~on_select:on_choice_select

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
      ("Install baker", "install_baker_form");
    ]
  in
  open_choice_modal
    ~title:"Menu"
    ~items
    ~to_string:(fun (label, _) -> label)
    ~on_select:(fun (_, target) -> Context.navigate target)
