module Pager = Miaou_widgets_display.Pager_widget
module Select_widget = Miaou_widgets_input.Select_widget
module Textbox_widget = Miaou_widgets_input.Textbox_widget

let open_text_modal ~title ~lines =
  let module Modal = struct
    type state = Pager.t

    type msg = unit

    let init () = Pager.open_lines ~title lines

    let update s _ = s

    let view s ~focus ~size =
      let rows = max 1 (size.LTerm_geom.rows - 4) in
      let cols = max 1 (size.LTerm_geom.cols - 4) in
      Pager.render ~win:rows ~cols s ~focus

    let move s _ = s

    let refresh s = s

    let enter s = s

    let service_select s _ = s

    let service_cycle s _ = s

    let back s = s

    let keymap _ = []

    let handle_modal_key s key ~size =
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
      if key = "Esc" then (
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
    {title; left = None; max_width = None; dim_background = true}
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

let show_success ~title message =
  open_text_modal ~title ~lines:["Success"; ""; message]

let show_error ~title message =
  open_text_modal ~title ~lines:["Error"; ""; message]
