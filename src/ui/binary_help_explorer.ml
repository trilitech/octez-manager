open Rresult
open Octez_manager_lib
module Keys = Miaou.Core.Keys
module Widgets = Miaou_widgets_display.Widgets
module Textbox_widget = Miaou_widgets_input.Textbox_widget

let ( let* ) = Result.bind

(* Structured representation of a CLI option extracted from --help output. *)
type value_kind =
  | Addr_port
  | Port
  | Addr
  | File
  | Dir
  | Path
  | Number
  | Float
  | Text

type arg_kind = Toggle | Value of value_kind

type option_entry = {
  names : string list;
  arg : string option;
  doc : string;
  kind : arg_kind;
}

(* Mutable selection entry used inside the modal. *)
type row = {
  opt : option_entry;
  mutable value : string option;
  mutable selected : bool;
}

let cache : (string, option_entry list) Hashtbl.t = Hashtbl.create 3

let set_help_hint ?short ?long () =
  Miaou.Core.Help_hint.clear () ;
  match (short, long) with
  | None, None -> ()
  | _ -> Miaou.Core.Help_hint.push ?short ?long ()

let primary_name names =
  match
    List.find_opt
      (fun n -> String.length n > 2 && String.sub n 0 2 = "--")
      names
  with
  | Some long -> long
  | None -> ( match names with h :: _ -> h | [] -> "")

let display_names names =
  let longs =
    List.filter (fun n -> String.length n > 2 && String.sub n 0 2 = "--") names
  in
  if longs = [] then names else longs

let trim_nonempty s =
  let t = String.trim s in
  if t = "" then None else Some t

let contains ~needle haystack =
  let nlen = String.length needle in
  let hlen = String.length haystack in
  let rec loop i =
    if i + nlen > hlen then false
    else if String.sub haystack i nlen = needle then true
    else loop (i + 1)
  in
  if nlen = 0 then true else loop 0

let classify_arg_kind ~names ~arg ~doc =
  match arg with
  | None -> Toggle
  | Some placeholder ->
      let text =
        String.concat
          " "
          [
            String.lowercase_ascii placeholder;
            String.lowercase_ascii doc;
            String.concat " " (List.map String.lowercase_ascii names);
          ]
      in
      let has s = contains ~needle:(String.lowercase_ascii s) text in
      if has "addr:port" || has "address:port" then Value Addr_port
      else if has "port" then Value Port
      else if has "addr" || has "address" || has "host" then Value Addr
      else if has "file" then Value File
      else if has "dir" || has "directory" then Value Dir
      else if has "path" then Value Path
      else if has "mode" then Value Text
      else if has "float" then Value Float
      else if has "int" || has "num" || has "number" then Value Number
      else Value Text

let split_spec_doc_default raw_line =
  let line = String.trim raw_line in
  let len = String.length line in
  let rec find_gap idx =
    if idx >= len - 1 then None
    else if line.[idx] = '\t' then Some idx
    else if line.[idx] = ' ' && line.[idx + 1] = ' ' then Some idx
    else find_gap (idx + 1)
  in
  match find_gap 0 with
  | Some idx ->
      let gap = if line.[idx] = '\t' then 1 else 2 in
      ( String.sub line 0 idx |> String.trim,
        String.sub line (idx + gap) (len - (idx + gap)) |> String.trim )
  | None -> (String.trim line, "")

(* Clap outputs often use ": " instead of aligned double spaces; fall back to
   splitting on the first occurrence of ": " when tabs/double-spaces are
   absent. *)
let split_spec_doc_baker raw_line =
  let line = String.trim raw_line in
  match split_spec_doc_default raw_line with
  | spec, doc when doc <> "" -> (spec, doc)
  | _ -> (
      match String.index_opt line ':' with
      | None -> (line, "")
      | Some idx ->
          if idx + 1 < String.length line && line.[idx + 1] = ' ' then
            let spec = String.sub line 0 idx |> String.trim in
            let doc =
              String.sub line (idx + 1) (String.length line - idx - 1)
              |> String.trim
            in
            (spec, doc)
          else (line, ""))

let clean_placeholder s =
  let trimmed = String.trim s in
  let len = String.length trimmed in
  if len = 0 then ""
  else
    let rec drop_trailing i =
      if i <= 0 then 0
      else
        match trimmed.[i - 1] with
        | ',' | ';' | ')' -> drop_trailing (i - 1)
        | _ -> i
    in
    let stop = drop_trailing len in
    String.sub trimmed 0 stop

let clean_name name =
  let trimmed = String.trim name in
  let len = String.length trimmed in
  let rec drop_trailing i =
    if i <= 0 then 0
    else
      match trimmed.[i - 1] with
      | ':' | ';' | ',' -> drop_trailing (i - 1)
      | _ -> i
  in
  String.sub trimmed 0 (drop_trailing len)

let parse_spec spec =
  let tokens =
    spec |> String.split_on_char ' '
    |> List.filter (fun s -> String.trim s <> "")
  in
  let names, arg_tokens =
    List.fold_left
      (fun (names, args) tok ->
        let tok = String.trim tok in
        if tok = "," then (names, args)
        else if String.length tok > 0 && tok.[0] = '-' then
          let name, inline_arg =
            match String.index_opt tok '=' with
            | None -> (tok, None)
            | Some idx ->
                let name = String.sub tok 0 idx in
                let placeholder =
                  String.sub tok (idx + 1) (String.length tok - idx - 1)
                in
                (name, Some placeholder)
          in
          ( clean_name name :: names,
            match inline_arg with None -> args | Some a -> a :: args )
        else (names, tok :: args))
      ([], [])
      tokens
  in
  let arg =
    match List.rev arg_tokens |> List.filter_map trim_nonempty with
    | [] -> None
    | x :: _ -> Some (clean_placeholder x)
  in
  (List.rev names, arg)

(* Check if a line looks like an option definition line from --help output.
   Option lines start with - and have at most one or two option names.
   Example/command lines often have multiple --flags and should be rejected. *)
let is_option_line_node line =
  let trimmed = String.trim line in
  if
    trimmed = ""
    || trimmed.[0] <> '-'
    || String.for_all (fun c -> c = '-') trimmed
  then false
  else
    (* Count occurrences of " --" which indicates multiple flags on one line
       (typical of example command lines, not option definitions) *)
    let double_dash_count =
      let rec count acc i =
        if i >= String.length trimmed - 2 then acc
        else if
          trimmed.[i] = ' ' && trimmed.[i + 1] = '-' && trimmed.[i + 2] = '-'
        then count (acc + 1) (i + 3)
        else count acc (i + 1)
      in
      count 0 0
    in
    (* Real option lines have at most one " --" (for alternate names like "-d, --data-dir")
       Example lines like "--data-dir /foo --rpc-addr bar" have multiple *)
    double_dash_count <= 1

let is_option_line_baker line =
  let trimmed = String.trim line in
  if
    trimmed = ""
    || trimmed.[0] <> '-'
    || String.for_all (fun c -> c = '-') trimmed
    || not (String.contains trimmed ' ')
  then false
  else
    (* Avoid example lines containing multiple flags. Clap-style lines usually
       present a single flag and its doc; we still allow alternate names like
       "-d, --data-dir". *)
    let double_dash_count =
      let rec count acc i =
        if i >= String.length trimmed - 2 then acc
        else if
          trimmed.[i] = ' ' && trimmed.[i + 1] = '-' && trimmed.[i + 2] = '-'
        then count (acc + 1) (i + 3)
        else count acc (i + 1)
      in
      count 0 0
    in
    double_dash_count <= 1

let strip_ansi s =
  let len = String.length s in
  let buf = Buffer.create len in
  let rec skip_escape i =
    if i >= len then len
    else
      match s.[i] with
      | 'A' .. 'Z' | 'a' .. 'z' -> i + 1
      | _ -> skip_escape (i + 1)
  in
  let rec loop i =
    if i >= len then ()
    else if s.[i] = '\027' then
      let next =
        if i + 1 < len && s.[i + 1] = '[' then skip_escape (i + 2)
        else skip_escape (i + 1)
      in
      loop next
    else (
      Buffer.add_char buf s.[i] ;
      loop (i + 1))
  in
  loop 0 ;
  Buffer.contents buf

let parse_help_with ~is_option_line ~split_spec_doc output =
  let lines = String.split_on_char '\n' output in
  let finalize current acc =
    match current with None -> acc | Some r -> r :: acc
  in
  (* Section headers that indicate we should stop parsing options *)
  let is_stop_section line =
    let trimmed = String.trim line in
    List.mem
      trimmed
      ["EXAMPLES"; "BUGS"; "SEE ALSO"; "EXIT STATUS"; "ENVIRONMENT"]
  in
  let rec loop acc current = function
    | [] -> List.rev (finalize current acc)
    | line :: _ when is_stop_section line ->
        (* Stop parsing when we hit these sections *)
        List.rev (finalize current acc)
    | line :: rest when is_option_line line ->
        let acc = finalize current acc in
        let spec, doc = split_spec_doc line in
        let names, arg = parse_spec spec in
        let kind = classify_arg_kind ~names ~arg ~doc in
        let entry = {names; arg; doc; kind} in
        loop acc (Some entry) rest
    | line :: rest ->
        let trimmed = String.trim line in
        if trimmed = "" then loop acc current rest
        else
          let current =
            match current with
            | None -> None
            | Some opt ->
                let doc =
                  if opt.doc = "" then trimmed else opt.doc ^ " " ^ trimmed
                in
                let kind =
                  classify_arg_kind ~names:opt.names ~arg:opt.arg ~doc
                in
                Some {opt with doc; kind}
          in
          loop acc current rest
  in
  loop [] None lines

let parse_help_node =
  parse_help_with
    ~is_option_line:is_option_line_node
    ~split_spec_doc:split_spec_doc_default

let parse_help_baker =
  parse_help_with
    ~is_option_line:is_option_line_baker
    ~split_spec_doc:split_spec_doc_baker

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
  match Hashtbl.find_opt cache binary with
  | Some opts -> Ok opts
  | None ->
      let* output = run_help binary in
      let output = strip_ansi output in
      let opts = parse_help_node output in
      if opts = [] then Error (`Msg "No options parsed from help output")
      else (
        Hashtbl.replace cache binary opts ;
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
      match r.value with None -> [flag] | Some v -> [flag ^ "=" ^ v])

let open_modal ~title ~options ~on_apply =
  let rows =
    Array.of_list
      (List.map (fun opt -> {opt; value = None; selected = false}) options)
  in
  let module Modal = struct
    type state = {
      rows : row array;
      mutable cursor : int;
      mutable scroll_offset : int;
      mutable last_key : string;
    }

    type msg = unit

    let update_help_hint s =
      match s.rows |> Array.to_list |> fun lst -> List.nth_opt lst s.cursor with
      | None -> set_help_hint ()
      | Some row ->
          let short, long = option_hint_markdown row in
          set_help_hint ?short ?long ()

    let init () = {rows; cursor = 0; scroll_offset = 0; last_key = ""}

    let update s _ = s

    let refresh s = s

    let adjust_scroll s ~visible_rows =
      if s.cursor < s.scroll_offset then s.scroll_offset <- s.cursor
      else if s.cursor > s.scroll_offset + visible_rows - 1 then
        s.scroll_offset <- s.cursor - visible_rows + 1

    let move s delta =
      let len = Array.length s.rows in
      let next = max 0 (min (len - 1) (s.cursor + delta)) in
      s.cursor <- next ;
      update_help_hint s ;
      s

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

    let open_value_modal row placeholder =
      let doc_lines = wrap_text ~width:68 row.opt.doc in
      let title = option_label row.opt in
      let initial_value = match row.value with Some v -> v | None -> "" in
      let module Modal = struct
        type state = {textbox : Textbox_widget.t; doc_lines : string list}

        type msg = unit

        let init () =
          {
            textbox =
              Textbox_widget.open_centered
                ~title:placeholder
                ~width:60
                ~initial:initial_value
                ();
            doc_lines;
          }

        let update s _ = s

        let view s ~focus ~size:_ =
          let doc_block = String.concat "\n" s.doc_lines in
          let input_block = Textbox_widget.render s.textbox ~focus in
          let hint = Widgets.dim "Enter: confirm · Esc: cancel" in
          doc_block ^ "\n\n" ^ input_block ^ "\n\n" ^ hint

        let move s _ = s

        let refresh s = s

        let enter s = s

        let service_select s _ = s

        let service_cycle s _ = s

        let back s = s

        let keymap _ = []

        let handled_keys () = []

        let handle_modal_key s key ~size:_ =
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
            s)
          else if mapped = "Esc" then (
            Miaou.Core.Modal_manager.set_consume_next_key () ;
            Miaou.Core.Modal_manager.close_top `Cancel ;
            s)
          else {s with textbox = Textbox_widget.handle_key s.textbox ~key}

        let handle_key = handle_modal_key

        let next_page _ = None

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

    let view s ~focus:_ ~size =
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
      let hint_line =
        match current_row s with
        | None -> Widgets.dim "Select an option to view details"
        | Some row -> truncate ~max_len:(max 20 (width - 10)) row.opt.doc
      in
      let footer =
        [
          Widgets.dim
            "↑/↓ move  Enter edit/toggle  Space toggle  s apply  ? hint  Esc \
             close";
          Widgets.dim (Printf.sprintf "Last key: %S | %s" s.last_key hint_line);
        ]
      in
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
        ~footer
        ~child:(fun _ -> body)

    let handle_modal_key s key ~size:_ =
      (* Store key for debug display *)
      s.last_key <- Printf.sprintf "%s(handler)" key ;
      (* Check raw key strings first (as sent by Miaou), then try Keys.of_string *)
      if key = "Enter" || key = "Return" then (
        edit_value s ;
        s)
      else if key = " " || key = "Space" then (
        (match current_row s with
        | None -> ()
        | Some row -> row.selected <- not row.selected) ;
        update_help_hint s ;
        s)
      else if key = "Backspace" || key = "\b" then (
        (match current_row s with
        | None -> ()
        | Some row ->
            row.selected <- false ;
            row.value <- None) ;
        update_help_hint s ;
        s)
      else if key = "s" || key = "S" then (
        apply_and_close s ;
        s)
      else if key = "?" then (
        show_hint s ;
        s)
      else if key = "Esc" || key = "Escape" || key = "q" || key = "Q" then (
        Miaou.Core.Modal_manager.close_top `Cancel ;
        s)
      else
        (* Fall back to Keys.of_string for special keys *)
        match Keys.of_string key with
        | Some Keys.Up -> move s (-1)
        | Some Keys.Down -> move s 1
        | Some (Keys.Char "k") -> move s (-1)
        | Some (Keys.Char "j") -> move s 1
        | Some Keys.Enter ->
            edit_value s ;
            s
        | Some Keys.Backspace ->
            (match current_row s with
            | None -> ()
            | Some row ->
                row.selected <- false ;
                row.value <- None) ;
            update_help_hint s ;
            s
        | _ -> s

    let handle_key = handle_modal_key

    let enter s = s

    let service_select s _ = s

    let service_cycle s _ = s

    let back s = s

    let handled_keys () =
      Miaou.Core.Keys.[Up; Down; Enter; Char " "; Backspace; Char "s"; Char "?"]

    let keymap _ =
      [
        ( "Up",
          (fun s ->
            s.last_key <- "Up(keymap)" ;
            move s (-1)),
          "Up" );
        ( "Down",
          (fun s ->
            s.last_key <- "Down(keymap)" ;
            move s 1),
          "Down" );
        ( "Enter",
          (fun s ->
            s.last_key <- "Enter(keymap)" ;
            edit_value s ;
            s),
          "Edit/Select" );
        ( "Return",
          (fun s ->
            s.last_key <- "Return(keymap)" ;
            edit_value s ;
            s),
          "Edit/Select" );
        ( "Space",
          (fun s ->
            s.last_key <- "Space(keymap)" ;
            (match current_row s with
            | None -> ()
            | Some row -> row.selected <- not row.selected) ;
            s),
          "Toggle" );
        ( "Backspace",
          (fun s ->
            s.last_key <- "Backspace(keymap)" ;
            (match current_row s with
            | None -> ()
            | Some row ->
                row.selected <- false ;
                row.value <- None) ;
            s),
          "Clear" );
        ( "s",
          (fun s ->
            s.last_key <- "s(keymap)" ;
            apply_and_close s ;
            s),
          "Apply" );
        ( "?",
          (fun s ->
            s.last_key <- "?(keymap)" ;
            show_hint s ;
            s),
          "Help" );
      ]

    let next_page _ = None

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
    ~on_close:(fun state _reason ->
      (* Apply current selections even when closing with Esc to keep extra args in sync. *)
      let tokens = state.rows |> Array.to_list |> format_tokens in
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

let open_node_run_help ~app_bin_dir ~on_apply =
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
        open_modal ~title:"Node Flags" ~options:filtered ~on_apply
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
  match Hashtbl.find_opt cache cache_key with
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
      Hashtbl.replace cache cache_key opts ;
      Ok opts

let open_baker_run_help ~app_bin_dir ~mode ~on_apply =
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
        open_modal ~title ~options:filtered ~on_apply
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
end
