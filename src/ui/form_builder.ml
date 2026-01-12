(*****************************************************************************)
(*                                                                           *)
(* SPDX-License-Identifier: MIT                                              *)
(* Copyright (c) 2025 Nomadic Labs <contact@nomadic-labs.com>                *)
(*                                                                           *)
(*****************************************************************************)

module Widgets = Miaou_widgets_display.Widgets
module Table_widget = Miaou_widgets_display.Table_widget

(*****************************************************************************)
(*                             TYPE DEFINITIONS                              *)
(*****************************************************************************)

(** Internal field representation with existential type for value. *)
type ('model, 'value) field_internal = {
  label : string;
  hint : string option;  (** Help text describing the field's purpose *)
  get : 'model -> 'value;
  set : 'value -> 'model -> 'model; [@warning "-69"]
  to_string : 'value -> string;
  validate : 'model -> bool;
  validate_msg : 'model -> string option;
  edit : 'model ref -> unit;
}

(** Existentially wrapped field (hides the 'value type parameter). *)
type 'model field = Field : ('model, 'value) field_internal -> 'model field

(** Add a hint to a field. *)
let with_hint hint (Field f) = Field {f with hint = Some hint}

type 'model pre_submit_modal_config =
  | PreSubmitModal : {
      title : string;
      message : string option;
      choices : 'choice list;
      to_string : 'choice -> string;
      on_choice : 'choice -> 'model -> 'model;
    }
      -> 'model pre_submit_modal_config

type 'model spec = {
  title : string;
  initial_model : unit -> 'model;  (** Called to get fresh initial values *)
  fields : 'model -> 'model field list;
      (** Dynamic field generation - called with current model to allow
          conditional fields (e.g., read-only in edit mode) *)
  on_init : ('model -> unit) option;
  on_refresh : ('model -> unit) option;
  pre_submit :
    ('model ->
    (unit, [`Msg of string | `Modal of string * (unit -> unit)]) result)
    option;
  pre_submit_modal : ('model -> 'model pre_submit_modal_config option) option;
  on_submit : 'model -> (unit, [`Msg of string]) result;
}

(*****************************************************************************)
(*                                FIELD HELPERS                              *)
(*****************************************************************************)

let text ~label ~get ~set =
  let to_string v = v in
  let validate _ = true in
  let validate_msg _ = None in
  let edit model_ref =
    let initial = get !model_ref in
    let on_submit v = model_ref := set v !model_ref in
    Modal_helpers.prompt_text_modal ~title:label ~initial ~on_submit ()
  in
  Field {label; hint = None; get; set; to_string; validate; validate_msg; edit}

let validated_text ~label ~get ~set ~validate =
  let to_string v = v in
  let validate_fn model =
    match validate model with Ok () -> true | Error _ -> false
  in
  let validate_msg model =
    match validate model with Ok () -> None | Error msg -> Some msg
  in
  let edit model_ref =
    let initial = get !model_ref in
    let on_submit v = model_ref := set v !model_ref in
    let validator v =
      let temp_model = set v !model_ref in
      validate temp_model
    in
    Modal_helpers.prompt_validated_text_modal
      ~title:label
      ~initial
      ~validator
      ~on_submit
      ()
  in
  Field
    {
      label;
      hint = None;
      get;
      set;
      to_string;
      validate = validate_fn;
      validate_msg;
      edit;
    }

let toggle ~label ~get ~set =
  let to_string = string_of_bool in
  let validate _ = true in
  let validate_msg _ = None in
  let edit model_ref =
    let current = get !model_ref in
    model_ref := set (not current) !model_ref
  in
  Field {label; hint = None; get; set; to_string; validate; validate_msg; edit}

let choice ~label ~get ~set ~to_string ~items =
  let validate _ = true in
  let validate_msg _ = None in
  let edit model_ref =
    let on_select v = model_ref := set v !model_ref in
    Modal_helpers.open_choice_modal ~title:label ~items ~to_string ~on_select
  in
  Field {label; hint = None; get; set; to_string; validate; validate_msg; edit}

let dynamic_choice ~label ~get ~set ~to_string ~get_items =
  let validate _ = true in
  let validate_msg _ = None in
  let edit model_ref =
    let items = get_items !model_ref in
    let on_select v = model_ref := set v !model_ref in
    Modal_helpers.open_choice_modal ~title:label ~items ~to_string ~on_select
  in
  Field {label; hint = None; get; set; to_string; validate; validate_msg; edit}

let readonly ~label ~get =
  let set _ m = m in
  let to_string v = v in
  let validate _ = true in
  let validate_msg _ = None in
  let edit _ = () in
  Field {label; hint = None; get; set; to_string; validate; validate_msg; edit}

let custom ~label ~get ~edit ?(validate = fun _ -> true)
    ?(validate_msg = fun _ -> None) () =
  let set _ m = m in
  let to_string v = v in
  Field {label; hint = None; get; set; to_string; validate; validate_msg; edit}

let node_data_dir ~label ~get ~set ?(validate = fun _ -> true) () =
  let to_string v = v in
  let validate_msg _ = None in
  let edit model_ref =
    let on_select path = model_ref := set path !model_ref in
    Modal_helpers.select_node_data_dir_modal ~on_select ()
  in
  Field {label; hint = None; get; set; to_string; validate; validate_msg; edit}

let client_base_dir ~label ~get ~set ?(validate = fun _ -> true) () =
  let to_string v = v in
  let validate_msg _ = None in
  let edit model_ref =
    let on_select path = model_ref := set path !model_ref in
    Modal_helpers.select_client_base_dir_modal ~on_select ()
  in
  Field {label; hint = None; get; set; to_string; validate; validate_msg; edit}

let app_bin_dir ~label ~get ~set ?(validate = fun _ -> true) () =
  let to_string v = v in
  let validate_msg _ = None in
  let edit model_ref =
    let on_select path = model_ref := set path !model_ref in
    Modal_helpers.select_app_bin_dir_modal ~on_select ()
  in
  Field {label; hint = None; get; set; to_string; validate; validate_msg; edit}

let extra_args ?baker_mode ~label ~get_args ~set_args ~get_bin_dir ~binary
    ?subcommand () =
  let to_string args = if args = "" then "(none)" else args in
  let validate _ = true in
  let validate_msg _ = None in
  let get = get_args in
  let set args m = set_args args m in
  let edit model_ref =
    let app_bin_dir = get_bin_dir !model_ref in
    let initial_args = get_args !model_ref in
    let on_apply tokens =
      let arg_str = String.concat " " tokens in
      model_ref := set_args arg_str !model_ref
    in
    (* Call appropriate help function based on binary and subcommand *)
    match (binary, subcommand) with
    | "octez-node", _ ->
        Binary_help_explorer.open_node_run_help
          ~app_bin_dir
          ~initial_args
          ~on_apply
    | "octez-baker", _ ->
        let mode =
          match baker_mode with None -> `Local | Some f -> f !model_ref
        in
        Binary_help_explorer.open_baker_run_help
          ~app_bin_dir
          ~mode
          ~initial_args
          ~on_apply
    | _ ->
        (* Generic fallback - show error for now *)
        Modal_helpers.show_error
          ~title:"Extra Args"
          (Printf.sprintf "Binary help not available for %s" binary)
  in
  Field {label; hint = None; get; set; to_string; validate; validate_msg; edit}

(** Helper to parse host:port *)
let parse_host_port s =
  match String.split_on_char ':' s with
  | [host; port] -> (
      try
        let p = int_of_string (String.trim port) in
        if p > 0 && p < 65536 && String.trim host <> "" then Ok ()
        else Error "Port must be between 1 and 65535"
      with _ -> Error "Invalid port number")
  | _ -> Error "Format must be host:port (e.g., 127.0.0.1:8732)"

let endpoint ~label ~get ~set ?default_port:(_ = 8732) () =
  let to_string v = v in
  let validate model =
    let v = get model in
    if String.trim v = "" then true (* Allow empty *)
    else match parse_host_port v with Ok () -> true | Error _ -> false
  in
  let validate_msg model =
    let v = get model in
    if String.trim v = "" then None
    else match parse_host_port v with Ok () -> None | Error msg -> Some msg
  in
  let edit model_ref =
    let initial = get !model_ref in
    let validator v = if String.trim v = "" then Ok () else parse_host_port v in
    Modal_helpers.prompt_validated_text_modal
      ~title:label
      ~initial
      ~validator
      ~on_submit:(fun v -> model_ref := set v !model_ref)
      ()
  in
  Field {label; hint = None; get; set; to_string; validate; validate_msg; edit}

let service_or_endpoint ~label ~role ~get ~set
    ?(external_label = "External endpoint...")
    ?(endpoint_validator = parse_host_port) () =
  let to_string = function
    | `None -> "(none)"
    | `Service inst -> inst
    | `Endpoint ep -> ep
  in
  let validate model =
    match get model with
    | `None -> true
    | `Service _ -> true
    | `Endpoint ep -> (
        match endpoint_validator ep with Ok () -> true | Error _ -> false)
  in
  let validate_msg model =
    match get model with
    | `Endpoint ep -> (
        match endpoint_validator ep with Ok () -> None | Error msg -> Some msg)
    | _ -> None
  in
  let edit model_ref =
    (* Get list of services with matching role *)
    let open Octez_manager_lib in
    let services = Form_builder_common.cached_service_states () in
    let matching_services =
      services
      |> List.filter (fun s -> s.Data.Service_state.service.Service.role = role)
    in
    let service_items = List.map (fun s -> `Service s) matching_services in
    let items = [`None] @ service_items @ [`External] in

    let to_string_item = function
      | `None -> "None"
      | `Service s ->
          let svc = s.Data.Service_state.service in
          Printf.sprintf "%s (%s)" svc.Service.instance svc.Service.network
      | `External -> external_label
    in

    let on_select = function
      | `None -> model_ref := set `None !model_ref
      | `Service s ->
          let inst = s.Data.Service_state.service.Service.instance in
          model_ref := set (`Service inst) !model_ref
      | `External ->
          let initial =
            match get !model_ref with `Endpoint ep -> ep | _ -> ""
          in
          Modal_helpers.prompt_validated_text_modal
            ~title:label
            ~initial
            ~validator:endpoint_validator
            ~on_submit:(fun ep -> model_ref := set (`Endpoint ep) !model_ref)
            ()
    in
    Modal_helpers.open_choice_modal
      ~title:label
      ~items
      ~to_string:to_string_item
      ~on_select
  in
  Field {label; hint = None; get; set; to_string; validate; validate_msg; edit}

let string_list ~label ~get ~set ?(get_suggestions = fun _ -> [])
    ?(item_validator = fun _ -> Ok ()) () =
  let to_string lst = if lst = [] then "(none)" else String.concat ", " lst in
  let validate _ = true in
  let validate_msg _ = None in
  let edit model_ref =
    let build_items () =
      let current = get !model_ref in
      let suggestions = get_suggestions !model_ref in
      (* Available suggestions that can be toggled *)
      let suggestion_items = List.map (fun s -> `Toggle s) suggestions in
      (* Current items that can be removed *)
      let remove_items = List.map (fun item -> `Remove item) current in
      let clear_item = if current = [] then [] else [`Clear] in
      suggestion_items @ [`Add] @ clear_item @ remove_items
    in

    let to_string_item = function
      | `Toggle item ->
          let current = get !model_ref in
          let checked = List.mem item current in
          let checkbox = if checked then "[x]" else "[ ]" in
          Printf.sprintf "%s %s" checkbox item
      | `Add -> "Add item (manual)"
      | `Clear -> "Clear all"
      | `Remove item -> "Remove: " ^ item
    in

    let on_select = function
      | `Toggle item ->
          let current = get !model_ref in
          let updated =
            if List.mem item current then
              List.filter (fun x -> x <> item) current
            else current @ [item]
          in
          model_ref := set updated !model_ref ;
          `KeepOpen
      | `Add ->
          Modal_helpers.prompt_validated_text_modal
            ~title:("Add " ^ label)
            ~validator:(fun v ->
              if String.trim v = "" then Error "Cannot be empty"
              else item_validator v)
            ~on_submit:(fun v ->
              let v = String.trim v in
              let current = get !model_ref in
              if not (List.mem v current) then
                model_ref := set (current @ [v]) !model_ref)
            () ;
          `KeepOpen
      | `Clear ->
          model_ref := set [] !model_ref ;
          `KeepOpen
      | `Remove item ->
          let current = get !model_ref in
          model_ref := set (List.filter (fun x -> x <> item) current) !model_ref ;
          `KeepOpen
    in
    Modal_helpers.open_multiselect_modal
      ~title:label
      ~items:build_items
      ~to_string:to_string_item
      ~on_select
  in
  Field {label; hint = None; get; set; to_string; validate; validate_msg; edit}

(*****************************************************************************)
(*                                  FUNCTOR                                  *)
(*****************************************************************************)

module Navigation = Miaou.Core.Navigation

module Make (S : sig
  type model

  val spec : model spec
end) =
struct
  type state = {model_ref : S.model ref; cursor : int}

  type msg = unit

  type pstate = state Navigation.t

  let init () =
    let s = {model_ref = ref (S.spec.initial_model ()); cursor = 0} in
    (* Call on_init hook if provided *)
    (match S.spec.on_init with Some f -> f !(s.model_ref) | None -> ()) ;
    Navigation.make s

  let update ps _ = ps

  let refresh ps =
    (* Check for pending navigation from Context (set by modal callbacks) *)
    let ps =
      match Context.consume_navigation () with
      | Some p -> Navigation.goto p ps
      | None -> ps
    in
    (* Call on_refresh hook if provided *)
    let s = ps.Navigation.s in
    (match S.spec.on_refresh with Some f -> f !(s.model_ref) | None -> ()) ;
    ps

  let move_state s delta =
    let fields = S.spec.fields !(s.model_ref) in
    let max_cursor = List.length fields in
    let cursor = max 0 (min max_cursor (s.cursor + delta)) in
    {s with cursor}

  let rec submit s =
    let model = !(s.model_ref) in
    let fields = S.spec.fields model in
    (* Check all validations before submitting *)
    let validation_errors =
      List.filter_map
        (fun (Field f) ->
          if f.validate model then None else Some (f.label, f.validate_msg model))
        fields
    in
    match validation_errors with
    | (label, Some msg) :: _ ->
        Modal_helpers.show_error
          ~title:"Validation Failed"
          (Printf.sprintf "%s: %s" label msg) ;
        s
    | (label, None) :: _ ->
        Modal_helpers.show_error
          ~title:"Validation Failed"
          (Printf.sprintf "%s is invalid" label) ;
        s
    | [] -> (
        (* Check if we need to show a pre-submission modal *)
        match S.spec.pre_submit_modal with
        | Some modal_fn -> (
            match modal_fn model with
            | Some (PreSubmitModal modal_config) ->
                (* Show choice modal, update model, and auto-submit on selection *)
                let on_select choice =
                  let updated_model =
                    modal_config.on_choice choice !(s.model_ref)
                  in
                  s.model_ref := updated_model ;
                  (* Auto-submit after choice - ignore returned state since we
                     can't update it from this callback *)
                  ignore (proceed_with_submission s updated_model)
                in
                Modal_helpers.open_choice_modal
                  ~title:modal_config.title
                  ~items:modal_config.choices
                  ~to_string:modal_config.to_string
                  ~on_select ;
                s
            | None ->
                (* No modal needed, proceed with normal submission *)
                proceed_with_submission s model)
        | None ->
            (* No pre-submit modal configured, proceed normally *)
            proceed_with_submission s model)

  and proceed_with_submission s model =
    (* Run pre-submission validation if provided *)
    match S.spec.pre_submit with
    | Some pre_fn -> (
        match pre_fn model with
        | Ok () -> (
            (* Pre-validation passed, proceed with submission *)
            match S.spec.on_submit model with
            | Ok () ->
                Context.mark_instances_dirty () ;
                (* Reset form to fresh initial values for next use *)
                s.model_ref := S.spec.initial_model () ;
                (* Navigate back to instances page via Context.
                   Set skip_back_once to prevent loop when user presses Esc on instances. *)
                Context.set_skip_back_once () ;
                Context.navigate "instances" ;
                s
            | Error (`Msg msg) ->
                Modal_helpers.show_error ~title:"Installation Failed" msg ;
                s)
        | Error (`Msg msg) ->
            Modal_helpers.show_error ~title:"Validation Failed" msg ;
            s
        | Error (`Modal (_title, action)) ->
            (* Pre-validation wants to show a modal (e.g., "Keep or Refresh data?") *)
            action () ;
            s)
    | None -> (
        (* No pre-validation, proceed directly *)
        match S.spec.on_submit model with
        | Ok () ->
            Context.mark_instances_dirty () ;
            (* Reset form to fresh initial values for next use *)
            s.model_ref := S.spec.initial_model () ;
            (* Navigate back to instances page via Context.
               Set skip_back_once to prevent loop when user presses Esc on instances. *)
            Context.set_skip_back_once () ;
            Context.navigate "instances" ;
            s
        | Error (`Msg msg) ->
            Modal_helpers.show_error ~title:"Installation Failed" msg ;
            s)

  let enter s =
    let fields = S.spec.fields !(s.model_ref) in
    if s.cursor < List.length fields then (
      let (Field field) = List.nth fields s.cursor in
      field.edit s.model_ref ;
      s)
    else submit s

  let view ps ~focus:_ ~size =
    let s = ps.Navigation.s in
    let model = !(s.model_ref) in
    let fields = S.spec.fields model in
    (* Truncate string to max length with ellipsis *)
    let truncate max_len s =
      if String.length s <= max_len then s
      else String.sub s 0 (max_len - 1) ^ "…"
    in
    (* Validate each field once and collect results *)
    let field_results =
      fields
      |> List.map (fun (Field f) ->
          let value = f.get model in
          let ok = f.validate model in
          let value_str = f.to_string value in
          let formatted_value =
            if ok then value_str
            else
              (* Show value and short error message *)
              let err_msg =
                match f.validate_msg model with
                | Some msg -> " ⚠ " ^ truncate 40 msg
                | None -> " ⚠ invalid"
              in
              Widgets.fg 214 (Widgets.bold (truncate 20 value_str ^ err_msg))
          in
          (f.label, formatted_value, ok))
    in
    let all_valid = List.for_all (fun (_, _, ok) -> ok) field_results in
    let rows =
      field_results
      |> List.map (fun (label, value, ok) ->
          (label, value, if ok then "✓" else "✗"))
    in
    let confirm_row =
      ( "Confirm & Install",
        (if all_valid then "Ready" else "Incomplete"),
        if all_valid then "✓" else "✗" )
    in
    let all_rows = rows @ [confirm_row] in
    let columns =
      [
        {
          Table_widget.Table.header = "Parameter";
          to_string = (fun (l, _, _) -> l);
        };
        {header = "Value"; to_string = (fun (_, v, _) -> v)};
        {header = "S"; to_string = (fun (_, _, st) -> st)};
      ]
    in
    let table =
      Table_widget.Table.create
        ~cols:size.LTerm_geom.cols
        ~columns
        ~rows:all_rows
        ()
    in
    let table = Table_widget.Table.move_cursor table s.cursor in
    let status_banner =
      if all_valid then
        Widgets.bg 22 (Widgets.fg 15 " ✓ Form is valid - ready to install! ")
      else Widgets.bg 160 (Widgets.fg 15 (Widgets.bold " ⚠ Form incomplete "))
    in
    (* Get hint for current field and set it for Help_hint modal *)
    (* If field is invalid, show validation error instead of hint *)
    let current_hint =
      if s.cursor < List.length fields then
        let (Field f) = List.nth fields s.cursor in
        if f.validate model then f.hint
        else
          (* Show validation error message *)
          match f.validate_msg model with
          | Some msg -> Some ("⚠ " ^ msg)
          | None -> Some "⚠ This field is invalid"
      else Some "Press Enter to install the service"
    in
    Miaou.Core.Help_hint.set current_hint ;
    let title_line = Widgets.title_highlight S.spec.title in
    let header = [title_line; status_banner] in
    let footer =
      [Widgets.dim "↑/↓ navigate, Enter to edit, ? for help, Esc back"]
    in
    Miaou_widgets_layout.Vsection.render ~size ~header ~footer ~child:(fun _ ->
        Table_widget.Table.render table)

  let handle_modal_key ps key ~size:_ =
    Miaou.Core.Modal_manager.handle_key key ;
    refresh ps

  let handle_key ps key ~size:_ =
    if Miaou.Core.Modal_manager.has_active () then (
      Miaou.Core.Modal_manager.handle_key key ;
      refresh ps)
    else
      match Miaou.Core.Keys.of_string key with
      | Some (Miaou.Core.Keys.Char "Esc") | Some (Miaou.Core.Keys.Char "Escape")
        ->
          (* Go to instances page directly - Navigation.back would go to
             details page which may have stale/consumed context.
             Set skip_back_once to prevent loop when user presses Esc on instances. *)
          Context.set_skip_back_once () ;
          Context.navigate "instances" ;
          ps
      | Some Miaou.Core.Keys.Up ->
          Navigation.update (fun s -> move_state s (-1)) ps
      | Some Miaou.Core.Keys.Down ->
          Navigation.update (fun s -> move_state s 1) ps
      | Some Miaou.Core.Keys.Enter -> Navigation.update enter ps
      | _ -> ps

  let has_modal _ = Miaou.Core.Modal_manager.has_active ()

  let service_select ps _ = ps

  let service_cycle ps _ = refresh ps

  let back ps =
    Context.set_skip_back_once () ;
    Context.navigate "instances" ;
    ps

  let move ps delta = Navigation.update (fun s -> move_state s delta) ps

  let keymap _ps =
    [
      ("Up", (fun ps -> move ps (-1)), "Move up");
      ("Down", (fun ps -> move ps 1), "Move down");
      ("Enter", (fun ps -> Navigation.update enter ps), "Edit field / Submit");
      ("Esc", back, "Back to instances");
    ]

  let handled_keys () =
    Miaou.Core.Keys.[Up; Down; Enter; Char "Esc"; Char "Escape"]
end
