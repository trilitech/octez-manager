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
  get : 'model -> 'value;
  set : 'value -> 'model -> 'model; [@warning "-69"]
  to_string : 'value -> string;
  validate : 'model -> bool;
  validate_msg : 'model -> string option;
  edit : 'model ref -> unit;
}

(** Existentially wrapped field (hides the 'value type parameter). *)
type 'model field = Field : ('model, 'value) field_internal -> 'model field

type 'model spec = {
  title : string;
  initial_model : 'model;
  fields : 'model field list;
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
  Field {label; get; set; to_string; validate; validate_msg; edit}

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
  Field {label; get; set; to_string; validate = validate_fn; validate_msg; edit}

let toggle ~label ~get ~set =
  let to_string = string_of_bool in
  let validate _ = true in
  let validate_msg _ = None in
  let edit model_ref =
    let current = get !model_ref in
    model_ref := set (not current) !model_ref
  in
  Field {label; get; set; to_string; validate; validate_msg; edit}

let choice ~label ~get ~set ~to_string ~items =
  let validate _ = true in
  let validate_msg _ = None in
  let edit model_ref =
    let on_select v = model_ref := set v !model_ref in
    Modal_helpers.open_choice_modal ~title:label ~items ~to_string ~on_select
  in
  Field {label; get; set; to_string; validate; validate_msg; edit}

let dynamic_choice ~label ~get ~set ~to_string ~get_items =
  let validate _ = true in
  let validate_msg _ = None in
  let edit model_ref =
    let items = get_items !model_ref in
    let on_select v = model_ref := set v !model_ref in
    Modal_helpers.open_choice_modal ~title:label ~items ~to_string ~on_select
  in
  Field {label; get; set; to_string; validate; validate_msg; edit}

let readonly ~label ~get =
  let set _ m = m in
  let to_string v = v in
  let validate _ = true in
  let validate_msg _ = None in
  let edit _ = () in
  Field {label; get; set; to_string; validate; validate_msg; edit}

let custom ~label ~get ~edit ?(validate = fun _ -> true) () =
  let set _ m = m in
  let to_string v = v in
  let validate_msg _ = None in
  Field {label; get; set; to_string; validate; validate_msg; edit}

let node_data_dir ~label ~get ~set ?(validate = fun _ -> true) () =
  let to_string v = v in
  let validate_msg _ = None in
  let edit model_ref =
    let on_select path = model_ref := set path !model_ref in
    Modal_helpers.select_node_data_dir_modal ~on_select ()
  in
  Field {label; get; set; to_string; validate; validate_msg; edit}

let client_base_dir ~label ~get ~set ?(validate = fun _ -> true) () =
  let to_string v = v in
  let validate_msg _ = None in
  let edit model_ref =
    let on_select path = model_ref := set path !model_ref in
    Modal_helpers.select_client_base_dir_modal ~on_select ()
  in
  Field {label; get; set; to_string; validate; validate_msg; edit}

let app_bin_dir ~label ~get ~set ?(validate = fun _ -> true) () =
  let to_string v = v in
  let validate_msg _ = None in
  let edit model_ref =
    let on_select path = model_ref := set path !model_ref in
    Modal_helpers.select_app_bin_dir_modal ~on_select ()
  in
  Field {label; get; set; to_string; validate; validate_msg; edit}

let extra_args ~label ~get_args ~set_args ~get_bin_dir ~binary ?subcommand () =
  let to_string args = if args = "" then "(none)" else args in
  let validate _ = true in
  let validate_msg _ = None in
  let get = get_args in
  let set args m = set_args args m in
  let edit model_ref =
    let app_bin_dir = get_bin_dir !model_ref in
    let on_apply tokens =
      let arg_str = String.concat " " tokens in
      model_ref := set_args arg_str !model_ref
    in
    (* Call appropriate help function based on binary and subcommand *)
    match (binary, subcommand) with
    | ("octez-node", _) ->
        Binary_help_explorer.open_node_run_help ~app_bin_dir ~on_apply
    | ("octez-baker", Some ["run"; "accuser"]) | ("octez-baker", Some ["run"; "with"; _; _; _; "accuser"]) ->
        Binary_help_explorer.open_accuser_run_help ~app_bin_dir ~on_apply
    | ("octez-baker", _) ->
        (* For baker, we need to determine local vs remote mode *)
        Binary_help_explorer.open_baker_run_help ~app_bin_dir ~mode:`Local ~on_apply
    | _ ->
        (* Generic fallback - show error for now *)
        Modal_helpers.show_error
          ~title:"Extra Args"
          (Printf.sprintf "Binary help not available for %s" binary)
  in
  Field {label; get; set; to_string; validate; validate_msg; edit}

(*****************************************************************************)
(*                                  FUNCTOR                                  *)
(*****************************************************************************)

module Make (S : sig
  type model

  val spec : model spec
end) =
struct
  type state = {
    model_ref : S.model ref;
    cursor : int;
    mutable next_page : string option;
  }

  type msg = unit

  let init () =
    {model_ref = ref S.spec.initial_model; cursor = 0; next_page = None}

  let update s _ = s

  let refresh s =
    (match Context.consume_navigation () with
    | Some p -> s.next_page <- Some p
    | None -> ()) ;
    s

  let move s delta =
    let max_cursor = List.length S.spec.fields in
    let cursor = max 0 (min max_cursor (s.cursor + delta)) in
    {s with cursor}

  let submit s =
    let model = !(s.model_ref) in
    (* Check all validations before submitting *)
    let validation_errors =
      List.filter_map
        (fun (Field f) ->
          if f.validate model then None else Some (f.label, f.validate_msg model))
        S.spec.fields
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
        match S.spec.on_submit model with
        | Ok () ->
            Context.mark_instances_dirty () ;
            s.next_page <- Some "instances" ;
            s
        | Error (`Msg msg) ->
            Modal_helpers.show_error ~title:"Installation Failed" msg ;
            s)

  let enter s =
    if s.cursor < List.length S.spec.fields then (
      let (Field field) = List.nth S.spec.fields s.cursor in
      field.edit s.model_ref ;
      s)
    else submit s

  let view s ~focus:_ ~size =
    let model = !(s.model_ref) in
    let all_valid =
      List.for_all (fun (Field f) -> f.validate model) S.spec.fields
    in
    let rows =
      S.spec.fields
      |> List.map (fun (Field f) ->
             let value = f.get model in
             let ok = f.validate model in
             let value_str = f.to_string value in
             let formatted_value =
               if ok then value_str
               else Widgets.fg 214 (Widgets.bold value_str)
             in
             (f.label, formatted_value, if ok then "✓" else "✗"))
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
        {header = "S"; to_string = (fun (_, _, s) -> s)};
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
      else
        Widgets.bg 160
          (Widgets.fg 15 (Widgets.bold " ⚠ Form incomplete "))
    in
    let title_line = Widgets.title_highlight S.spec.title in
    let header = [title_line; status_banner] in
    let footer = [Widgets.dim "↑/↓ navigate, Enter to edit, Esc back"] in
    Miaou_widgets_layout.Vsection.render ~size ~header ~footer ~child:(fun _ ->
        Table_widget.Table.render table)

  let handle_modal_key s key ~size:_ =
    Miaou.Core.Modal_manager.handle_key key ;
    refresh s

  let handle_key s key ~size:_ =
    if Miaou.Core.Modal_manager.has_active () then (
      Miaou.Core.Modal_manager.handle_key key ;
      refresh s)
    else
      match Miaou.Core.Keys.of_string key with
      | Some (Miaou.Core.Keys.Char "Esc") | Some (Miaou.Core.Keys.Char "Escape") ->
          s.next_page <- Some "instances" ;
          s
      | Some Miaou.Core.Keys.Up -> move s (-1)
      | Some Miaou.Core.Keys.Down -> move s 1
      | Some Miaou.Core.Keys.Enter -> enter s
      | _ -> s

  let next_page s = s.next_page

  let has_modal _ = Miaou.Core.Modal_manager.has_active ()

  let service_select s _ = s

  let service_cycle s _ = s

  let back s =
    s.next_page <- Some "instances" ;
    s

  let keymap _ =
    [
      ("Up", (fun s -> s), "Move up");
      ("Down", (fun s -> s), "Move down");
      ("Enter", (fun s -> s), "Edit field / Submit");
      ("Esc", (fun s -> s), "Back to instances");
    ]

  let handled_keys () =
    Miaou.Core.Keys.[Up; Down; Enter; Char "Esc"; Char "Escape"]
end
