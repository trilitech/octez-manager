(*****************************************************************************)
(*                                                                           *)
(* SPDX-License-Identifier: MIT                                              *)
(* Copyright (c) 2025 Nomadic Labs <contact@nomadic-labs.com>                *)
(*                                                                           *)
(*****************************************************************************)
open Miaou_widgets_display.Widgets

type 'a validation_result = Valid of 'a | Invalid of string

type 'a validator = string -> 'a validation_result

type 'a t = {
  textbox : Textbox_widget.t;
  validator : 'a validator;
  validation_state : 'a validation_result;
  (* Debounce support *)
  debounce_ms : int;
  last_input_time : float;
  pending_validation : bool;
}

let default_debounce_ms = 250

let create ?title ?(width = 60) ?(initial = "") ?(placeholder = None)
    ?(debounce_ms = default_debounce_ms) ~validator () =
  let textbox = Textbox_widget.create ?title ~width ~initial ~placeholder () in
  let validation_state = validator initial in
  {
    textbox;
    validator;
    validation_state;
    debounce_ms;
    last_input_time = 0.;
    pending_validation = false;
  }

let open_centered ?title ?(width = 60) ?(initial = "") ?(placeholder = None)
    ?(debounce_ms = default_debounce_ms) ~validator () =
  create ?title ~width ~initial ~placeholder ~debounce_ms ~validator ()

let run_validation t =
  let current_value = Textbox_widget.value t.textbox in
  let validation_state = t.validator current_value in
  {t with validation_state; pending_validation = false}

(* Check if debounce period has elapsed and run validation if needed.
   Call this before render to update the validation state. *)
let tick t =
  if not t.pending_validation then t
  else
    let now = Unix.gettimeofday () in
    let elapsed_ms = (now -. t.last_input_time) *. 1000. in
    if elapsed_ms >= float_of_int t.debounce_ms then run_validation t else t

let render t ~focus =
  let base_render = Textbox_widget.render t.textbox ~focus in
  match t.validation_state with
  | Valid _ -> base_render
  | Invalid error_msg ->
      let error_display =
        if visible_chars_count error_msg > 60 then
          let idx = visible_byte_index_of_pos error_msg 57 in
          String.sub error_msg 0 idx ^ "..."
        else error_msg
      in
      (* Add red coloring to indicate error and show error message below *)
      let colored_base = red base_render in
      colored_base ^ "\n" ^ red ("⚠ " ^ error_display)

let handle_key t ~key =
  (* First, check if any pending validation should run now *)
  let t = tick t in
  let updated_textbox = Textbox_widget.handle_key t.textbox ~key in
  let text_changed =
    Textbox_widget.value updated_textbox <> Textbox_widget.value t.textbox
  in
  if text_changed then
    (* Text changed: mark validation as pending, record timestamp *)
    let now = Unix.gettimeofday () in
    if t.debounce_ms <= 0 then
      (* No debounce: validate immediately *)
      run_validation {t with textbox = updated_textbox}
    else (
      (* Debounce enabled: defer validation and request a re-render *)
      Miaou_helpers.Render_notify.request_render () ;
      {
        t with
        textbox = updated_textbox;
        last_input_time = now;
        pending_validation = true;
      })
  else
    (* No text change (e.g., cursor movement): no validation needed *)
    {t with textbox = updated_textbox}

let is_cancelled t = Textbox_widget.is_cancelled t.textbox

let reset_cancelled t =
  let reset_textbox = Textbox_widget.reset_cancelled t.textbox in
  {t with textbox = reset_textbox}

let value t = Textbox_widget.value t.textbox

let validation_result t = t.validation_state

let is_valid t =
  match t.validation_state with Valid _ -> true | Invalid _ -> false

let get_validated_value t =
  match t.validation_state with Valid v -> Some v | Invalid _ -> None

let get_error_message t =
  match t.validation_state with Valid _ -> None | Invalid msg -> Some msg

let width t = Textbox_widget.width t.textbox

let with_width t width =
  let textbox = Textbox_widget.with_width t.textbox width in
  {t with textbox}

(* Force immediate validation, useful before form submission *)
let flush_validation t = if t.pending_validation then run_validation t else t

(* Check if there's a pending validation that hasn't run yet *)
let has_pending_validation t = t.pending_validation
