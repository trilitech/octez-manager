(*****************************************************************************)
(*                                                                           *)
(* SPDX-License-Identifier: MIT                                              *)
(* Copyright (c) 2025 Nomadic Labs <contact@nomadic-labs.com>                *)
(*                                                                           *)
(*****************************************************************************)

type 'a validation_result = Valid of 'a | Invalid of string

type 'a validator = string -> 'a validation_result

type 'a t

(** [create ?debounce_ms ~validator ()] creates a validated textbox.

    @param debounce_ms Delay in milliseconds before running validation after
    the last keystroke. Default is 250ms. Set to 0 to disable debouncing and
    validate immediately on every keystroke (legacy behavior). *)
val create :
  ?title:string ->
  ?width:int ->
  ?initial:string ->
  ?placeholder:string option ->
  ?debounce_ms:int ->
  validator:'a validator ->
  unit ->
  'a t

val open_centered :
  ?title:string ->
  ?width:int ->
  ?initial:string ->
  ?placeholder:string option ->
  ?debounce_ms:int ->
  validator:'a validator ->
  unit ->
  'a t

(** Check if debounce period has elapsed and run pending validation.
    Call this before [render] in your view function to update validation state.
    Returns the widget unchanged if no validation is pending or debounce
    period hasn't elapsed yet. *)
val tick : 'a t -> 'a t

val render : 'a t -> focus:bool -> string

val handle_key : 'a t -> key:string -> 'a t

val is_cancelled : 'a t -> bool

val reset_cancelled : 'a t -> 'a t

val value : 'a t -> string

val validation_result : 'a t -> 'a validation_result

val is_valid : 'a t -> bool

val get_validated_value : 'a t -> 'a option

val get_error_message : 'a t -> string option

val width : 'a t -> int

val with_width : 'a t -> int -> 'a t

(** Force immediate validation, bypassing debounce. Useful before form
    submission to ensure validation reflects the current text value. *)
val flush_validation : 'a t -> 'a t

(** Returns [true] if there's a pending validation that hasn't run yet
    due to debouncing. *)
val has_pending_validation : 'a t -> bool

(** Usage:
    {[
      let validate_int s =
        match int_of_string_opt s with
        | Some v when v >= 0 -> Valid v
        | _ -> Invalid "Enter a non-negative integer"
      in
      let box = create ~title:"Instances" ~validator:validate_int () in
      let box = handle_key box ~key:"1" in
      render box ~focus:true
    ]}
    Keys: forwarded to [Textbox_widget.handle_key]; render shows validation errors in red. *)
