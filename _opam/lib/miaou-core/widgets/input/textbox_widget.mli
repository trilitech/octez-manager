(*****************************************************************************)
(*                                                                           *)
(* SPDX-License-Identifier: MIT                                              *)
(* Copyright (c) 2025 Nomadic Labs <contact@nomadic-labs.com>                *)
(*                                                                           *)
(*****************************************************************************)

(** Single-line text input widget with cursor support.

    This widget provides a text input box with editing capabilities including
    cursor movement, backspace/delete, and placeholder text. It's commonly used
    in modal forms for user text input.

    {b Typical usage}:
    {[
      (* Create a textbox *)
      let textbox = Textbox_widget.open_centered
        ~title:"Enter your name"
        ~width:40
        ~initial:""
        ~placeholder:(Some "Type here...")
        ()

      (* In your PAGE_SIG module *)
      let view state ~focus ~size =
        Textbox_widget.render state ~focus

      let handle_key state ~key ~size =
        Textbox_widget.handle_key state ~key

      (* Extract the final text *)
      let text = Textbox_widget.get_text state
    ]}
*)

(** The textbox state. This type is opaque; use the provided functions to
    create and manipulate textbox instances. *)
type t

(** {1 Creation} *)

(** Create a new textbox with the specified configuration.

    @param title Optional title displayed above the textbox
    @param width Display width in characters (default: 60, minimum: 4)
    @param initial Initial text content (default: "")
    @param placeholder Optional placeholder text shown when empty (displayed dimmed)
*)
val create :
  ?title:string ->
  ?width:int ->
  ?initial:string ->
  ?placeholder:string option ->
  unit ->
  t

(** Alias for {!create}. The name "open_centered" is historical and will work
    the same as [create]. *)
val open_centered :
  ?title:string ->
  ?width:int ->
  ?initial:string ->
  ?placeholder:string option ->
  unit ->
  t

(** {1 Rendering} *)

(** Render the textbox as a string for display.

    Returns a formatted string representation of the textbox, including:
    - Optional title (if set)
    - Text content with cursor (shown as underscore: [_])
    - Placeholder text (dimmed, when content is empty)
    - Border characters: [\[]...\]]
    - Padding to match the configured width

    @param focus Whether the widget has focus (currently unused, but kept for
      consistency with other widget APIs)

    {b Example output}:
    {v
    Enter your name
    [Hello_world                              ]
    v}
*)
val render : t -> focus:bool -> string

(** {1 Input Handling} *)

(** Handle a keyboard input event.

    Processes keyboard input and returns an updated textbox state. Supports:
    - Printable characters: Insert at cursor position
    - [Backspace]: Delete character before cursor
    - [Delete]: Delete character at cursor
    - [Left]/[Right]: Move cursor
    - [Home]: Move cursor to start
    - [End]: Move cursor to end
    - [Esc]/[Escape]: Mark textbox as cancelled

    @param key The key string (e.g., "a", "Enter", "Backspace")
    @return Updated textbox state

    {b Note}: This function does NOT handle [Enter] - you typically want to
    handle Enter in your modal's [handle_key] to trigger form submission.
*)
val handle_key : t -> key:string -> t

(** {1 Text Access} *)

(** Get the current text content of the textbox. *)
val get_text : t -> string

(** Alias for {!get_text}. *)
val value : t -> string

(** Set the text content, preserving the cursor position if possible.
    If the new text is shorter than the current cursor position, the cursor
    is moved to the end of the new text. *)
val set_text : t -> string -> t

(** Set both text and cursor position explicitly.
    The cursor position is clamped to the valid range [0, String.length text]. *)
val set_text_with_cursor : t -> text:string -> cursor:int -> t

(** {1 State Queries} *)

(** Check if the user pressed Esc/Escape.
    This flag is set when the user presses Esc and can be used to distinguish
    between form cancellation and normal submission. *)
val is_cancelled : t -> bool

(** Clear the cancelled flag. *)
val reset_cancelled : t -> t

(** Get the current cursor position (character index). *)
val cursor : t -> int

(** Get the display width of the textbox. *)
val width : t -> int

(** Set a new display width, returning an updated textbox.
    The width is clamped to a minimum of 4 characters. *)
val with_width : t -> int -> t
