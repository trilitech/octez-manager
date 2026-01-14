(*****************************************************************************)
(*                                                                           *)
(* SPDX-License-Identifier: MIT                                              *)
(* Copyright (c) 2025 Nomadic Labs <contact@nomadic-labs.com>                *)
(*                                                                           *)
(*****************************************************************************)

(** Polymorphic selection widget for choosing from a list of items.

    This widget provides a scrollable list interface for selecting items
    of any type. It supports keyboard navigation (arrows, Home/End, PageUp/PageDown),
    cancellation (Esc), and optional sectioned lists with headers.

    {b Typical usage}:
    {[
      (* Create a select widget *)
      type color = Red | Green | Blue

      let color_to_string = function
        | Red -> "Red"
        | Green -> "Green"
        | Blue -> "Blue"

      let widget = Select_widget.open_centered
        ~title:"Choose a color"
        ~items:[Red; Green; Blue]
        ~to_string:color_to_string
        ()

      (* In your PAGE_SIG module *)
      let view state ~focus ~size =
        Select_widget.render_with_size widget ~focus ~size

      let handle_key state ~key ~size =
        let widget' = Select_widget.handle_key_with_size widget ~key ~size in
        ...

      (* Get selected item *)
      match Select_widget.get_selection widget' with
      | Some color -> ...
      | None -> ...
    ]}
*)

(** The select widget type, parameterized by item type ['a] *)
type 'a t

(** {1 Creation} *)

(** Create a select widget with centered modal-style rendering.

    @param cursor Initial cursor position (default: 0)
    @param max_visible Maximum number of visible items (default: unlimited)
    @param title Title displayed at the top
    @param items List of items to choose from
    @param to_string Function to convert items to display strings
*)
val open_centered :
  ?cursor:int ->
  ?max_visible:int ->
  title:string ->
  items:'a list ->
  to_string:('a -> string) ->
  unit ->
  'a t

(** Create a select widget with sectioned items (groups with headers).

    @param cursor_label Initial item to select (by display string)
    @param max_visible Maximum number of visible items (default: unlimited)
    @param title Title displayed at the top
    @param sections List of (header, items) pairs
    @param to_string Function to convert items to display strings
*)
val open_centered_sectioned :
  ?cursor_label:string ->
  ?max_visible:int ->
  title:string ->
  sections:(string * 'a list) list ->
  to_string:('a -> string) ->
  unit ->
  'a t

(** {1 Rendering} *)

(** Render the select widget with size awareness.

    Recommended for use in PAGE_SIG modules where size is available.

    @param backend Rendering backend (Terminal or SDL, default: current backend)
    @param focus Whether the widget has focus
    @param size Terminal/window size for responsive rendering
*)
val render_with_size :
  ?backend:Miaou_widgets_display.Widgets.backend ->
  'a t ->
  focus:bool ->
  size:LTerm_geom.size ->
  string

(** Render the select widget with default size assumptions.

    Uses a default size of 24x80. For better responsiveness, prefer
    {!render_with_size} when size information is available.

    @param backend Rendering backend (Terminal or SDL, default: current backend)
    @param focus Whether the widget has focus
*)
val render :
  ?backend:Miaou_widgets_display.Widgets.backend -> 'a t -> focus:bool -> string

(** Render for a specific backend (advanced).

    @param backend Rendering backend
    @param focus Whether the widget has focus
*)
val render_for_backend :
  Miaou_widgets_display.Widgets.backend -> 'a t -> focus:bool -> string

(** {1 Input Handling} *)

(** Handle keyboard input with size awareness.

    Processes keyboard input and returns an updated select widget state.

    Supported keys:
    - [Up]/[Down]: Move cursor
    - [PageUp]/[PageDown]: Move by page (8 items)
    - [Home]/[End]: Jump to first/last item
    - [Esc]: Mark as cancelled

    @param key The key string (e.g., "Up", "Down", "Enter", "Esc")
    @param size Terminal/window size (currently unused but provided for future use)
    @return Updated select widget state
*)
val handle_key_with_size : 'a t -> key:string -> size:LTerm_geom.size -> 'a t

(** Handle keyboard input with default size assumptions.

    Convenience wrapper around {!handle_key_with_size} with default 24x80 size.

    @param key The key string (e.g., "Up", "Down", "Enter", "Esc")
    @return Updated select widget state
*)
val handle_key : 'a t -> key:string -> 'a t

(** {1 State Queries} *)

(** Get the currently selected item.

    Returns [None] if the list is empty, [Some item] otherwise.
    The selected item is determined by the current cursor position.
*)
val get_selection : 'a t -> 'a option

(** Check if the user pressed Esc to cancel selection.

    This flag is set when the user presses Esc and can be used to
    distinguish between confirmed selection and cancellation.
*)
val is_cancelled : 'a t -> bool

(** Clear the cancelled flag.

    Resets the widget's cancelled state to false.
*)
val reset_cancelled : 'a t -> 'a t
