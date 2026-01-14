(*****************************************************************************)
(*                                                                           *)
(* SPDX-License-Identifier: MIT                                              *)
(* Copyright (c) 2025 Nomadic Labs <contact@nomadic-labs.com>                *)
(*                                                                           *)
(*****************************************************************************)

(** Animated spinner widget for indicating ongoing operations.

    This widget provides a simple animated spinner with optional label,
    useful for showing that a background task is in progress.

    {b Typical usage}:
    {[
      (* Create a spinner *)
      let spinner = Spinner_widget.open_centered ~label:(Some "Loading...") () in

      (* In your render loop, tick the spinner to advance animation *)
      let spinner' = Spinner_widget.tick spinner in
      let output = Spinner_widget.render spinner' in
      (* Output: "⠋ Loading..." (glyph animates on each tick) *)

      (* Update the label *)
      let spinner' = Spinner_widget.set_label spinner' (Some "Processing...") in
    ]}
*)

(** The spinner state *)
type t

(** {1 Creation} *)

(** Create a centered spinner.

    @param label Optional label displayed after the spinner glyph
    @param width Maximum width in columns (content truncated if longer, default: 60)
*)
val open_centered : ?label:string -> ?width:int -> unit -> t

(** {1 Animation} *)

(** Advance the spinner animation to the next frame.

    Call this repeatedly (e.g., on each render or timer tick) to
    animate the spinner. The spinner cycles through frames:
    - Unicode: ⠋ ⠙ ⠹ ⠸ ⠼ ⠴ ⠦ ⠧ ⠇ ⠏ (10 frames)
    - ASCII: | / - \\ (4 frames)
*)
val tick : t -> t

(** {1 State Updates} *)

(** Set or update the label.

    @param label New label (use [Some "text"] to set, [None] to clear)
    @return Updated spinner with new label
*)
val set_label : t -> string option -> t

(** {1 Rendering} *)

(** Render the spinner.

    Returns a string with the current animation frame and label:
    - ["⠋ Loading..."] (Unicode mode)
    - ["| Loading..."] (ASCII mode)

    The output is automatically truncated to the configured width.

    @param backend Rendering backend (Terminal or SDL, default: current backend)
*)
val render : ?backend:Miaou_widgets_display.Widgets.backend -> t -> string

(** Render with explicit backend selection (advanced).

    @param backend Rendering backend to use
*)
val render_with_backend : Miaou_widgets_display.Widgets.backend -> t -> string
