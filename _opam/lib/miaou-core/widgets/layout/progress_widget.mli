(*****************************************************************************)
(*                                                                           *)
(* SPDX-License-Identifier: MIT                                              *)
(* Copyright (c) 2025 Nomadic Labs <contact@nomadic-labs.com>                *)
(*                                                                           *)
(*****************************************************************************)

(** Progress bar widget for visualizing task completion.

    This widget provides a horizontal progress bar with percentage display,
    styled with gradient colors (when using Unicode). Supports both inline
    and centered/modal variants.

    {b Typical usage}:
    {[
      (* Create an inline progress bar *)
      let progress = Progress_widget.open_inline ~width:40 ~label:(Some "Downloading") () in

      (* Update progress *)
      let progress = Progress_widget.set_progress progress 0.5 in  (* 50% *)

      (* Render *)
      let output = Progress_widget.render progress in
      (* Output: "Downloading [████████████████████          ] 50%" *)

      (* Or create a centered modal variant *)
      let progress = Progress_widget.open_centered ~title:"Processing" ~width:50 () in
    ]}
*)

(** The progress bar state *)
type t

(** {1 Creation} *)

(** Create an inline progress bar.

    An inline progress bar is rendered as a single line with optional label:
    [Label [████████░░░░░░░░] 42%]

    @param width Width of the progress bar in columns (minimum: 1)
    @param label Optional label displayed to the left of the bar
*)
val open_inline : width:int -> ?label:string -> unit -> t

(** Create a centered progress bar (modal-style).

    A centered progress bar is rendered with a title above:
    {v
      Title
      [████████░░░░░░░░] 42%
    v}

    @param title Title displayed above the progress bar
    @param width Width of the progress bar in columns (minimum: 1)
*)
val open_centered : title:string -> width:int -> unit -> t

(** {1 State Updates} *)

(** Set the progress value.

    @param progress Value between 0.0 (0%) and 1.0 (100%), automatically clamped
    @return Updated progress bar with new progress value
*)
val set_progress : t -> float -> t

(** Get the current progress value.

    @return Current progress as a float between 0.0 and 1.0
*)
val get_progress : t -> float

(** Set or update the label.

    @param label New label (use [Some "text"] to set, [None] to clear)
    @return Updated progress bar with new label
*)
val set_label : t -> string option -> t

(** {1 Rendering} *)

(** Render for terminal backend with column width awareness.

    @param cols Terminal width in columns (used for layout decisions)
*)
val render_terminal : t -> cols:int -> string

(** Render the progress bar with column width.

    Returns a string representation of the progress bar, styled with:
    - Gradient colors (purple/pink) when using Unicode
    - ASCII fallback ([###    ]) when Unicode is disabled
    - Percentage display (e.g., " 42%")
    - Optional label prefix

    The rendering automatically adapts to the current backend (Terminal/SDL)
    and respects the ASCII mode setting.

    @param cols Terminal width in columns (used for layout decisions)
*)
val render : t -> cols:int -> string
