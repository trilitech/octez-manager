(*****************************************************************************)
(*                                                                           *)
(* SPDX-License-Identifier: MIT                                              *)
(* Copyright (c) 2025 Nomadic Labs <contact@nomadic-labs.com>                *)
(*                                                                           *)
(*****************************************************************************)

(** Scrollable text pager widget with search and streaming support.

    This widget provides a full-featured text viewer with:
    - Scrolling (Up/Down, PageUp/PageDown, Home/End)
    - Search with regex support (/, n/N for next/prev match)
    - Follow mode for streaming logs
    - Batched appends for performance
    - Line wrapping support
    - JSON syntax highlighting (optional)

    {b Typical usage}:
    {[
      (* Create a pager from text *)
      let pager = Pager_widget.open_text ~title:"Logs" "Line 1\nLine 2\nLine 3" in

      (* Or from lines *)
      let pager = Pager_widget.open_lines ~title:"Output" ["Line 1"; "Line 2"] in

      (* Render in your view *)
      let view state ~size ~focus =
        Pager_widget.render_with_size ~size pager ~focus

      (* Handle keyboard input *)
      let handle_key state ~key ~size =
        let pager', consumed = Pager_widget.handle_key pager ~key in
        if consumed then {state with pager = pager'} else ...

      (* For streaming logs, use batched appends *)
      let pager = Pager_widget.start_streaming pager in
      Pager_widget.append_text_batched pager "new log line\n" ;
      (* pager automatically flushes pending lines on render *)
    ]}
*)

(** The pager state.

    This type is opaque - use the provided functions to create and manipulate
    pager instances. Internal state includes scroll position, lines, search
    query, and streaming buffers.
*)
type t = {
  title : string option;
  mutable lines : string list;
  mutable offset : int;
  mutable follow : bool;
  mutable wrap : bool;
  mutable streaming : bool;
  mutable spinner_pos : int;
  mutable pending_lines : string list;
  mutable pending_rev : string list;
  mutable pending_dirty : bool;
  mutable cached_body : string option;
  mutable last_flush : float;
  mutable flush_interval_ms : int;
  mutable last_win : int;
  mutable last_cols : int;
  mutable search : string option;
  mutable is_regex : bool;
  mutable input_mode : [`None | `Search_edit | `Lookup | `Help];
  mutable input_buffer : string;
  mutable input_pos : int;
  mutable notify_render : (unit -> unit) option;
}

(** {1 Creation} *)

(** Create a pager from a list of lines.

    @param title Optional title displayed at the top
    @param notify_render Optional callback invoked when content changes
           (useful for background threads to request UI refresh)
    @param lines Initial list of lines to display
*)
val open_lines :
  ?title:string -> ?notify_render:(unit -> unit) -> string list -> t

(** Create a pager from text (splits on newlines).

    @param title Optional title displayed at the top
    @param notify_render Optional callback invoked when content changes
    @param text Text content (will be split on '\\n')
*)
val open_text : ?title:string -> ?notify_render:(unit -> unit) -> string -> t

(** {1 Content Updates} *)

(** Append lines immediately (synchronous).

    Lines are added to the pager's content right away. For high-frequency
    updates from background threads, prefer {!append_lines_batched}.

    @param lines Lines to append
*)
val append_lines : t -> string list -> unit

(** Append text immediately (synchronous).

    Text is split on newlines and appended. For high-frequency updates,
    prefer {!append_text_batched}.

    @param text Text to append (will be split on '\\n')
*)
val append_text : t -> string -> unit

(** Append lines in batched mode (for streaming/performance).

    Lines are buffered and flushed at a limited rate (default: 200ms intervals)
    to avoid excessive rendering. Useful for streaming logs from background
    threads.

    Note: Call {!start_streaming} first to enable streaming mode.

    @param lines Lines to append to the pending buffer
*)
val append_lines_batched : t -> string list -> unit

(** Append text in batched mode (for streaming/performance).

    Text is split on newlines and added to the pending buffer. Flushed
    at limited rate during rendering.

    @param text Text to append (will be split on '\\n')
*)
val append_text_batched : t -> string -> unit

(** {1 Streaming Mode} *)

(** Enable streaming mode with spinner animation.

    In streaming mode:
    - Spinner shows activity in the title area
    - Batched appends are buffered and flushed at intervals
    - Automatic flush at limited rate (default: 200ms)
    - Follow mode can be enabled to auto-scroll to latest content

    {b Follow Mode and Streaming:}
    Follow mode (toggled with 'f' key) works independently of streaming mode,
    but they complement each other well:
    - When follow mode is ON: viewport automatically shows the last N lines,
      scrolling down as new content arrives
    - When follow mode is OFF: viewport stays at current scroll position
    - Streaming mode handles efficient batched updates regardless of follow mode

    Typical usage: enable streaming before starting a background task that will
    call {!append_text_batched} repeatedly, and enable follow mode (press 'f')
    if you want to auto-tail the output.

    {[
      let pager = Pager_widget.open_text ~title:"Build Log" "" in
      Pager_widget.start_streaming pager ;
      (* User can press 'f' to enable follow mode for auto-tailing *)
      (* Background task appends lines *)
      Pager_widget.append_text_batched pager "Building...\n" ;
      (* ... *)
      Pager_widget.stop_streaming pager
    ]}
*)
val start_streaming : t -> unit

(** Disable streaming mode and flush all pending lines.

    Stops the spinner animation and ensures all buffered lines are
    displayed immediately.
*)
val stop_streaming : t -> unit

(** Flush pending batched lines if needed.

    Manually triggers a flush of buffered lines. Normally called
    automatically during rendering.

    @param force If true, flushes regardless of interval timer
*)
val flush_pending_if_needed : ?force:bool -> t -> unit

(** {1 Navigation} *)

(** Set the scroll offset (line number at top of viewport).

    @param offset Line number to scroll to (0-indexed)
    @return New pager with updated offset
*)
val set_offset : t -> int -> t

(** {1 Search} *)

(** Set search query and jump to first match.

    Resets offset to 0 and highlights all matching lines.

    @param query Search string (supports regex if configured)
    @return New pager with search query set
*)
val set_search : t -> string option -> t

(** {1 Rendering} *)

(** Render the pager with explicit window size.

    @param cols Terminal width in columns (optional, for wrapping)
    @param wrap Enable line wrapping (default: true)
    @param win Visible window height in lines
    @param focus Whether the pager has focus (affects styling)
    @return Rendered string
*)
val render : ?cols:int -> win:int -> t -> focus:bool -> string

(** Render with size-aware window calculation.

    Convenience wrapper that extracts window size from LambdaTerm size record.

    @param size Terminal/window size
    @param focus Whether the pager has focus
    @return Rendered string
*)
val render_with_size : size:'a -> t -> focus:bool -> string

(** {1 Input Handling} *)

(** Handle keyboard input.

    Processes navigation keys, search commands, and other pager controls.

    {b Navigation Keys:}
    - [Up]/[Down]: Scroll by line (disables follow mode)
    - [Page_up]/[Page_down]: Scroll by page (disables follow mode)
    - [g]: Jump to start (disables follow mode)
    - [G]: Jump to end (disables follow mode)

    {b Search:}
    - [/]: Enter search mode - displays search prompt
    - When in search mode:
      - Type characters to build search query
      - [Backspace]: Delete character before cursor
      - [Left]/[Right]: Move cursor within search query
      - [Enter]: Execute search and exit search mode
      - [Esc]: Cancel search and exit search mode
    - [n]: Jump to next search match
    - [p]: Jump to previous search match

    {b Follow Mode:}
    - [f] or [F]: Toggle follow mode
    - When follow mode is enabled, the pager automatically scrolls to show
      the latest content (bottom of the log). Any manual scrolling (Up/Down/
      PageUp/PageDown/g/G) will disable follow mode.
    - Follow mode is useful for tailing streaming logs.

    {b Integration Example:}
    {[
      (* In your page/component that embeds the pager: *)
      let handle_key state ~key ~size =
        (* First, try to let the pager handle the key *)
        let pager', consumed = Pager_widget.handle_key state.pager ~key in
        if consumed then
          (* Pager handled it - update state with new pager *)
          { state with pager = pager' }
        else
          (* Pager didn't handle it - process it yourself *)
          match key with
          | "q" -> exit_page state
          | "Tab" -> switch_focus state
          | _ -> state
    ]}

    @param win Visible window height in lines (optional, default: 20)
    @param key Key string (e.g., "Up", "Down", "/", "n", "a", "Backspace")
    @return Tuple of (updated pager, consumed flag). If consumed is true,
            the key was handled by the pager. If false, the key should be
            processed by the parent.
*)
val handle_key : ?win:int -> t -> key:string -> t * bool

(** {1 JSON Streaming (Advanced)} *)

(** JSON streamer state for incremental parsing with syntax highlighting *)
type json_streamer

(** Create a new JSON streamer for incremental parsing. *)
val json_streamer_create : unit -> json_streamer

(** Feed a chunk of JSON to the streamer.

    Returns newly completed, colorized lines.

    @param chunk Raw JSON text chunk
    @return List of completed, syntax-highlighted lines
*)
val json_streamer_feed : json_streamer -> string -> string list
