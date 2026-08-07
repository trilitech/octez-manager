(******************************************************************************)
(*                                                                            *)
(* SPDX-License-Identifier: MIT                                               *)
(* Copyright (c) 2026 Nomadic Labs <contact@nomadic-labs.com>                 *)
(*                                                                            *)
(******************************************************************************)

(** RPC Browser result mode rendering.

    Pure rendering functions for the result viewing mode of the RPC Browser.
    Supports multi-pager layout with grid display and dynamic sizing. *)

(** {1 Layout Constants} *)

(** Minimum width per pager (standard terminal width). *)
val min_pager_cols : int

(** Minimum height per pager (standard terminal height). *)
val min_pager_rows : int

(** {1 Pager Header Rendering} *)

(** Render pager header with ID, request URL, response time, and size.
    @param slot Pager slot to render header for
    @param is_focused Whether this pager is focused
    @param is_target Whether this pager is the target for next RPC result *)
val render_pager_header :
  slot:Rpc_browser_state.pager_slot ->
  is_focused:bool ->
  is_target:bool ->
  string

(** {1 Status Rendering} *)

(** Render loading indicator for pending request. *)
val render_loading : unit -> string

(** Render error message. *)
val render_error : string -> string

(** {1 Help Line} *)

(** Render keyboard help line for result mode.
    @param num_pagers Number of pagers currently open *)
val render_help : num_pagers:int -> string

(** {1 Single Pager Rendering} *)

(** Render using pager widget.
    @param pager The pager widget
    @param cols Terminal width
    @param rows Terminal height
    @param focus Whether pager has focus *)
val render_with_pager :
  pager:Miaou_widgets_display.Pager_widget.t ->
  cols:int ->
  rows:int ->
  focus:bool ->
  string

(** Render a single pager slot.
    @param slot The pager slot to render
    @param cols Available width
    @param rows Available height
    @param is_focused Whether this pager is focused
    @param is_target Whether this pager is the target for next RPC result *)
val render_single_pager :
  slot:Rpc_browser_state.pager_slot ->
  cols:int ->
  rows:int ->
  is_focused:bool ->
  is_target:bool ->
  string

(** {1 Multi-Pager Layout} *)

(** Render hidden pager indicator showing which pagers are off-screen.
    @param hidden_left IDs of pagers hidden to the left
    @param hidden_right IDs of pagers hidden to the right *)
val render_hidden_indicator :
  hidden_left:int list -> hidden_right:int list -> string

(** Grid layout dimensions. *)
type grid_layout = {grid_cols : int; grid_rows : int}

(** Calculate optimal grid layout based on available space and number of pagers.
    Tries different grid arrangements and picks the one that maximizes
    space per pager while respecting minimum constraints.
    @param cols Available width
    @param rows Available height
    @param num_pagers Total number of pagers
    @return (grid_layout, max_visible_pagers) *)
val calculate_layout :
  cols:int -> rows:int -> num_pagers:int -> grid_layout * int

(** Get visible pager slots based on focus - focused pager is always visible.
    @param pagers All pager slots
    @param focused_id ID of the focused pager
    @param max_visible Maximum number of visible pagers
    @return (visible_pagers, hidden_left_ids, hidden_right_ids) *)
val get_visible_pagers :
  pagers:Rpc_browser_state.pager_slot list ->
  focused_id:int ->
  max_visible:int ->
  Rpc_browser_state.pager_slot list * int list * int list

(** Render pager tabs for single-column mode.
    @param pagers All pager slots
    @param focused_id ID of the focused pager *)
val render_pager_tabs :
  pagers:Rpc_browser_state.pager_slot list -> focused_id:int -> string

(** {1 Testing} *)

(** Functions exposed for testing. *)
module For_tests : sig
  (** Calculate visible length of a string excluding ANSI escapes, handling UTF-8. *)
  val visible_length : string -> int

  (** Truncate a string with ANSI codes to a visible width.
      @param width Maximum visible width *)
  val truncate_to_width : string -> width:int -> string

  (** Split string into lines, padding/truncating to exact dimensions.
      @param target_lines Number of lines to produce
      @param width Width to pad/truncate each line to *)
  val split_lines_padded :
    string -> target_lines:int -> width:int -> string list
end

(** {1 Main Rendering} *)

(** Render complete result view from state with multi-pager support.
    @param state Current RPC Browser state (must be in Result mode)
    @param cols Terminal width
    @param rows Terminal height
    @param focus Whether result view has focus
    @return Rendered content as single string *)
val render :
  state:Rpc_browser_state.state -> cols:int -> rows:int -> focus:bool -> string
