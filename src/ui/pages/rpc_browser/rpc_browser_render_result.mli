(******************************************************************************)
(*                                                                            *)
(* SPDX-License-Identifier: MIT                                               *)
(* Copyright (c) 2026 Nomadic Labs <contact@nomadic-labs.com>                 *)
(*                                                                            *)
(******************************************************************************)

(** RPC Browser result mode rendering.

    Pure rendering functions for the result viewing mode of the RPC Browser.
    Displays JSON responses with syntax highlighting and scrolling support. *)

(** {1 Header Rendering} *)

(** Render header with request URL, response time, and size.
    @param request URL that was requested
    @param response_time_ms Response time in milliseconds
    @param response_size Response body size in bytes *)
val render_header :
  request:string ->
  response_time_ms:float option ->
  response_size:int option ->
  string

(** {1 Body Rendering} *)

(** Render JSON body with optional syntax highlighting.
    Applies scroll offset and limits visible lines to fit height.
    @param body Response body (may be highlighted)
    @param scroll_offset Vertical scroll position
    @param visible_height Number of visible lines
    @return List of rendered lines *)
val render_body :
  body:string -> scroll_offset:int -> visible_height:int -> string list

(** Render scroll position indicator.
    @param current Current scroll position
    @param total Total number of lines
    @return Scroll indicator string *)
val render_scroll_indicator : current:int -> total:int -> string

(** {1 Status Rendering} *)

(** Render loading indicator for pending request. *)
val render_loading : unit -> string

(** Render error message. *)
val render_error : string -> string

(** {1 Help Line} *)

(** Render keyboard help line for result mode. *)
val render_help : unit -> string

(** {1 Main Rendering} *)

(** Render complete result view from state.
    @param state Current RPC Browser state (must be in Result mode)
    @param cols Terminal width
    @param rows Terminal height
    @return List of rendered lines *)
val render :
  state:Rpc_browser_state.state -> cols:int -> rows:int -> string list
