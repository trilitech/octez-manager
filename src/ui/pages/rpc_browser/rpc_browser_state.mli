(******************************************************************************)
(*                                                                            *)
(* SPDX-License-Identifier: MIT                                               *)
(* Copyright (c) 2026 Nomadic Labs <contact@nomadic-labs.com>                 *)
(*                                                                            *)
(******************************************************************************)

(** RPC Browser state management.

    Pure functional state transformations for the RPC Browser page. *)

open Octez_manager_lib

(** Entry kind for navigation list. *)
type entry_kind = Get | Sub | Dyn of string

(** A navigation entry. *)
type entry = {name : string; kind : entry_kind}

(** Display mode. *)
type mode =
  | List of {
      entries : entry list;  (** Available entries at current path *)
      cursor : int;  (** Selected entry index *)
      loading : bool;  (** Whether entries are being fetched *)
    }
  | Result of {
      request : string;  (** Full URL requested *)
      body : string;  (** Response body (may be highlighted) *)
      raw_body : string;  (** Original unformatted response *)
      scroll_offset : int;  (** Vertical scroll position *)
      response_time_ms : float option;  (** Request duration in milliseconds *)
      response_size : int option;  (** Response body size in bytes *)
    }

(** OpenAPI loading status. *)
type openapi_status = Loading | Ready | Error of string | NotAvailable

(** RPC Browser page state. *)
type state = {
  instances : Service.t list;  (** Available instances with RPC *)
  selected_idx : int;  (** Currently selected instance index *)
  path : string list;  (** Current navigation path segments *)
  mode : mode;  (** Current display mode *)
  openapi_status : openapi_status;  (** OpenAPI spec status *)
  error : string option;  (** Last error message *)
}

(** {1 Initialization} *)

(** Create initial state.
    @param instances List of services with RPC endpoints *)
val init : instances:Service.t list -> state

(** {1 Instance Selection} *)

(** Select an instance by index.
    Returns unchanged state if index out of bounds. *)
val select_instance : int -> state -> state

(** Get the currently selected instance, if any. *)
val current_instance : state -> Service.t option

(** {1 Navigation} *)

(** Navigate into a child path.
    Clears current entries and sets loading. *)
val navigate_to : string -> state -> state

(** Navigate up one level in the path.
    Returns to root if at top level. *)
val navigate_up : state -> state

(** Navigate to root path. *)
val navigate_root : state -> state

(** {1 Entry Updates} *)

(** Set entries for current path.
    Clears loading state and error. *)
val set_entries : entry list -> state -> state

(** Set loading state for entries. *)
val set_loading : bool -> state -> state

(** {1 Result Mode} *)

(** Execute a GET request (sets mode to Result with loading).
    @param url Full URL being requested *)
val execute_get : url:string -> state -> state

(** Set result body after successful request.
    @param body Formatted/highlighted body
    @param raw_body Original response
    @param response_time_ms Optional request duration in milliseconds
    @param response_size Optional response body size in bytes *)
val set_result :
  body:string ->
  raw_body:string ->
  ?response_time_ms:float ->
  ?response_size:int ->
  state ->
  state

(** {1 Cursor Movement} *)

(** Move cursor up in list mode. *)
val cursor_up : state -> state

(** Move cursor down in list mode. *)
val cursor_down : state -> state

(** Scroll result view. *)
val scroll : int -> state -> state

(** {1 Errors} *)

(** Set error message. *)
val set_error : string -> state -> state

(** Clear error message. *)
val clear_error : state -> state

(** {1 OpenAPI Status} *)

(** Set OpenAPI loading status. *)
val set_openapi_status : openapi_status -> state -> state
