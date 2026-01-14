(*****************************************************************************)
(*                                                                           *)
(* SPDX-License-Identifier: MIT                                              *)
(* Copyright (c) 2025 Nomadic Labs <contact@nomadic-labs.com>                *)
(*                                                                           *)
(*****************************************************************************)
(* Typed Workflow DSL for Miaou Pages & Modals                                *)
(*                                                                            *)
(* This module provides a small, GADT‑based DSL to script deterministic       *)
(* headless UI workflows across TUI pages and modal dialogs while preserving  *)
(* static guarantees about which operations are legal in a given UI context.  *)
(*                                                                            *)
(* Key Ideas                                                                  *)
(*  - A workflow is a typed sequence of primitive steps that either operate   *)
(*    on a page, on an active modal, or are context‑independent assertions.   *)
(*  - The GADT encodes at the type level whether a modal is currently active, *)
(*    preventing accidental submission / key routing when no modal exists.    *)
(*  - Interpreting a workflow drives the existing headless driver (no code    *)
(*    duplication) and returns a final value.                                 *)
(*                                                                            *)
(* Non‑Goals (initial iteration)                                               *)
(*  - Exhaustive modelling of every key source.                               *)
(*  - Deep static tracking of concrete page identity after switches.          *)
(*  - Rich time abstractions.                                                 *)
(*                                                                            *)
(* Extension Hooks                                                             *)
(*  - Page switching detection (future): capture next_page events and allow   *)
(*    typed branching on target page name.                                    *)
(*  - Modal value extraction (future): expose typed extractors.               *)
(*  - Property tracing / shrinking for property tests.                        *)
(******************************************************************************)

(* Forward declare PAGE signature fields we actually need to avoid ordering issues. *)
module type PAGE = sig
  type state

  val init : unit -> state
end

(** Phantom markers for UI context *)
module Ctx : sig
  type page

  type modal
end

(** A workflow step producing a value of type ['a] from a given UI context ['ctx].
    ['ctx] encodes whether a modal is active (Ctx.page or Ctx.modal). *)
type (_, _) t =
  | Return : 'a -> ('a, 'ctx) t
  | Bind : ('a, 'c) t * ('a -> ('b, 'c) t) -> ('b, 'c) t
  | Page : (module PAGE with type state = 's) * 's -> ('s, Ctx.page) t
  | Feed_keys : string list * (unit, 'ctx) t -> (unit, 'ctx) t
  | Await_modal : {
      max_iters : int;
      sleep : float;
      screen_pred : (string -> bool) option;
      k : (unit, Ctx.page) t;
    }
      -> (unit, Ctx.modal) t
  | Await_no_modal : {
      max_iters : int;
      sleep : float;
      k : (unit, Ctx.modal) t;
    }
      -> (unit, Ctx.page) t
  | Seq_modal_to_page :
      (unit, Ctx.modal) t * (unit, Ctx.page) t
      -> (unit, Ctx.page) t
  | Expect : (string -> bool) * (unit, 'ctx) t -> (unit, 'ctx) t
  | Capture_screen : (string -> 'a) * ('b, 'ctx) t -> ('a * 'b, 'ctx) t
  | Map : ('a -> 'b) * ('a, 'ctx) t -> ('b, 'ctx) t
  | When : (string -> bool) * (unit, 'ctx) t * (unit, 'ctx) t -> (unit, 'ctx) t
  | Loop_until : {
      max_iters : int;
      sleep : float;
      pred : string -> bool;
      k : (unit, 'ctx) t;
    }
      -> (unit, 'ctx) t

(** Driver abstraction supplying side‑effects used by the interpreter. *)
type driver = {
  feed_key : string -> unit;  (** enqueue a single key *)
  feed_keys : string list -> unit;
      (** enqueue a batch (may default to iter feed_key) *)
  screen : unit -> string;  (** current rendered screen buffer *)
  has_modal : unit -> bool;  (** true if a modal is currently active *)
  sleep : float -> unit;  (** sleep (seconds, fractional allowed) *)
  log : string -> unit;  (** debugging / trace hook *)
}

(** Register a global driver used by [run]/[run_modal]. *)
val register_driver : driver -> unit

(** Obtain currently registered driver or raise if absent. *)
val current_driver : unit -> driver

val with_driver : driver -> (unit -> 'a) -> 'a

(** Monadic / applicative helpers *)
val return : 'a -> ('a, 'ctx) t

val ( let* ) : ('a, 'c) t -> ('a -> ('b, 'c) t) -> ('b, 'c) t

val ( let+ ) : ('a, 'ctx) t -> ('a -> 'b) -> ('b, 'ctx) t

(** Context transitions are only performed by the explicit primitives
  [await_modal] (page -> modal) and [await_no_modal] (modal -> page).
  Monadic [Bind] is now same‑context to prevent accidental silent shifts. *)

(** Construct an initial page workflow. *)
val start_page : (module PAGE with type state = 's) -> ('s, Ctx.page) t

(** Feed raw key strings (enqueued) before continuing. *)
val feed : string list -> (unit, 'ctx) t

(** Await a modal becoming active (with bounded polling) *)
val await_modal :
  ?max_iters:int ->
  ?sleep:float ->
  ?screen_pred:(string -> bool) ->
  unit ->
  (unit, Ctx.page) t ->
  (unit, Ctx.modal) t

(** Await modal dismissal, returning to page context. *)
val await_no_modal :
  ?max_iters:int ->
  ?sleep:float ->
  unit ->
  (unit, Ctx.modal) t ->
  (unit, Ctx.page) t

(** Explicitly sequence a modal action (ignored value) followed by a page action.
  This is a controlled context transition helper complementing [await_no_modal]
  when multiple modal-local steps must occur before returning to page. *)
val seq_modal_to_page :
  (unit, Ctx.modal) t -> (unit, Ctx.page) t -> (unit, Ctx.page) t

(** Assert predicate on current screen; fails (raises) if predicate false. *)
val expect : (string -> bool) -> (unit, 'ctx) t

(** Branch based on current screen predicate. *)
val when_ :
  (string -> bool) -> (unit, 'ctx) t -> (unit, 'ctx) t -> (unit, 'ctx) t

(** Loop until predicate matches or iteration/time budget exhausted. *)
val loop_until :
  ?max_iters:int -> ?sleep:float -> (string -> bool) -> (unit, 'ctx) t

(** Run a workflow starting from an implicit freshly rendered page. *)
val run : ('a, Ctx.page) t -> 'a

(** Run providing an explicit driver (bypasses the global registry). *)
val run_with : driver -> ('a, Ctx.page) t -> 'a

(** Run while already inside a modal context (rare). *)
val run_modal : ('a, Ctx.modal) t -> 'a

(** Run a modal workflow with explicit driver. *)
val run_modal_with : driver -> ('a, Ctx.modal) t -> 'a

(** Error reporting and structured failures *)
type error = {
  step : string;  (** high-level step identifier *)
  message : string;  (** human readable description *)
  attempt : int option;  (** iteration attempt (for polling primitives) *)
  screen : string option;  (** snapshot of screen when captured *)
}

exception Workflow_error of error

val pp_error : error -> string

val run_result : ('a, Ctx.page) t -> ('a, error) result

val run_modal_result : ('a, Ctx.modal) t -> ('a, error) result

(** Convenience: typical Enter -> modal -> Enter -> close flow.
  NOTE: Assumes a single modal layer; stacked modals will require
  composing explicit await_modal calls. *)
val simple_modal_flow :
  open_keys:string list -> confirm_keys:string list -> (unit, Ctx.page) t

(** Helper builders (test convenience) *)
val navigate_menu_item : label:string -> downs:string list -> (unit, Ctx.page) t
(** [navigate_menu_item ~label ~downs] feeds the provided [downs] "Down" key sequence
      then Enter. It assumes the actions menu is already open. *)

(** Helper: build a [screen_pred] that matches a modal whose title line contains the
  given substring. Titles are typically rendered on the first or second line
  (depending on framing). We keep it simple and just regex search anywhere. *)
val modal_title_pred : substring:string -> string -> bool

(** Poll for a registered service whose [params] JSON contains a
  ["delegate_key_alias"] entry (used by baker association flows). Returns when
  present or raises [Workflow_error] on timeout. Implemented as a pure screen
  predicate loop so we avoid a hard dependency on [Service_manager] in the DSL.
  The predicate function supplied should identify that the screen has advanced
  to a state where reading alias is expected (e.g. Instances page refreshed). *)
val await_delegate_alias_param :
  ?max_iters:int ->
  ?sleep:float ->
  alias:string ->
  (string -> bool) ->
  (unit, Ctx.page) t

(** Poll until the textual screen output shows an env args fragment (e.g.
  a space-separated delegate key list) for a baker. This is a heuristic
  screen-level wait that complements registry polling done in tests. *)
val await_env_args_fragment :
  ?max_iters:int ->
  ?sleep:float ->
  fragment:string ->
  unit ->
  (unit, Ctx.page) t

(******************************************************************************)
(* Example Usage                                                               *)
(******************************************************************************)
(** Capture_screen: captures the current screen, computes [f screen] and then
  evaluates [k], returning a pair [(captured, result_of_k)]. This avoids
  repeating side effects when both values are needed. *)

(******************************************************************************)
(* Notes                                                                       *)
(*  - All sleeps are best-effort micro-delays; they should be small to keep    *)
(*    tests fast.                                                              *)
(*  - Errors raise Workflow_error with diagnostic messages.                    *)
(******************************************************************************)
