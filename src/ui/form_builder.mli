(*****************************************************************************)
(*                                                                           *)
(* SPDX-License-Identifier: MIT                                              *)
(* Copyright (c) 2025 Nomadic Labs <contact@nomadic-labs.com>                *)
(*                                                                           *)
(*****************************************************************************)

[@@@warning "-67"] (* Disable unused-functor-parameter warning *)

(** A generic, declarative form builder for creating installer pages.

    This module provides a way to create form-based installer pages without
    writing repetitive boilerplate. Define your form model and fields
    declaratively, and the builder generates a complete PAGE_SIG implementation.

    Example:
    {[
      type model = {
        name : string;
        network : string;
        enable : bool;
      }

      let spec = {
        title = " Install Node ";
        initial_model = { name = ""; network = "mainnet"; enable = true };
        fields = [
          text ~label:"Name" ~get:(fun m -> m.name)
               ~set:(fun v m -> {m with name = v});
          choice ~label:"Network" ~get:(fun m -> m.network)
                 ~set:(fun v m -> {m with network = v})
                 ~items:["mainnet"; "ghostnet"];
          toggle ~label:"Enable on Boot" ~get:(fun m -> m.enable)
                 ~set:(fun v m -> {m with enable = v});
        ];
        on_submit = install_node;
      }
    ]}
*)

(** An existentially quantified field wrapper. *)
type 'model field

(** Configuration for a pre-submission choice modal (with existential type for choices). *)
type 'model pre_submit_modal_config =
  | PreSubmitModal : {
      title : string;
      message : string option;
      choices : 'choice list;
      to_string : 'choice -> string;
      on_choice : 'choice -> 'model -> 'model;
    } -> 'model pre_submit_modal_config

(** A specification for an entire form page. *)
type 'model spec = {
  title : string;
  initial_model : 'model;
  fields : 'model field list;
  (** Optional initialization hook called once when page is first loaded.
      Use for prefetching data, starting background tasks, etc. *)
  on_init : ('model -> unit) option;
  (** Optional refresh hook called on every refresh cycle.
      WARNING: Keep this lightweight - it's called frequently (on every keystroke).
      Avoid spawning subprocesses or expensive operations here. *)
  on_refresh : ('model -> unit) option;
  (** Pre-submission validation with custom error handling.
      Return Ok() to proceed with submission, or Error to show modal and abort. *)
  pre_submit : ('model -> (unit, [`Msg of string | `Modal of string * (unit -> unit)]) result) option;
  (** Optional conditional modal shown before submission.
      If the function returns Some modal_config, shows a choice modal and updates
      the model based on user's choice. User must then submit again to proceed.
      If returns None, proceeds directly to on_submit. *)
  pre_submit_modal : ('model -> 'model pre_submit_modal_config option) option;
  (** Main submission handler. Can perform async operations, show progress modals, etc.
      Return Ok() for success (navigates to instances), Error for failure (shows error modal). *)
  on_submit : 'model -> (unit, [`Msg of string]) result;
}

(** Functor to generate a full page module from a specification.
    The resulting module implements {!Miaou.Core.Tui_page.PAGE_SIG}. *)
module Make (S : sig
  type model

  val spec : model spec
end) : Miaou.Core.Tui_page.PAGE_SIG

(** {1 Field Constructors} *)

(** Simple text input field. *)
val text :
  label:string ->
  get:('model -> string) ->
  set:(string -> 'model -> 'model) ->
  'model field

(** Text input with validation. *)
val validated_text :
  label:string ->
  get:('model -> string) ->
  set:(string -> 'model -> 'model) ->
  validate:('model -> (unit, string) result) ->
  'model field

(** Boolean toggle (flips on Enter). *)
val toggle :
  label:string ->
  get:('model -> bool) ->
  set:(bool -> 'model -> 'model) ->
  'model field

(** Choice from a static list. *)
val choice :
  label:string ->
  get:('model -> 'a) ->
  set:('a -> 'model -> 'model) ->
  to_string:('a -> string) ->
  items:'a list ->
  'model field

(** Choice from a dynamic list (computed from current model). *)
val dynamic_choice :
  label:string ->
  get:('model -> 'a) ->
  set:('a -> 'model -> 'model) ->
  to_string:('a -> string) ->
  get_items:('model -> 'a list) ->
  'model field

(** Read-only display field. *)
val readonly :
  label:string ->
  get:('model -> string) ->
  'model field

(** Field with custom editing action. *)
val custom :
  label:string ->
  get:('model -> string) ->
  edit:('model ref -> unit) ->
  ?validate:('model -> bool) ->
  unit ->
  'model field

(** Node data directory selector (creates if needed). *)
val node_data_dir :
  label:string ->
  get:('model -> string) ->
  set:(string -> 'model -> 'model) ->
  ?validate:('model -> bool) ->
  unit ->
  'model field

(** Client base directory selector (creates if needed). *)
val client_base_dir :
  label:string ->
  get:('model -> string) ->
  set:(string -> 'model -> 'model) ->
  ?validate:('model -> bool) ->
  unit ->
  'model field

(** Application binary directory selector. *)
val app_bin_dir :
  label:string ->
  get:('model -> string) ->
  set:(string -> 'model -> 'model) ->
  ?validate:('model -> bool) ->
  unit ->
  'model field

(** Extra args field that opens binary help explorer.
    @param binary The binary name ("octez-node", "octez-baker")
    @param subcommand Optional subcommand (e.g., ["run"] for node, ["run"; "accuser"] for accuser) *)
val extra_args :
  label:string ->
  get_args:('model -> string) ->
  set_args:(string -> 'model -> 'model) ->
  get_bin_dir:('model -> string) ->
  binary:string ->
  ?subcommand:string list ->
  unit ->
  'model field

(** {1 Advanced Field Constructors} *)

(** Service selection with external endpoint option.
    Shows existing service instances of a given role OR allows entering an external endpoint.
    @param role Service role to filter (e.g., "node", "dal-node")
    @param external_label Label for the external endpoint option (e.g., "External endpoint...")
    @param endpoint_validator Optional validator for external endpoints *)
val service_or_endpoint :
  label:string ->
  role:string ->
  get:('model -> [`Service of string | `Endpoint of string | `None]) ->
  set:([`Service of string | `Endpoint of string | `None] -> 'model -> 'model) ->
  ?external_label:string ->
  ?endpoint_validator:(string -> (unit, string) result) ->
  unit ->
  'model field

(** String list editor with optional suggestions.
    Allows adding/removing strings with optional pre-populated suggestions that can be toggled.
    @param get_suggestions Optional function to get suggested items from current model *)
val string_list :
  label:string ->
  get:('model -> string list) ->
  set:(string list -> 'model -> 'model) ->
  ?get_suggestions:('model -> string list) ->
  ?item_validator:(string -> (unit, string) result) ->
  unit ->
  'model field

(** Endpoint input with host:port validation. *)
val endpoint :
  label:string ->
  get:('model -> string) ->
  set:(string -> 'model -> 'model) ->
  ?default_port:int ->
  unit ->
  'model field
