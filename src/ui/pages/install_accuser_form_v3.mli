(******************************************************************************)
(*                                                                            *)
(* SPDX-License-Identifier: MIT                                               *)
(* Copyright (c) 2025-2026 Nomadic Labs <contact@nomadic-labs.com>            *)
(*                                                                            *)
(******************************************************************************)

(** Accuser installation form using field bundles *)

(** Form model holding accuser configuration fields. *)
type model = {
  core : Form_builder_common.core_service_config;
      (** Shared service config (name, bin dir, user, etc.). *)
  client : Form_builder_common.client_config;
      (** Client-side config (base dir, node endpoint). *)
  edit_mode : bool;  (** [true] when editing an existing accuser instance. *)
  original_instance : string option;
      (** Original instance name in edit mode. *)
  stopped_dependents : string list;
      (** Services that were stopped for the edit. *)
}

(** Page identifier for registration *)
val name : string

(** The page module implementing accuser installation *)
val page : Miaou.Core.Registry.page

(** Register the accuser installation page in the Miaou registry *)
val register : unit -> unit

(** The page module for direct TUI initialization (used in tests) *)
module Page : Miaou.Core.Tui_page.PAGE_SIG

module For_tests : sig
  (** Build the model as it would be constructed on page entry, including
      edit-mode prefill from a pending edit context, if any. *)
  val initial_model : unit -> model

  val initial_base_dir : string

  (** Test helper: simulate setting instance name and return resulting base_dir *)
  val base_dir_after_set_instance_name : instance_name:string -> string
end
