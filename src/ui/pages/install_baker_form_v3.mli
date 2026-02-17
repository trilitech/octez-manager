(******************************************************************************)
(*                                                                            *)
(* SPDX-License-Identifier: MIT                                               *)
(* Copyright (c) 2026 Nomadic Labs <contact@nomadic-labs.com>                 *)
(*                                                                            *)
(******************************************************************************)

(** Baker installation form using field bundles. *)

(** How the baker connects to a DAL node. *)
type dal_selection =
  | Dal_none  (** No DAL node configured. *)
  | Dal_instance of string  (** Use a managed DAL node instance by name. *)
  | Dal_endpoint of string
      (** Use an external DAL node at the given endpoint. *)

(** How the baker handles key signing. *)
type signer_selection =
  | Signer_local_keys  (** Use local key files from base directory. *)
  | Signer_instance of string  (** Use a managed Signatory instance by name. *)
  | Signer_uri of string  (** Use an external remote signer at the given URI. *)

(** Form model holding all baker configuration fields. *)
type model = {
  core : Form_builder_common.core_service_config;
      (** Shared service config (name, bin dir, user, etc.). *)
  client : Form_builder_common.client_config;
      (** Client-side config (base dir, node endpoint). *)
  parent_node : string;
      (** Parent node instance name (empty for external node). *)
  node_data_dir : string;  (** Data directory of the parent node. *)
  dal : dal_selection;  (** DAL node connection mode. *)
  delegates : string list;  (** List of delegate public key hashes. *)
  liquidity_baking_vote : string;
      (** Liquidity baking toggle vote (["on"], ["off"], or ["pass"]). *)
  signer : signer_selection;  (** Key signing mode. *)
  edit_mode : bool;  (** [true] when editing an existing baker instance. *)
  original_instance : string option;
      (** Original instance name in edit mode. *)
  stopped_dependents : string list;
      (** Services that were stopped for the edit. *)
}

(** Page name for the page registry. *)
val name : string

(** Pre-built page value for registration. *)
val page : Miaou.Core.Registry.page

(** Register this page with the global page registry. *)
val register : unit -> unit

(** Page implementation satisfying the Miaou TUI page signature. *)
module Page : Miaou.Core.Tui_page.PAGE_SIG

(** Testing interface exposing internal helpers. *)
module For_tests : sig
  (** Return the default initial model with empty/default field values. *)
  val initial_model : unit -> model

  (** Determine whether the baker runs in [`Local] or [`Remote] node mode
      based on the model and current service states. *)
  val baker_node_mode : model -> Data.Service_state.t list -> [`Local | `Remote]
end
