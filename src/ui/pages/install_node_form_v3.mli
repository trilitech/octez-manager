(******************************************************************************)
(*                                                                            *)
(* SPDX-License-Identifier: MIT                                               *)
(* Copyright (c) 2026 Nomadic Labs <contact@nomadic-labs.com>                 *)
(*                                                                            *)
(******************************************************************************)

(** Node installation form using field bundles. *)

open Octez_manager_lib

(** A snapshot entry from the TzInit snapshot provider. *)
type tzinit_snapshot = {
  network_slug : string;  (** Network identifier slug (e.g. ["mainnet"]). *)
  kind_slug : string;  (** Snapshot kind slug (e.g. ["rolling"]). *)
  label : string;  (** Human-readable label for display. *)
}

(** User's snapshot selection for node installation. *)
type snapshot_selection = [`None | `Url of string | `Tzinit of tzinit_snapshot]

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
  (** Clear the cached snapshot entries for all networks. *)
  val clear_snapshot_cache : unit -> unit

  (** Populate the snapshot cache for a specific [network]. *)
  val set_snapshot_cache :
    network:string -> entries:Snapshots.entry list -> unit

  (** Check whether a snapshot entry matches the given [history_mode]
      (e.g. ["rolling"], ["full"], ["archive"]). *)
  val snapshot_entry_matches_history_mode :
    Snapshots.entry -> history_mode:string -> bool

  (** Detect whether the selected snapshot conflicts with the chosen
      history mode for the given [network]. Returns [true] on conflict. *)
  val history_snapshot_conflict :
    history_mode:string -> snapshot:snapshot_selection -> network:string -> bool

  (** Generate instance name from network and history mode.
      Format: node-{network} for rolling, node-{network}-{history_mode} for full/archive *)
  val generate_instance_name : network:string -> history_mode:string -> string

  (** Format the display string for a selected tzinit snapshot.
      Shows "tzinit · {label}" using the human-readable label (or the
      kind slug as fallback), without duplicating the slug in parentheses. *)
  val format_selected_snapshot : tzinit_snapshot -> string

  (** Opaque form model, exposed only to drive {!set_node_with_autoname}
      from tests. Fields are intentionally not revealed. *)
  type model

  (** A model as the wizard produces it for a fresh (non-edit) install:
      network = ["shadownet"], history_mode = ["rolling"], with the
      instance name auto-generated to match. *)
  val fresh_model : unit -> model

  (** Test-only setter overriding the model's snapshot selection directly,
      bypassing any field on-change logic. *)
  val with_snapshot : snapshot_selection -> model -> model

  (** Read back the model's node configuration. *)
  val node_of : model -> Form_builder_common.node_config

  (** Read back the model's current snapshot selection. *)
  val snapshot_of : model -> snapshot_selection

  (** The autoname/autosnapshot transition applied whenever the Network or
      History Mode field changes value (this is the [~set_node] callback
      wired into the History Mode field). *)
  val set_node_with_autoname : Form_builder_common.node_config -> model -> model
end
