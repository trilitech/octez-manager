(******************************************************************************)
(*                                                                            *)
(* SPDX-License-Identifier: MIT                                               *)
(* Copyright (c) 2026 Nomadic Labs <contact@nomadic-labs.com>                 *)
(*                                                                            *)
(******************************************************************************)

(** RPC node selection page for choosing local or public nodes. *)

open Octez_manager_lib

(** A selectable node entry. *)
type node_item = {
  label : string;  (** Display label for the node. *)
  rpc_addr : string;
      (** RPC endpoint address (e.g. ["https://mainnet.api.tez.ie"]). *)
  is_public : bool;
      (** [true] for public nodes, [false] for local instances. *)
  network : string option;  (** Network name if known (e.g. [Some "mainnet"]). *)
}

(** An entry in the flat display list. *)
type display_item =
  | SectionHeader of string  (** Section heading (e.g. ["PUBLIC NODES"]). *)
  | NetworkHeader of string  (** Network subheading (e.g. ["Mainnet"]). *)
  | NodeItem of node_item  (** A selectable node entry. *)

(** Page state holding the loaded nodes and cursor position. *)
type state = {
  public_nodes : node_item list;  (** Public nodes fetched from Taquito. *)
  local_instances : node_item list;  (** Locally configured node instances. *)
  cursor : int;  (** Currently highlighted item index. *)
  loading : bool;  (** [true] while fetching public nodes. *)
  error : string option;  (** Error message from the last fetch attempt. *)
  display_items : display_item list;  (** Flat list of all display entries. *)
}

(** Page name for the page registry. *)
val name : string

(** Pre-built page value for registration. *)
val page : Miaou.Core.Registry.page

(** Register this page with the global page registry. *)
val register : unit -> unit

(** Page implementation satisfying the Miaou TUI page signature. *)
module Page : Miaou.Core.Tui_page.PAGE_SIG

(** Parse Taquito JSON response into a list of public node items. *)
val parse_taquito_json : string -> node_item list

(** Curated list of default public nodes used as fallback. *)
val curated_defaults : node_item list

(** Build a flat display list with section headers, network headers,
    and node items from the given public and local node lists. *)
val build_display_items :
  public_nodes:node_item list ->
  local_instances:node_item list ->
  display_item list

(** Return the total number of display items. *)
val total_items : state -> int

(** Return the item at the current cursor position.
    Headers are distinguished from selectable nodes. *)
val get_item_at_cursor :
  state -> [`SectionHeader | `NetworkHeader | `Node of node_item | `None]

(** Move the cursor by [delta] positions, skipping non-selectable headers. *)
val move_cursor : int -> state -> state

(** Create a synthetic {!Service.t} from a node item, suitable for
    passing to the RPC browser page. *)
val make_service_for_node : node_item -> Service.t
