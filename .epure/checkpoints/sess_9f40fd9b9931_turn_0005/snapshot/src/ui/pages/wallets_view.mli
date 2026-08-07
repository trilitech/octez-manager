(******************************************************************************)
(*                                                                            *)
(* SPDX-License-Identifier: MIT                                               *)
(* Copyright (c) 2026 Nomadic Labs <contact@nomadic-labs.com>                 *)
(*                                                                            *)
(******************************************************************************)

(** Pure rendering functions for the wallets page. No Eio calls. *)

module StringSet : Set.S with type elt = string

(** A group of keys from one base directory, with enriched metadata. *)
type enriched_group = {
  base_dir : string;
  keys : Octez_manager_lib.Keys_reader.key_metadata list;
  error : string option;
  services : string list;
  networks : string list;
  sandbox_name : string option;
}

(** Items in the flattened navigation list. *)
type nav_item =
  | GroupHeader of enriched_group
  | KeyItem of enriched_group * Octez_manager_lib.Keys_reader.key_metadata

type focus_panel = ListPanel | DetailPanel

type sort_mode = SortAlias | SortBalance | SortNetwork

val sort_mode_label : sort_mode -> string

type state = {
  groups : enriched_group list;
  nav_items : nav_item list;
  cursor : int;
  folded : StringSet.t;
  focus_panel : focus_panel;
  scroll_offset : int;
  total_keys : int;
  search_query : string;
  sort_mode : sort_mode;
  multi_select : bool;
  selected : StringSet.t;
}

(** Resolve the display alias for a key: OM alias if set, else octez-client alias. *)
val display_alias :
  base_dir:string -> Octez_manager_lib.Keys_reader.key_metadata -> string

(** Check if [haystack] contains [needle] as a substring. *)
val contains_substring : string -> string -> bool

(** Format mutez as tez with 6 decimal places. *)
val format_tez : string -> string

(** Strip URL scheme/host from network identifiers for display. *)
val pretty_network_name : string -> string

(** Short PKH for display: first 7 + last 4 chars. *)
val short_pkh : string -> string

val side_by_side_min_width : int

val view : state -> focus:bool -> size:LTerm_geom.size -> string
