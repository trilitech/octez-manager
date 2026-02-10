(******************************************************************************)
(*                                                                            *)
(* SPDX-License-Identifier: MIT                                               *)
(* Copyright (c) 2025-2026 Nomadic Labs <contact@nomadic-labs.com>            *)
(*                                                                            *)
(******************************************************************************)

type entry = {
  network : string;
  slug : string;
  label : string;
  download_url : string option;
  history_mode : string option;
  metadata : (string * string) list;
}

(** Strip a trailing [-YYYY-MM-DD] date suffix from a slug.  Teztnets encodes
    the rotation date in the URL (e.g. [weeklynet-2026-02-04]) but the
    tzinit.org snapshot mirror uses the bare alias ([weeklynet]). *)
val strip_date_suffix : string -> string

(** Extract a tzinit network slug from either an alias ("mainnet") or a
    teztnets JSON URL. *)
val slug_of_network : string -> string option

(** Normalize user-provided snapshot kind strings (e.g. "full:50") into the
    slug used by tzinit ("full50"). *)
val sanitize_kind_input : string -> string option

(** Fetch metadata for a specific snapshot kind. Returns [Ok None] when the
    page is not found (HTTP 404). *)
val fetch_entry :
  network_slug:string ->
  slug:string ->
  label:string ->
  (entry option, Rresult.R.msg) result

(** List all snapshot kinds advertised for a network. Falls back to common
    kinds when the tzinit index cannot be parsed. *)
val list : network_slug:string -> (entry list, Rresult.R.msg) result

(** Helper to temporarily override the HTTP fetcher while running tests. *)
module For_tests : sig
  val with_fetch :
    (string -> (int * string, Rresult.R.msg) result) -> (unit -> 'a) -> 'a

  val fetch_html_with :
    try_eio:(unit -> (int * string, Rresult.R.msg) result) ->
    try_curl:(unit -> (int * string, Rresult.R.msg) result) ->
    (int * string, Rresult.R.msg) result
end
