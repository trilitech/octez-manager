(******************************************************************************)
(*                                                                            *)
(* SPDX-License-Identifier: MIT                                               *)
(* Copyright (c) 2026 Nomadic Labs <contact@nomadic-labs.com>                 *)
(*                                                                            *)
(******************************************************************************)

(** Snapshot download and import functionality for node installation *)

open Installer_types

(** Check if requested history mode matches snapshot metadata *)
val history_mode_matches :
  requested:History_mode.t -> snapshot_mode:string -> bool

(** Resolve automatic snapshot download details from tzinit service *)
val resolve_snapshot_download :
  network:string ->
  history_mode:History_mode.t ->
  (snapshot_resolution, Rresult.R.msg) result

(** Local snapshot file with cleanup flag *)
type snapshot_file = {path : string; cleanup : bool}

(** Progress callback for snapshot download *)
type snapshot_progress = {
  on_download_progress : (int -> int option -> unit) option;
}

(** Download snapshot from URL to temporary file *)
val download_snapshot :
  ?quiet:bool ->
  ?on_log:(string -> unit) ->
  ?progress:snapshot_progress ->
  ?tmp_dir:string ->
  string ->
  (snapshot_file, Rresult.R.msg) result

(** Prepare snapshot source (download if URL, or validate local path) *)
val prepare_snapshot_source :
  ?quiet:bool ->
  ?on_log:(string -> unit) ->
  ?progress:snapshot_progress ->
  ?tmp_dir:string ->
  string ->
  (snapshot_file, Rresult.R.msg) result

(** Convert bootstrap request to snapshot plan *)
val snapshot_plan_of_request :
  node_request -> (snapshot_plan, Rresult.R.msg) result

(** Convert snapshot plan to metadata for service registry *)
val snapshot_metadata_of_plan :
  no_check:bool -> snapshot_plan -> snapshot_metadata

(** Import snapshot file into node data directory *)
val import_snapshot :
  ?quiet:bool ->
  ?on_log:(string -> unit) ->
  app_bin_dir:string ->
  data_dir:string ->
  snapshot_path:string ->
  no_check:bool ->
  unit ->
  (unit, Rresult.R.msg) result

(** Import snapshot file (wrapper for snapshot_file record) *)
val import_snapshot_file :
  ?quiet:bool ->
  ?on_log:(string -> unit) ->
  app_bin_dir:string ->
  data_dir:string ->
  snapshot_file:snapshot_file ->
  no_check:bool ->
  unit ->
  (unit, Rresult.R.msg) result

(** Execute snapshot plan: download (if needed) and import *)
val perform_snapshot_plan :
  ?quiet:bool ->
  ?on_log:(string -> unit) ->
  ?tmp_dir:string ->
  ?keep_snapshot:bool ->
  plan:snapshot_plan ->
  app_bin_dir:string ->
  data_dir:string ->
  no_check:bool ->
  unit ->
  (unit, Rresult.R.msg) result

(** Perform bootstrap according to node request *)
val perform_bootstrap :
  ?quiet:bool ->
  ?on_log:(string -> unit) ->
  ?tmp_dir:string ->
  plan:snapshot_plan ->
  request:node_request ->
  data_dir:string ->
  unit ->
  (unit, Rresult.R.msg) result
