(******************************************************************************)
(*                                                                            *)
(* SPDX-License-Identifier: MIT                                               *)
(* Copyright (c) 2025 Nomadic Labs <contact@nomadic-labs.com>                 *)
(*                                                                            *)
(******************************************************************************)

val write_pairs :
  inst:string -> (string * string) list -> (unit, Rresult.R.msg) result

(** [read_from_disk ~inst] reads the environment file for an instance.
    WARNING: Performs file I/O. For render-safe access, use {!Render_data}. *)
val read_from_disk :
  inst:string -> ((string * string) list, Rresult.R.msg) result

val write :
  inst:string ->
  data_dir:string ->
  run_args:string ->
  extra_env:(string * string) list ->
  (unit, Rresult.R.msg) result
