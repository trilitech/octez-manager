(******************************************************************************)
(*                                                                            *)
(* SPDX-License-Identifier: MIT                                               *)
(* Copyright (c) 2025 Nomadic Labs <contact@nomadic-labs.com>                 *)
(*                                                                            *)
(******************************************************************************)

val write_pairs :
  inst:string -> (string * string) list -> (unit, Rresult.R.msg) result

val read : inst:string -> ((string * string) list, Rresult.R.msg) result

val write :
  inst:string ->
  data_dir:string ->
  run_args:string ->
  extra_env:(string * string) list ->
  (unit, Rresult.R.msg) result
