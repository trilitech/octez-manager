(*****************************************************************************)
(*                                                                           *)
(* SPDX-License-Identifier: MIT                                              *)
(* Copyright (c) 2025 Nomadic Labs <contact@nomadic-labs.com>                *)
(*                                                                           *)
(*****************************************************************************)
val render :
  size:LTerm_geom.size ->
  header:string list ->
  content_footer:string list ->
  child:(LTerm_geom.size -> string) ->
  string
