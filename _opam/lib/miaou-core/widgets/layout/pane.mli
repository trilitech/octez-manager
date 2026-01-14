(*****************************************************************************)
(*                                                                           *)
(* SPDX-License-Identifier: MIT                                              *)
(* Copyright (c) 2025 Nomadic Labs <contact@nomadic-labs.com>                *)
(*                                                                           *)
(*****************************************************************************)
(* Pane splitting helpers for text UIs (pure renderer) *)

val split_vertical :
  width:int ->
  left_pad:int ->
  right_pad:int ->
  border:bool ->
  wrap:bool ->
  sep:string ->
  left:string ->
  right:string ->
  string

val split_vertical_with_left_width :
  width:int ->
  left_pad:int ->
  right_pad:int ->
  border:bool ->
  wrap:bool ->
  sep:string ->
  left:string ->
  right:string ->
  left_width:int ->
  string

val split_horizontal :
  height:int ->
  top_pad:int ->
  bottom_pad:int ->
  border:bool ->
  wrap:bool ->
  sep:string ->
  top:string ->
  bottom:string ->
  string
