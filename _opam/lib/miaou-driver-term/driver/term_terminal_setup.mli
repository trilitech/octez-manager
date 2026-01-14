(*****************************************************************************)
(*                                                                           *)
(* SPDX-License-Identifier: MIT                                              *)
(* Copyright (c) 2025 Nomadic Labs <contact@nomadic-labs.com>                *)
(*                                                                           *)
(*****************************************************************************)

(** Returns (fd, enter_raw, cleanup, install_signal_handlers, signal_exit_flag) *)
val setup_and_cleanup :
  unit ->
  Unix.file_descr
  * (unit -> unit)
  * (unit -> unit)
  * (unit -> unit)
  * bool Atomic.t
