(******************************************************************************)
(*                                                                            *)
(* SPDX-License-Identifier: MIT                                               *)
(* Copyright (c) 2025 Nomadic Labs <contact@nomadic-labs.com>                 *)
(*                                                                            *)
(******************************************************************************)

(** Matrix driver terminal operations - thin wrapper around Terminal_raw. *)

module Raw = Miaou_driver_common.Terminal_raw

type t = Raw.t

let setup = Raw.setup

let fd = Raw.fd

let enter_raw = Raw.enter_raw

let leave_raw = Raw.leave_raw

let enable_mouse = Raw.enable_mouse

let disable_mouse = Raw.disable_mouse

let cleanup = Raw.cleanup

let write = Raw.write

let size = Raw.size

let invalidate_size_cache = Raw.invalidate_size_cache

let resize_pending = Raw.resize_pending

let clear_resize_pending = Raw.clear_resize_pending

let install_signals t cleanup_fn =
  Raw.install_signals t ~on_resize:(fun () -> ()) ~on_exit:cleanup_fn

let set_exit_screen_dump = Raw.set_exit_screen_dump
