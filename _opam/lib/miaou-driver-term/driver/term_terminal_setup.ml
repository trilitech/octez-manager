(******************************************************************************)
(*                                                                            *)
(* SPDX-License-Identifier: MIT                                               *)
(* Copyright (c) 2025 Nomadic Labs <contact@nomadic-labs.com>                 *)
(*                                                                            *)
(******************************************************************************)

(** Terminal setup for lambda-term driver - thin wrapper around Terminal_raw. *)

module Raw = Miaou_driver_common.Terminal_raw

(* Keep the same interface for backward compatibility with lambda_term_driver.ml *)
let setup_and_cleanup () =
  let t = Raw.setup () in
  let fd = Raw.fd t in
  let enter_raw () = Raw.enter_raw t in
  let cleanup () = Raw.cleanup t in
  let signal_exit_flag =
    Raw.install_signals
      t
      ~on_resize:(fun () -> ())
      ~on_exit:(fun () -> Raw.cleanup t)
  in
  let install_signal_handlers () =
    (* Signals already installed by Raw.install_signals, this is a no-op *)
    ()
  in
  (fd, enter_raw, cleanup, install_signal_handlers, signal_exit_flag)
