(******************************************************************************)
(*                                                                            *)
(* SPDX-License-Identifier: MIT                                               *)
(* Copyright (c) 2026 Nomadic Labs <contact@nomadic-labs.com>                 *)
(*                                                                            *)
(******************************************************************************)

(** Eio-based process execution.

    This module provides non-blocking process execution using
    [Eio.Process]. Call {!init} at TUI startup to register the Eio
    implementations as hooks in {!Cmd_runner} and {!Download}.

    In CLI mode (when {!init} is not called), {!Cmd_runner} and
    {!Download} use their default blocking Unix implementations. *)

(** Existential wrapper for the Eio process manager. *)
type any_proc_mgr = Mgr : _ Eio.Process.mgr -> any_proc_mgr

(** Return the process manager set by {!init}, or [None] in CLI mode. *)
val get_process_mgr : unit -> any_proc_mgr option

(** Initialize Eio-based process execution.

    Registers Eio implementations for [Cmd_runner.run],
    [Cmd_runner.run_out], [Cmd_runner.run_out_with_timeout],
    [Cmd_runner.run_out_silent], [Cmd_runner.run_streaming], and
    [Download.download_file_with_progress].

    Must be called from within an Eio fiber context (i.e., inside
    [Eio_posix.run] or similar). *)
val init : _ Eio.Process.mgr -> unit
