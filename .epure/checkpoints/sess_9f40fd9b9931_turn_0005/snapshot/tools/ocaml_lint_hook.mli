(******************************************************************************)
(*                                                                            *)
(* SPDX-License-Identifier: MIT                                               *)
(* Copyright (c) 2026 Nomadic Labs <contact@nomadic-labs.com>                 *)
(*                                                                            *)
(******************************************************************************)

(** OCaml lint hook for the Épure validation pipeline.

    Runs [dune fmt --check] followed by [dune build] as pre-build checks.
    On the first failure the hook short-circuits and returns an error,
    blocking the build from proceeding. *)

type hook_error =
  | Fmt_check_failed of int  (** [dune fmt --check] exited with this code *)
  | Build_failed of int  (** [dune build] exited with this code *)

(** [run_hook ~run_command ()] executes the pre-build lint checks.

    [run_command cmd] must execute [cmd] and return its exit code.
    Invokes [dune fmt --check] first; if that succeeds, invokes [dune build].
    Returns [Ok ()] when both pass, or [Error e] on the first failure. *)
val run_hook : run_command:(string -> int) -> unit -> (unit, hook_error) result

(** [exit_code_of_result r] returns [0] for [Ok ()] and [1] for [Error _]. *)
val exit_code_of_result : (unit, hook_error) result -> int
