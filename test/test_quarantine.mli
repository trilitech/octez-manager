(******************************************************************************)
(*                                                                            *)
(* SPDX-License-Identifier: MIT                                               *)
(* Copyright (c) 2026 Nomadic Labs <contact@nomadic-labs.com>                 *)
(*                                                                            *)
(******************************************************************************)

(** Inverted quarantine for Alcotest cases that are known to fail.

    A quarantined case must keep failing: it passes while the wrapped test
    raises, and fails once the wrapped test passes, so that the wrapper gets
    removed together with the fix. Every wrapper cites the tracking issue. *)

(** [known_failure ~issue ~expect test] runs [test] and inverts its outcome.
    [issue] is a [trilitech/octez-manager] issue number. The case passes only
    if [test] fails on an Alcotest check whose message contains [expect]. It
    fails if [test] passes, or fails on another check; any other exception (a
    crash, a [failwith], a setup error) propagates. *)
val known_failure : issue:int -> expect:string -> (unit -> unit) -> unit -> unit
