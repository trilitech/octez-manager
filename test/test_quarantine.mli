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

(** [known_failure ~issue test] runs [test] and inverts its outcome. [issue] is
    a [trilitech/octez-manager] issue number. Any exception other than
    [Stack_overflow] or [Out_of_memory] counts as the known failure: Alcotest
    does not expose its assertion exception, so a crash cannot be told apart
    from an assertion here. *)
val known_failure : issue:int -> (unit -> unit) -> unit -> unit
