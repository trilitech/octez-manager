(******************************************************************************)
(*                                                                            *)
(* SPDX-License-Identifier: MIT                                               *)
(* Copyright (c) 2026 Nomadic Labs <contact@nomadic-labs.com>                 *)
(*                                                                            *)
(******************************************************************************)

(* Inventory fixture, orphan case: no stanza of dune.fixture lists this
   module, which reproduces a test source silently ignored by dune because
   every stanza declares an explicit (modules) field. The checker must
   report it and exit 1. It is never compiled; the fixture directory is
   read only through check_test_inventory. *)
let orphan = ()
