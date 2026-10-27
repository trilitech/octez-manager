(******************************************************************************)
(*                                                                            *)
(* SPDX-License-Identifier: MIT                                               *)
(* Copyright (c) 2026 Nomadic Labs <contact@nomadic-labs.com>                 *)
(*                                                                            *)
(******************************************************************************)

(* Inventory fixture, orphan case: this module is listed in the (modules)
   field of the fixture's only stanza, so the checker must accept it. The
   sibling t_e.ml is listed nowhere and must be reported as an orphan
   source. Neither file is compiled: the fixture directory has no dune file
   of its own, only dune.fixture, which the checker reads as data. *)
let attached = ()
