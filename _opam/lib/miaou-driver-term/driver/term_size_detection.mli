(*****************************************************************************)
(*                                                                           *)
(* SPDX-License-Identifier: MIT                                              *)
(* Copyright (c) 2025 Nomadic Labs <contact@nomadic-labs.com>                *)
(*                                                                           *)
(*****************************************************************************)

(** Portable terminal size detection. Tries multiple methods in priority order:
    1. Environment variables (MIAOU_TUI_ROWS, MIAOU_TUI_COLS)
    2. Lambda-term direct query
    3. stty size
    4. tput
    5. stty -a parsing
    Falls back to 24x80 if all methods fail.

    Results are cached to avoid spawning subprocesses on every render.
    Call [invalidate_cache] on SIGWINCH to pick up terminal resize. *)
val detect_size : unit -> LTerm_geom.size

(** Invalidate the cached terminal size. Call this on SIGWINCH. *)
val invalidate_cache : unit -> unit
