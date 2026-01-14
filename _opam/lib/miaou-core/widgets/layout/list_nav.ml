(*****************************************************************************)
(*                                                                           *)
(* SPDX-License-Identifier: MIT                                              *)
(* Copyright (c) 2025 Nomadic Labs <contact@nomadic-labs.com>                *)
(*                                                                           *)
(*****************************************************************************)
let clamp lo hi x = max lo (min hi x)

let move_cursor ~total ~cursor ~delta =
  let max_idx = max 0 (total - 1) in
  clamp 0 max_idx (cursor + delta)

let page_move ~total ~cursor ~page_size ~dir =
  let delta = match dir with `Up -> -page_size | `Down -> page_size in
  move_cursor ~total ~cursor ~delta
