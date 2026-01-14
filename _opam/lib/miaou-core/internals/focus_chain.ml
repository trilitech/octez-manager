(*****************************************************************************)
(*                                                                           *)
(* SPDX-License-Identifier: MIT                                              *)
(* Copyright (c) 2025 Nomadic Labs <contact@nomadic-labs.com>                *)
(*                                                                           *)
(*****************************************************************************)

type t = {active : int; total : int}

let create ~total = {active = 0; total = max 0 total}

let with_total t total =
  let total = max 0 total in
  let active = if total = 0 then 0 else min t.active (total - 1) in
  {active; total}

let current t = if t.total = 0 then None else Some t.active

let move t dir =
  match t.total with
  | 0 -> (t, `Bubble)
  | total ->
      let next =
        match dir with
        | `Next -> (t.active + 1) mod total
        | `Prev -> (t.active - 1 + total) mod total
      in
      ({t with active = next}, `Handled)

let handle_key t ~key =
  match key with
  | "Tab" -> move t `Next
  | "S-Tab" | "BackTab" -> move t `Prev
  | _ -> (t, `Bubble)
