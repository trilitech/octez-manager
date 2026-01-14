(*****************************************************************************)
(*                                                                           *)
(* SPDX-License-Identifier: MIT                                              *)
(* Copyright (c) 2025 Nomadic Labs <contact@nomadic-labs.com>                *)
(*                                                                           *)
(*****************************************************************************)

[@@@warning "-32-34-37-69"]

type hint = {short : string option; long : string option}

(* Maintain a stack of hints. Top-most (head) takes precedence. *)
let stack : hint list ref = ref []

let normalize (s : string option) : hint option =
  match s with None -> None | Some s -> Some {short = Some s; long = None}

let set (s : string option) : unit =
  match normalize s with None -> stack := [] | Some h -> stack := [h]

let clear () = stack := []

let push ?short ?long () : unit =
  let h = {short; long} in
  stack := h :: !stack

let pop () : unit = match !stack with [] -> () | _ :: tl -> stack := tl

let get_active () : hint option =
  match !stack with h :: _ -> Some h | [] -> None

(* Back-compat helper: return short variant only. *)
let get () : string option =
  match get_active () with None -> None | Some {short; long = _} -> short
