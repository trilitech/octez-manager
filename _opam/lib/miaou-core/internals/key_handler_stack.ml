(*****************************************************************************)
(*                                                                           *)
(* SPDX-License-Identifier: MIT                                              *)
(* Copyright (c) 2025 Nomadic Labs <contact@nomadic-labs.com>                *)
(*                                                                           *)
(*****************************************************************************)
[@@@warning "-32-34-37-69"]

type action = unit -> unit

type binding = {action : action option; help : string; display_only : bool}

type frame = {
  id : int;
  delegate : bool;
      (* If true, keys not handled here bubble down to lower frames *)
  bindings : (string, binding) Hashtbl.t;
}

type t = {frames : frame list; next_id : int}

let empty = {frames = []; next_id = 0}

type handle = int

let push st ?(delegate = true) bindings =
  let id = st.next_id in
  let tbl = Hashtbl.create 17 in
  List.iter (fun (k, b) -> Hashtbl.replace tbl k b) bindings ;
  let fr = {id; delegate; bindings = tbl} in
  ({frames = fr :: st.frames; next_id = id + 1}, id)

let pop st h =
  let rec remove = function
    | [] -> []
    | f :: rest when f.id = h -> rest
    | f :: rest -> f :: remove rest
  in
  {st with frames = remove st.frames}

let pop_top st =
  match st.frames with [] -> st | _ :: rest -> {st with frames = rest}

let clear st = {st with frames = []}

let depth st = List.length st.frames

let top_keys st =
  match st.frames with
  | [] -> []
  | f :: _ -> Hashtbl.fold (fun k _ acc -> k :: acc) f.bindings []

let top_bindings st =
  match st.frames with
  | [] -> []
  | f :: _ -> Hashtbl.fold (fun k b acc -> (k, b.help) :: acc) f.bindings []

let all_bindings st =
  let rec gather acc = function
    | [] -> acc
    | f :: rest ->
        let pairs =
          Hashtbl.fold (fun k b acc -> (k, b.help) :: acc) f.bindings []
        in
        gather (pairs @ acc) rest
  in
  (* Top-first ordering: start with empty acc, then append lower frames at end *)
  List.rev (gather [] st.frames)

let dispatch st key =
  let rec loop = function
    | [] -> (false, st)
    | f :: rest -> (
        match Hashtbl.find_opt f.bindings key with
        | Some b -> (
            match b.action with
            | Some a when not b.display_only ->
                a () ;
                (true, st)
            | _ -> if f.delegate then loop rest else (false, st))
        | None -> if f.delegate then loop rest else (false, st))
  in
  loop st.frames
