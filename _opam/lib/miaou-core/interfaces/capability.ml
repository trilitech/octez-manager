(*****************************************************************************)
(*                                                                           *)
(* SPDX-License-Identifier: MIT                                              *)
(* Copyright (c) 2025 Nomadic Labs <contact@nomadic-labs.com>                *)
(*                                                                           *)
(*****************************************************************************)
(* Copyright (c) 2025 Nomadic Labs <contact@nomadic-labs.com> *)

[@@@warning "-32-34-37-69"]

type 'a key = {id : int; name : string}

let next_id = ref 0

let fresh_id () =
  let v = !next_id in
  incr next_id ;
  v

let create ~name : 'a key = {id = fresh_id (); name}

module IntMap = Map.Make (Int)

let store : Obj.t IntMap.t ref = ref IntMap.empty

let names : string IntMap.t ref = ref IntMap.empty

let set (type a) (k : a key) (v : a) : unit =
  store := IntMap.add k.id (Obj.repr v) !store ;
  names := IntMap.add k.id k.name !names

let register = set

let get (type a) (k : a key) : a option =
  match IntMap.find_opt k.id !store with
  | None -> None
  | Some o -> Some (Obj.obj o : a)

let require k =
  match get k with
  | Some v -> v
  | None ->
      let bt = try Printexc.get_backtrace () with _ -> "(no backtrace)" in
      failwith
        (Printf.sprintf "capability missing: %s\nbacktrace:\n%s" k.name bt)

let mem k = IntMap.mem k.id !store

let clear () =
  store := IntMap.empty ;
  names := IntMap.empty

let list () =
  let all =
    IntMap.fold
      (fun id name acc -> (name, IntMap.mem id !store) :: acc)
      !names
      []
  in
  List.rev all

type any = Any : 'a key -> any

let any k = Any k

let check_all lst =
  let missing = ref [] in
  List.iter
    (fun (Any k) -> if not (mem k) then missing := k.name :: !missing)
    lst ;
  List.rev !missing
