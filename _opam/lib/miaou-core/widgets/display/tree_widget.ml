(*****************************************************************************)
(*                                                                           *)
(* SPDX-License-Identifier: MIT                                              *)
(* Copyright (c) 2025 Nomadic Labs <contact@nomadic-labs.com>                *)
(*                                                                           *)
(*****************************************************************************)
[@@@warning "-32-34-37-69"]

type node = {label : string; children : node list}

type t = {root : node; cursor_path : int list}

let rec json_to_node j =
  match j with
  | `Assoc kvs ->
      let children =
        List.fold_left
          (fun acc (k, v) -> {label = k; children = [json_to_node v]} :: acc)
          []
          kvs
      in
      {label = "obj"; children = List.rev children}
  | `List lst -> {label = "list"; children = List.map json_to_node lst}
  | other -> {label = Yojson.Safe.to_string other; children = []}

let of_json j = json_to_node j

let open_root node = {root = node; cursor_path = [0]}

let handle_key t ~key:_ = t

let render_node indent n =
  let buf = Buffer.create 64 in
  let rec render_into buf indent n =
    Buffer.add_string buf (String.make indent ' ') ;
    Buffer.add_string buf n.label ;
    match n.children with
    | [] -> ()
    | children ->
        Buffer.add_char buf '\n' ;
        List.iteri
          (fun idx child ->
            if idx > 0 then Buffer.add_char buf '\n' ;
            render_into buf (indent + 2) child)
          children
  in
  render_into buf indent n ;
  Buffer.contents buf

let render t ~focus:_ = render_node 0 t.root
