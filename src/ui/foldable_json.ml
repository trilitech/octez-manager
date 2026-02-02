(******************************************************************************)
(*                                                                            *)
(* SPDX-License-Identifier: MIT                                               *)
(* Copyright (c) 2026 Nomadic Labs <contact@nomadic-labs.com>                 *)
(*                                                                            *)
(******************************************************************************)

module Widgets = Miaou_widgets_display.Widgets

(* Node ID for tracking fold state *)
type node_id = int

(* JSON node with fold tracking *)
type json_node =
  | JNull
  | JBool of bool
  | JInt of int
  | JFloat of float
  | JString of string
  | JArray of {id : node_id; items : json_node list}
  | JObject of {id : node_id; fields : (string * json_node) list}

type t = {
  root : json_node;
  raw_json : string;
  folded : (node_id, bool) Hashtbl.t;
  mutable line_to_node : (int * node_id) list;  (* line -> node_id for foldable lines *)
  next_id : int ref;
}

let new_id t =
  let id = !(t.next_id) in
  t.next_id := id + 1 ;
  id

(* Convert Yojson to our node type, assigning IDs *)
let rec json_to_node next_id (json : Yojson.Safe.t) : json_node =
  match json with
  | `Null -> JNull
  | `Bool b -> JBool b
  | `Int i -> JInt i
  | `Intlit s -> JInt (int_of_string_opt s |> Option.value ~default:0)
  | `Float f -> JFloat f
  | `String s -> JString s
  | `List items ->
      let id = !next_id in
      next_id := !next_id + 1 ;
      JArray {id; items = List.map (json_to_node next_id) items}
  | `Assoc fields ->
      let id = !next_id in
      next_id := !next_id + 1 ;
      JObject {id; fields = List.map (fun (k, v) -> (k, json_to_node next_id v)) fields}
  | `Tuple items ->
      let id = !next_id in
      next_id := !next_id + 1 ;
      JArray {id; items = List.map (json_to_node next_id) items}
  | `Variant (name, opt) ->
      let id = !next_id in
      next_id := !next_id + 1 ;
      let items = match opt with None -> [] | Some v -> [json_to_node next_id v] in
      JObject {id; fields = [(name, JArray {id = !next_id; items})]}

let of_json json =
  let next_id = ref 0 in
  let root = json_to_node next_id json in
  let folded = Hashtbl.create 64 in
  (* Fold all nodes by default *)
  let rec fold_all_nodes node =
    match node with
    | JArray {id; items} ->
        Hashtbl.replace folded id true ;
        List.iter fold_all_nodes items
    | JObject {id; fields} ->
        Hashtbl.replace folded id true ;
        List.iter (fun (_, v) -> fold_all_nodes v) fields
    | _ -> ()
  in
  fold_all_nodes root ;
  (* Unfold root if it's an object/array *)
  (match root with
  | JArray {id; _} | JObject {id; _} -> Hashtbl.replace folded id false
  | _ -> ()) ;
  {
    root;
    raw_json = Yojson.Safe.pretty_to_string json;
    folded;
    line_to_node = [];
    next_id;
  }

let of_string json_str =
  try
    let json = Yojson.Safe.from_string json_str in
    Some (of_json json)
  with _ -> None

let raw t = t.raw_json

let is_folded t id = Hashtbl.find_opt t.folded id |> Option.value ~default:false

(* Render helpers *)
let indent n = String.make (n * 2) ' '

let render_string s = Widgets.yellow (Printf.sprintf "\"%s\"" (String.escaped s))
let render_key k = Widgets.fg 14 (Printf.sprintf "\"%s\"" k)
let render_number n = Widgets.magenta n
let render_bool b = Widgets.green (if b then "true" else "false")
let render_null () = Widgets.dim "null"

let fold_indicator is_folded count kind =
  if is_folded then
    let bracket = if kind = `Array then "[]" else "{}" in
    Widgets.dim (Printf.sprintf " %s (%d items)" bracket count)
  else ""

(* Render JSON with folding *)
let render t =
  let buf = Buffer.create 4096 in
  let line_map = ref [] in
  let current_line = ref 0 in

  let add_line s =
    Buffer.add_string buf s ;
    Buffer.add_char buf '\n' ;
    incr current_line
  in

  let add_inline s = Buffer.add_string buf s in

  let rec render_node depth node =
    match node with
    | JNull -> add_inline (render_null ())
    | JBool b -> add_inline (render_bool b)
    | JInt i -> add_inline (render_number (string_of_int i))
    | JFloat f -> add_inline (render_number (string_of_float f))
    | JString s -> add_inline (render_string s)
    | JArray {id; items} ->
        let folded = is_folded t id in
        let count = List.length items in
        if folded then (
          line_map := (!current_line, id) :: !line_map ;
          add_inline (Widgets.dim (Printf.sprintf "[...] (%d items)" count)))
        else if items = [] then
          add_inline "[]"
        else (
          line_map := (!current_line, id) :: !line_map ;
          add_line "[" ;
          List.iteri
            (fun i item ->
              add_inline (indent (depth + 1)) ;
              render_node (depth + 1) item ;
              if i < count - 1 then add_inline "," ;
              add_line "")
            items ;
          add_inline (indent depth) ;
          add_inline "]")
    | JObject {id; fields} ->
        let folded = is_folded t id in
        let count = List.length fields in
        if folded then (
          line_map := (!current_line, id) :: !line_map ;
          add_inline (Widgets.dim (Printf.sprintf "{...} (%d fields)" count)))
        else if fields = [] then
          add_inline "{}"
        else (
          line_map := (!current_line, id) :: !line_map ;
          add_line "{" ;
          List.iteri
            (fun i (key, value) ->
              add_inline (indent (depth + 1)) ;
              add_inline (render_key key) ;
              add_inline ": " ;
              render_node (depth + 1) value ;
              if i < count - 1 then add_inline "," ;
              add_line "")
            fields ;
          add_inline (indent depth) ;
          add_inline "}")
  in
  render_node 0 t.root ;
  t.line_to_node <- List.rev !line_map ;
  Buffer.contents buf

let toggle_fold_at_line t ~line =
  match List.find_opt (fun (l, _) -> l = line) t.line_to_node with
  | Some (_, id) ->
      let currently_folded = is_folded t id in
      Hashtbl.replace t.folded id (not currently_folded) ;
      t
  | None -> t

let unfold_all t =
  Hashtbl.iter (fun id _ -> Hashtbl.replace t.folded id false) t.folded ;
  t

let fold_all t =
  Hashtbl.iter (fun id _ -> Hashtbl.replace t.folded id true) t.folded ;
  (* Unfold root *)
  (match t.root with
  | JArray {id; _} | JObject {id; _} -> Hashtbl.replace t.folded id false
  | _ -> ()) ;
  t

let line_count t =
  let rendered = render t in
  List.length (String.split_on_char '\n' rendered)

let is_foldable_line t ~line =
  List.exists (fun (l, _) -> l = line) t.line_to_node
