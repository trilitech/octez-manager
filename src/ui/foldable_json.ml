(******************************************************************************)
(*                                                                            *)
(* SPDX-License-Identifier: MIT                                               *)
(* Copyright (c) 2026 Nomadic Labs <contact@nomadic-labs.com>                 *)
(*                                                                            *)
(******************************************************************************)

module Widgets = Miaou_widgets_display.Widgets
module Style = Miaou_style.Style
module Style_context = Miaou_style.Style_context

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
  mutable line_to_node : (int * node_id) list;
      (* line -> node_id for foldable lines *)
}

(* Convert Yojson to our node type, assigning IDs.
   Returns (node, next_id) where next_id is the next available ID. *)
let rec json_to_node next_id (json : Yojson.Safe.t) : json_node * int =
  match json with
  | `Null -> (JNull, next_id)
  | `Bool b -> (JBool b, next_id)
  | `Int i -> (JInt i, next_id)
  | `Intlit s -> (JInt (int_of_string_opt s |> Option.value ~default:0), next_id)
  | `Float f -> (JFloat f, next_id)
  | `String s -> (JString s, next_id)
  | `List items ->
      let id = next_id in
      let next_id = next_id + 1 in
      let items, next_id =
        List.fold_left
          (fun (acc, nid) item ->
            let node, nid = json_to_node nid item in
            (node :: acc, nid))
          ([], next_id)
          items
      in
      (JArray {id; items = List.rev items}, next_id)
  | `Assoc fields ->
      let id = next_id in
      let next_id = next_id + 1 in
      let fields, next_id =
        List.fold_left
          (fun (acc, nid) (k, v) ->
            let node, nid = json_to_node nid v in
            ((k, node) :: acc, nid))
          ([], next_id)
          fields
      in
      (JObject {id; fields = List.rev fields}, next_id)

let of_json json =
  let root, _ = json_to_node 0 json in
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
  }

let of_string json_str =
  try
    let json = Yojson.Safe.from_string json_str in
    Some (of_json json)
  with _ -> None

let raw t = t.raw_json

let is_folded t id = Hashtbl.find_opt t.folded id |> Option.value ~default:false

(* Render helpers - use same styles as json_highlighter.ml for consistency *)
let indent n = String.make (n * 2) ' '

let render_style style text =
  let resolved = Style.to_resolved style in
  if resolved.r_fg >= 0 then Style.render style text
  else Widgets.themed_text text

let render_string s =
  render_style
    (Style_context.warning ())
    (Printf.sprintf "\"%s\"" (String.escaped s))

let render_key k =
  render_style (Style_context.text_emphasized ()) (Printf.sprintf "\"%s\"" k)

let render_number n = render_style (Style_context.info ()) n

let render_bool b =
  render_style (Style_context.success ()) (if b then "true" else "false")

let render_null () = render_style (Style_context.text_muted ()) "null"

(* Render JSON with folding *)
let render t =
  let buf = Buffer.create 4096 in

  (* State: current_line and accumulated line_map *)
  let rec render_node depth (line, line_map) node =
    match node with
    | JNull ->
        Buffer.add_string buf (render_null ()) ;
        (line, line_map)
    | JBool b ->
        Buffer.add_string buf (render_bool b) ;
        (line, line_map)
    | JInt i ->
        Buffer.add_string buf (render_number (string_of_int i)) ;
        (line, line_map)
    | JFloat f ->
        Buffer.add_string buf (render_number (string_of_float f)) ;
        (line, line_map)
    | JString s ->
        Buffer.add_string buf (render_string s) ;
        (line, line_map)
    | JArray {id; items} ->
        let folded = is_folded t id in
        let count = List.length items in
        if folded then (
          Buffer.add_string
            buf
            (Widgets.themed_muted (Printf.sprintf "[...] (%d items)" count)) ;
          (line, (line, id) :: line_map))
        else if items = [] then (
          Buffer.add_string buf "[]" ;
          (line, line_map))
        else
          let line_map = (line, id) :: line_map in
          Buffer.add_string buf "[\n" ;
          let line = line + 1 in
          let line, line_map =
            List.fold_left
              (fun (line, line_map) (i, item) ->
                Buffer.add_string buf (indent (depth + 1)) ;
                let line, line_map =
                  render_node (depth + 1) (line, line_map) item
                in
                if i < count - 1 then Buffer.add_string buf "," ;
                Buffer.add_char buf '\n' ;
                (line + 1, line_map))
              (line, line_map)
              (List.mapi (fun i x -> (i, x)) items)
          in
          Buffer.add_string buf (indent depth) ;
          Buffer.add_string buf "]" ;
          (line, line_map)
    | JObject {id; fields} ->
        let folded = is_folded t id in
        let count = List.length fields in
        if folded then (
          Buffer.add_string
            buf
            (Widgets.themed_muted (Printf.sprintf "{...} (%d fields)" count)) ;
          (line, (line, id) :: line_map))
        else if fields = [] then (
          Buffer.add_string buf "{}" ;
          (line, line_map))
        else
          let line_map = (line, id) :: line_map in
          Buffer.add_string buf "{\n" ;
          let line = line + 1 in
          let line, line_map =
            List.fold_left
              (fun (line, line_map) (i, (key, value)) ->
                Buffer.add_string buf (indent (depth + 1)) ;
                Buffer.add_string buf (render_key key) ;
                Buffer.add_string buf ": " ;
                let line, line_map =
                  render_node (depth + 1) (line, line_map) value
                in
                if i < count - 1 then Buffer.add_string buf "," ;
                Buffer.add_char buf '\n' ;
                (line + 1, line_map))
              (line, line_map)
              (List.mapi (fun i x -> (i, x)) fields)
          in
          Buffer.add_string buf (indent depth) ;
          Buffer.add_string buf "}" ;
          (line, line_map)
  in
  let _, line_map = render_node 0 (0, []) t.root in
  t.line_to_node <- List.rev line_map ;
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
