(*****************************************************************************)
(*                                                                           *)
(* SPDX-License-Identifier: MIT                                              *)
(* Copyright (c) 2025 Nomadic Labs <contact@nomadic-labs.com>                *)
(*                                                                           *)
(*****************************************************************************)
type page = (module Tui_page.PAGE_SIG)

let table : (string, page) Hashtbl.t = Hashtbl.create 8

let lazy_table : (string, unit -> page) Hashtbl.t = Hashtbl.create 8

(* Validate that a page doesn't use reserved global keys *)
let validate_page_keys name (module P : Tui_page.PAGE_SIG) =
  let handled = P.handled_keys () in
  let conflicts = List.filter Keys.is_global_key handled in
  if conflicts <> [] then
    let conflict_str = String.concat ", " (List.map Keys.to_label conflicts) in
    failwith
      (Printf.sprintf
         "Page '%s' attempts to handle reserved global keys: %s. Global keys \
          are reserved for application-wide functionality."
         name
         conflict_str)

let register name (p : page) =
  validate_page_keys name p ;
  Hashtbl.replace table name p

let register_once name (p : page) : bool =
  if Hashtbl.mem table name then false
  else (
    validate_page_keys name p ;
    Hashtbl.add table name p ;
    true)

let list () =
  let items = Hashtbl.fold (fun k v acc -> (k, v) :: acc) table [] in
  List.sort (fun (a, _) (b, _) -> compare a b) items

let exists name = Hashtbl.mem table name

let unregister name = Hashtbl.remove table name

let list_names () =
  Hashtbl.fold (fun k _ acc -> k :: acc) table [] |> List.sort compare

let register_lazy name thunk = Hashtbl.replace lazy_table name thunk

let override name p = Hashtbl.replace table name p

(* Check for key conflicts between all registered pages *)
let check_all_conflicts () =
  let all_pages = list () in
  let page_keys =
    List.map
      (fun (name, (module P : Tui_page.PAGE_SIG)) -> (name, P.handled_keys ()))
      all_pages
  in
  (* Build a map of key -> list of pages that handle it *)
  let key_map = Hashtbl.create 16 in
  List.iter
    (fun (page_name, keys) ->
      List.iter
        (fun key ->
          let key_str = Keys.to_string key in
          let pages =
            match Hashtbl.find_opt key_map key_str with
            | Some ps -> page_name :: ps
            | None -> [page_name]
          in
          Hashtbl.replace key_map key_str pages)
        keys)
    page_keys ;
  (* Find conflicts (keys handled by multiple pages) *)
  let conflicts = ref [] in
  Hashtbl.iter
    (fun key pages ->
      if List.length pages > 1 then
        conflicts := (key, List.sort String.compare pages) :: !conflicts)
    key_map ;
  !conflicts

(* Get a human-readable report of all key conflicts *)
let conflict_report () =
  let conflicts = check_all_conflicts () in
  if conflicts = [] then None
  else
    let lines =
      List.map
        (fun (key, pages) ->
          Printf.sprintf
            "  Key '%s' handled by: %s"
            key
            (String.concat ", " pages))
        (List.sort (fun (a, _) (b, _) -> String.compare a b) conflicts)
    in
    Some
      (Printf.sprintf "Key conflicts detected:\n%s" (String.concat "\n" lines))

(* Resolve lazily on demand: if not in table, try lazy_table and populate table *)
let find name =
  match Hashtbl.find_opt table name with
  | Some p -> Some p
  | None -> (
      match Hashtbl.find_opt lazy_table name with
      | None -> None
      | Some f ->
          let p = f () in
          Hashtbl.replace table name p ;
          Some p)

(* Application-specific helpers (moved out).

	Note: functions to store a last selected instance are application-specific
	and were intentionally removed from this core Registry. Consumers that need
	such functionality should implement it in their application-specific modules
	(see `src/app_specific/registry.ml`). *)
