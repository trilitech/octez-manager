(******************************************************************************)
(*                                                                            *)
(* SPDX-License-Identifier: MIT                                               *)
(* Copyright (c) 2026 Nomadic Labs <contact@nomadic-labs.com>                 *)
(*                                                                            *)
(******************************************************************************)

open Octez_manager_lib

type describe_source = [`Describe | `None]

type entry_kind = Sub | Get | Dyn of string

type entry = {name : string; kind : entry_kind}

(* Cache with TTL *)
type cache_entry = {
  entries : entry list;
  source : describe_source;
  timestamp : float;
}

let describe_cache : (string * string, cache_entry) Hashtbl.t =
  Hashtbl.create 97

let cache_mutex = Mutex.create ()

let cache_ttl = 4.0

let clear_cache () =
  Mutex.lock cache_mutex ;
  Hashtbl.clear describe_cache ;
  Mutex.unlock cache_mutex

let get_cached ~rpc_addr ~segs =
  let key = (rpc_addr, String.concat "/" segs) in
  Mutex.lock cache_mutex ;
  let result =
    match Hashtbl.find_opt describe_cache key with
    | Some {entries; source; timestamp}
      when Unix.gettimeofday () -. timestamp < cache_ttl ->
        Some (entries, source)
    | Some _ ->
        Hashtbl.remove describe_cache key ;
        None
    | None -> None
  in
  Mutex.unlock cache_mutex ;
  result

let cache_put ~rpc_addr ~segs ~entries ~source =
  let key = (rpc_addr, String.concat "/" segs) in
  Mutex.lock cache_mutex ;
  Hashtbl.replace
    describe_cache
    key
    {entries; source; timestamp = Unix.gettimeofday ()} ;
  Mutex.unlock cache_mutex

let candidate_paths segs =
  if segs = [] then ["/describe?recurse=yes"]
  else
    let joined = "/" ^ String.concat "/" segs in
    [
      "/describe" ^ joined ^ "?recurse=yes";
      (* prefix form *)
      joined ^ "/describe?recurse=yes";
      (* suffix form *)
    ]

let parse_describe_json (j : Yojson.Safe.t) : entry list =
  (* Helper to extract string from JSON *)
  let get_string key kvs =
    match List.assoc_opt key kvs with Some (`String s) -> Some s | _ -> None
  in
  (* Check for GET service - returns entry if found *)
  let check_get_service kvs =
    match List.assoc_opt "static" kvs with
    | Some (`Assoc stat) -> (
        match List.assoc_opt "get_service" stat with
        | Some (`Assoc _) -> [{name = ""; kind = Get}]
        | _ -> [])
    | _ -> []
  in
  (* Parse subdirs for suffixes and dynamic_dispatch - returns entries *)
  let parse_subdirs subdirs_json =
    match subdirs_json with
    | `Assoc kv ->
        (* Static suffixes *)
        let suffix_entries =
          match List.assoc_opt "suffixes" kv with
          | Some (`List items) ->
              List.filter_map
                (fun item ->
                  match item with
                  | `Assoc item_kv -> (
                      match get_string "name" item_kv with
                      | Some name -> Some {name; kind = Sub}
                      | None -> None)
                  | _ -> None)
                items
          | _ -> []
        in
        (* Dynamic dispatch *)
        let dyn_entries =
          match List.assoc_opt "dynamic_dispatch" kv with
          | Some (`Assoc dd) -> (
              match List.assoc_opt "arg" dd with
              | Some (`Assoc arg) ->
                  let arg_name =
                    match get_string "name" arg with
                    | Some n -> n
                    | None -> "value"
                  in
                  [{name = "<" ^ arg_name ^ ">"; kind = Dyn arg_name}]
              | _ -> [])
          | _ -> []
        in
        suffix_entries @ dyn_entries
    | _ -> []
  in
  (* Parse the JSON structure *)
  match j with
  | `Assoc kvs ->
      let get_entries = check_get_service kvs in
      let static_subdir_entries =
        match List.assoc_opt "static" kvs with
        | Some (`Assoc stat) -> (
            match List.assoc_opt "subdirs" stat with
            | Some subdirs -> parse_subdirs subdirs
            | None -> [])
        | _ -> []
      in
      let top_subdir_entries =
        match List.assoc_opt "subdirs" kvs with
        | Some subdirs -> parse_subdirs subdirs
        | None -> []
      in
      get_entries @ static_subdir_entries @ top_subdir_entries
  | _ -> []

let parse_description (j : Yojson.Safe.t) : string option =
  match j with
  | `Assoc kvs -> (
      match List.assoc_opt "static" kvs with
      | Some (`Assoc stat) -> (
          match List.assoc_opt "get_service" stat with
          | Some (`Assoc gs) -> (
              match List.assoc_opt "description" gs with
              | Some (`String d) -> Some d
              | _ -> None)
          | _ -> None)
      | _ -> None)
  | _ -> None

let fetch_entries_uncached (s : Service.t) ~segs =
  let paths = candidate_paths segs in
  let rec try_paths = function
    | [] -> ([], `None)
    | p :: ps -> (
        match Rpc_client.http_get_string s p with
        | Error _ -> try_paths ps
        | Ok body -> (
            try
              let j = Yojson.Safe.from_string body in
              let entries = parse_describe_json j in
              if entries <> [] then (entries, `Describe) else try_paths ps
            with _ -> try_paths ps))
  in
  try_paths paths

(** Convert OpenAPI entries to our entry format.
    OpenAPI returns alternating [name; kind; name; kind; ...] *)
let entries_from_openapi openapi_entries =
  let rec pairs acc = function
    | [] -> List.rev acc
    | name :: kind :: rest ->
        let entry =
          match kind with
          | "__SUB__" -> Some {name; kind = Sub}
          | "__GET__" -> Some {name = ""; kind = Get} (* GET at current path *)
          | "__DYN__" ->
              (* Extract type from name like "<block_id>" *)
              let typ =
                if String.length name > 2 && name.[0] = '<' then
                  String.sub name 1 (String.length name - 2)
                else "value"
              in
              Some {name; kind = Dyn typ}
          | _ -> None
        in
        pairs (match entry with Some e -> e :: acc | None -> acc) rest
    | _ -> List.rev acc
  in
  pairs [] openapi_entries

let fetch_entries (s : Service.t) ~segs =
  let rpc_addr_s = Rpc_addr.to_string s.rpc_addr in
  match get_cached ~rpc_addr:rpc_addr_s ~segs with
  | Some (entries, source) -> (entries, source)
  | None ->
      (* Try OpenAPI first (works for public nodes without /describe) *)
      let openapi_entries = Rpc_openapi.entries_for ~segs in
      if openapi_entries <> [] then (
        let entries = entries_from_openapi openapi_entries in
        cache_put ~rpc_addr:rpc_addr_s ~segs ~entries ~source:`None ;
        (entries, `None))
      else
        (* Fall back to describe endpoint *)
        let entries, source = fetch_entries_uncached s ~segs in
        cache_put ~rpc_addr:rpc_addr_s ~segs ~entries ~source ;
        (entries, source)

let fetch_description (s : Service.t) ~segs =
  let paths = candidate_paths segs in
  let rec try_paths = function
    | [] -> None
    | p :: ps -> (
        match Rpc_client.http_get_string s p with
        | Error _ -> try_paths ps
        | Ok body -> (
            try
              let j = Yojson.Safe.from_string body in
              match parse_description j with
              | Some d -> Some d
              | None -> try_paths ps
            with _ -> try_paths ps))
  in
  try_paths paths
