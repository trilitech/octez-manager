(******************************************************************************)
(*                                                                            *)
(* SPDX-License-Identifier: MIT                                               *)
(* Copyright (c) 2026 Nomadic Labs <contact@nomadic-labs.com>                 *)
(*                                                                            *)
(******************************************************************************)

type status = NotDownloaded | Downloading | Ready | Error of string

(* placeholders kept for future use - dynamic segment type hints *)
type endpoint = {template : string; placeholders : string list [@warning "-69"]}

(* Internal trie for fast navigation *)
type node = {
  mutable get : bool;
  statics : (string, node) Hashtbl.t;
  mutable placeholder_name : string option; (* e.g., "chain_id", "block_id" *)
}

let current_status = ref NotDownloaded

let get_status () = !current_status

let openapi_dir () =
  Filename.concat (Paths.xdg_data_home ()) "octez-manager/openapi"

let openapi_path () = Filename.concat (openapi_dir ()) "rpc-openapi.json"

let needs_download () =
  let dir = openapi_dir () in
  let check fn = not (Sys.file_exists (Filename.concat dir fn)) in
  check "rpc-openapi.json" || check "block.openapi.json"
  || check "mempool.openapi.json"

(* OpenAPI spec URLs from GitLab *)
let openapi_urls =
  [
    ( "https://gitlab.com/tezos/tezos/-/raw/master/docs/api/rpc-openapi.json",
      "rpc-openapi.json" );
    ( "https://gitlab.com/tezos/tezos/-/raw/master/docs/api/alpha-openapi.json",
      "block.openapi.json" );
    ( "https://gitlab.com/tezos/tezos/-/raw/master/docs/api/alpha-mempool-openapi.json",
      "mempool.openapi.json" );
  ]

(* Memoization for trie building - declared early so download_sync can clear them *)
let cached_trie : node option ref = ref None

let cached_endpoints : endpoint list ref = ref []

let entries_cache : (string, string list) Hashtbl.t = Hashtbl.create 97

let ensure_dir = Runtime.ensure_dir

let download_file ~url ~dest =
  let cmd =
    [
      "curl";
      "-fsSL";
      "--max-time";
      "60";
      "--connect-timeout";
      "10";
      "-o";
      dest;
      url;
    ]
  in
  Cmd_runner.run cmd

let download_sync () =
  let dir = openapi_dir () in
  ensure_dir dir ;
  (* Download each file *)
  let results =
    List.map
      (fun (url, filename) ->
        let dest = Filename.concat dir filename in
        let tmp = dest ^ ".tmp" in
        match download_file ~url ~dest:tmp with
        | Result.Ok () -> (
            try
              Sys.rename tmp dest ;
              Result.Ok ()
            with exn -> Result.Error (Printexc.to_string exn))
        | Result.Error (`Msg msg) -> Result.Error msg)
      openapi_urls
  in
  (* Check if all succeeded *)
  let errors =
    List.filter_map
      (function Result.Error msg -> Some msg | Result.Ok () -> None)
      results
  in
  match errors with
  | [] ->
      current_status := Ready ;
      (* Clear caches so new endpoints get loaded *)
      cached_trie := None ;
      cached_endpoints := [] ;
      Hashtbl.clear entries_cache ;
      Result.Ok ()
  | errs ->
      let msg = String.concat "; " errs in
      current_status := Error msg ;
      Result.Error msg

let download_async ~on_complete =
  if !current_status = Downloading then ()
  else (
    current_status := Downloading ;
    Job_manager.submit
      ~description:"Downloading OpenAPI specs"
      (fun ~append_log:_ () ->
        match download_sync () with
        | Result.Ok () -> Result.Ok ()
        | Result.Error msg -> Result.Error (`Msg msg))
      ~on_complete:(fun job_status ->
        let status =
          match job_status with
          | Job_manager.Succeeded -> Ready
          | Job_manager.Failed msg -> Error msg
          | _ -> Error "Download interrupted"
        in
        current_status := status ;
        on_complete status))

let read_spec () =
  let path = openapi_path () in
  if Sys.file_exists path then
    try
      let ic = open_in path in
      let len = in_channel_length ic in
      let content = really_input_string ic len in
      close_in ic ;
      Some content
    with _ -> None
  else None

(* === OpenAPI Parsing and Navigation (ported from octez-setup) === *)

let make_node () =
  {get = false; statics = Hashtbl.create 17; placeholder_name = None}

let placeholder_key = "{}"

let is_placeholder_name (s : string) = String.length s > 0 && s.[0] = '{'

let extract_placeholders (p : string) : string list =
  let rec collect acc i =
    if i >= String.length p then List.rev acc
    else
      match String.index_from_opt p i '{' with
      | None -> List.rev acc
      | Some l -> (
          match String.index_from_opt p (l + 1) '}' with
          | None -> List.rev acc
          | Some r ->
              let name = String.sub p (l + 1) (r - l - 1) in
              collect (name :: acc) (r + 1))
  in
  collect [] 0

let extract_placeholder_name (s : string) : string option =
  if String.length s > 2 && s.[0] = '{' && s.[String.length s - 1] = '}' then
    Some (String.sub s 1 (String.length s - 2))
  else None

let build_trie (eps : endpoint list) : node =
  let split_path (tmpl : string) : string list =
    let parts = String.split_on_char '/' tmpl in
    List.filter (fun p -> p <> "") parts
  in
  let root = make_node () in
  let rec insert parts n =
    match parts with
    | [] -> n.get <- true
    | p :: ps ->
        let is_placeholder = is_placeholder_name p in
        let key = if is_placeholder then placeholder_key else p in
        let child =
          match Hashtbl.find_opt n.statics key with
          | Some c ->
              (* Update placeholder name if we have one and the node doesn't *)
              if is_placeholder && c.placeholder_name = None then
                c.placeholder_name <- extract_placeholder_name p ;
              c
          | None ->
              let c = make_node () in
              if is_placeholder then
                c.placeholder_name <- extract_placeholder_name p ;
              Hashtbl.add n.statics key c ;
              c
        in
        insert ps child
  in
  List.iter (fun (e : endpoint) -> insert (split_path e.template) root) eps ;
  root

let rec traverse (n : node) (segs : string list) : node option =
  match segs with
  | [] -> Some n
  | s :: ss -> (
      match Hashtbl.find_opt n.statics s with
      | Some c -> traverse c ss
      | None -> (
          match Hashtbl.find_opt n.statics placeholder_key with
          | Some c -> traverse c ss
          | None -> None))

let parse_openapi_json (content : string) : endpoint list =
  try
    let json = Yojson.Safe.from_string content in
    let paths =
      match json with
      | `Assoc kvs -> (
          match List.assoc_opt "paths" kvs with Some p -> p | None -> `Null)
      | _ -> `Null
    in
    match paths with
    | `Assoc kvs ->
        List.filter_map
          (fun (p, ops) ->
            let has_get =
              match ops with
              | `Assoc ops_kvs -> (
                  match List.assoc_opt "get" ops_kvs with
                  | Some (`Assoc _) -> true
                  | _ -> false)
              | _ -> false
            in
            if not has_get then None
            else
              let placeholders = extract_placeholders p in
              Some {template = p; placeholders})
          kvs
    | _ -> []
  with _ -> []

(** Add prefix to endpoint templates *)
let with_prefix base eps =
  let base =
    if String.length base > 0 && base.[String.length base - 1] = '/' then base
    else base ^ "/"
  in
  List.map
    (fun (e : endpoint) ->
      let suffix =
        if e.template = "" then ""
        else if e.template.[0] = '/' then
          String.sub e.template 1 (String.length e.template - 1)
        else e.template
      in
      let template = base ^ suffix in
      let placeholders = extract_placeholders template in
      {template; placeholders})
    eps

let read_file filename =
  let path = Filename.concat (openapi_dir ()) filename in
  if Sys.file_exists path then
    try
      let ic = open_in path in
      let len = in_channel_length ic in
      let content = really_input_string ic len in
      close_in ic ;
      Some content
    with _ -> None
  else None

let load_endpoints () =
  if !cached_endpoints <> [] then !cached_endpoints
  else
    (* Load main shell RPCs *)
    let eps_main =
      match read_file "rpc-openapi.json" with
      | Some content -> parse_openapi_json content
      | None -> []
    in
    (* Load block-level protocol RPCs and prefix them *)
    let eps_block =
      match read_file "block.openapi.json" with
      | Some content ->
          let eps = parse_openapi_json content in
          with_prefix "/chains/{chain_id}/blocks/{block_id}" eps
      | None -> []
    in
    (* Load mempool RPCs and prefix them *)
    let eps_mempool =
      match read_file "mempool.openapi.json" with
      | Some content ->
          let eps = parse_openapi_json content in
          with_prefix "/chains/{chain_id}/mempool" eps
      | None -> []
    in
    let combined = eps_main @ eps_block @ eps_mempool in
    cached_endpoints := combined ;
    combined

let get_trie () =
  match !cached_trie with
  | Some t -> Some t
  | None ->
      let eps = load_endpoints () in
      if eps = [] then None
      else begin
        let t = build_trie eps in
        cached_trie := Some t ;
        Some t
      end

(** Get navigation entries for a path from OpenAPI.
    Returns list of alternating [name; kind; name; kind; ...] where kind is
    "__SUB__", "__GET__", or "__DYN__". *)
let entries_for ~(segs : string list) : string list =
  let key = String.concat "/" segs in
  match Hashtbl.find_opt entries_cache key with
  | Some v -> v
  | None -> (
      match get_trie () with
      | None -> []
      | Some trie ->
          let nexts_tbl = Hashtbl.create 17 in
          let placeholder_name_ref = ref None in
          let has_get =
            match traverse trie segs with
            | None -> false
            | Some n ->
                Hashtbl.iter
                  (fun k child ->
                    Hashtbl.replace nexts_tbl k true ;
                    (* Capture placeholder name if this is the placeholder child *)
                    if k = placeholder_key then
                      placeholder_name_ref := child.placeholder_name)
                  n.statics ;
                if Hashtbl.mem nexts_tbl placeholder_key then
                  Hashtbl.replace nexts_tbl "__DYN__" true ;
                n.get
          in
          let subs =
            Hashtbl.to_seq_keys nexts_tbl
            |> List.of_seq
            |> List.filter (fun k -> k <> placeholder_key)
            |> List.sort String.compare
          in
          let acc = ref [] in
          List.iter
            (fun name ->
              if name <> "__DYN__" then acc := !acc @ [name; "__SUB__"])
            subs ;
          if Hashtbl.mem nexts_tbl "__DYN__" then begin
            (* Use the actual placeholder name if available *)
            let dyn_name =
              match !placeholder_name_ref with
              | Some name -> "<" ^ name ^ ">"
              | None -> "<value>"
            in
            acc := !acc @ [dyn_name; "__DYN__"]
          end ;
          if has_get then acc := !acc @ ["[GET]"; "__GET__"] ;
          let res = !acc in
          Hashtbl.replace entries_cache key res ;
          res)

(** Check if OpenAPI data is available for navigation *)
let is_available () = match read_spec () with Some _ -> true | None -> false

(** Clear caches (useful when OpenAPI is re-downloaded) *)
let clear_cache () =
  cached_trie := None ;
  cached_endpoints := [] ;
  Hashtbl.clear entries_cache

module For_tests = struct
  type nonrec endpoint = endpoint = {
    template : string;
    placeholders : string list;
  }

  type nonrec node = node

  let parse_openapi_json = parse_openapi_json

  let extract_placeholders = extract_placeholders

  let extract_placeholder_name = extract_placeholder_name

  let build_trie = build_trie

  let traverse = traverse

  let with_prefix = with_prefix

  let node_has_get (n : node) = n.get
end
