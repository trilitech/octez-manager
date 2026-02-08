(******************************************************************************)
(*                                                                            *)
(* SPDX-License-Identifier: MIT                                               *)
(* Copyright (c) 2026 Nomadic Labs <contact@nomadic-labs.com>                 *)
(*                                                                            *)
(******************************************************************************)

(** Persistence for RPC browser dynamic history and recent paths.

    Handles loading and saving dynamic segment history and recent path
    shortcuts to disk as JSON files. *)

open Rpc_browser_types

(** Return the file path for dynamic history storage. *)
let history_file () =
  Filename.concat
    (Paths.xdg_config_home ())
    "octez-manager/rpc_dynamic_history.json"

(** Return the file path for recent paths storage. *)
let recent_paths_file () =
  Filename.concat
    (Paths.xdg_config_home ())
    "octez-manager/rpc_recent_paths.json"

(** Load recent paths from disk. Returns empty list on failure. *)
let load_recent_paths () =
  let path = recent_paths_file () in
  if Sys.file_exists path then
    try
      let ic = open_in path in
      let content = really_input_string ic (in_channel_length ic) in
      close_in ic ;
      match Yojson.Safe.from_string content with
      | `List items ->
          List.filter_map
            (fun item ->
              match item with
              | `Assoc kvs -> (
                  match
                    ( List.assoc_opt "path" kvs,
                      List.assoc_opt "desc" kvs,
                      List.assoc_opt "timestamp" kvs )
                  with
                  | Some (`String p), Some (`String d), Some (`Float ts) ->
                      Some {rp_path = p; rp_desc = d; rp_timestamp = ts}
                  | _ -> None)
              | _ -> None)
            items
      | _ -> []
    with _ -> []
  else []

(** Save recent paths to disk. Silently ignores errors. *)
let save_recent_paths paths =
  let path = recent_paths_file () in
  let dir = Filename.dirname path in
  (if not (Sys.file_exists dir) then try Unix.mkdir dir 0o755 with _ -> ()) ;
  try
    let json =
      `List
        (List.map
           (fun rp ->
             `Assoc
               [
                 ("path", `String rp.rp_path);
                 ("desc", `String rp.rp_desc);
                 ("timestamp", `Float rp.rp_timestamp);
               ])
           paths)
    in
    let oc = open_out path in
    output_string oc (Yojson.Safe.pretty_to_string json) ;
    close_out oc
  with _ -> ()

(** Load dynamic history from disk. Returns empty list on failure. *)
let load_dynamic_history () =
  let path = history_file () in
  if Sys.file_exists path then
    try
      let ic = open_in path in
      let content = really_input_string ic (in_channel_length ic) in
      close_in ic ;
      match Yojson.Safe.from_string content with
      | `List items ->
          List.filter_map
            (fun item ->
              match item with
              | `Assoc kvs -> (
                  match
                    ( List.assoc_opt "segment_type" kvs,
                      List.assoc_opt "value" kvs,
                      List.assoc_opt "timestamp" kvs )
                  with
                  | Some (`String st), Some (`String v), Some (`Float ts) ->
                      Some {segment_type = st; value = v; timestamp = ts}
                  | _ -> None)
              | _ -> None)
            items
      | _ -> []
    with _ -> []
  else []

(** Save dynamic history to disk. Silently ignores errors. *)
let save_dynamic_history history =
  let path = history_file () in
  let dir = Filename.dirname path in
  (if not (Sys.file_exists dir) then try Unix.mkdir dir 0o755 with _ -> ()) ;
  try
    let json =
      `List
        (List.map
           (fun dv ->
             `Assoc
               [
                 ("segment_type", `String dv.segment_type);
                 ("value", `String dv.value);
                 ("timestamp", `Float dv.timestamp);
               ])
           history)
    in
    let oc = open_out path in
    output_string oc (Yojson.Safe.pretty_to_string json) ;
    close_out oc
  with _ -> ()
