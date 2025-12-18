(******************************************************************************)
(*                                                                            *)
(* SPDX-License-Identifier: MIT                                               *)
(* Copyright (c) 2025 Nomadic Labs <contact@nomadic-labs.com>                 *)
(*                                                                            *)
(******************************************************************************)

(** Baker highwatermarks reading.

    Reads the highwatermarks file from the baker's base directory
    to display the last signed block/attestation/preattestation levels.

    Data is cached and updated by a background scheduler. The UI only
    reads from the cache and never blocks on file I/O. *)

open Octez_manager_lib

(** Highwatermark entry for a delegate *)
type highwatermark = {round : int; level : int}

(** Per-delegate activity *)
type delegate_activity = {
  delegate : string;
  last_block : highwatermark option;
  last_preattestation : highwatermark option;
  last_attestation : highwatermark option;
}

(** Parse a highwatermark from JSON *)
let parse_highwatermark json =
  let open Yojson.Safe.Util in
  try
    let round = json |> member "round" |> to_int in
    let level = json |> member "level" |> to_int in
    Some {round; level}
  with _ -> None

(** Parse an entry with delegate and highwatermark *)
let parse_entry json =
  let open Yojson.Safe.Util in
  try
    let delegate = json |> member "delegate" |> to_string in
    let hwm = json |> member "highwatermark" |> parse_highwatermark in
    Some (delegate, hwm)
  with _ -> None

(** Parse all entries of a given type (blocks/preattestations/attestations) *)
let parse_entries json =
  let open Yojson.Safe.Util in
  try json |> to_list |> List.filter_map parse_entry with _ -> []

(** Read and parse a highwatermarks file *)
let read_file path =
  try
    let ic = open_in path in
    let content = really_input_string ic (in_channel_length ic) in
    close_in ic ;
    let json = Yojson.Safe.from_string content in
    let open Yojson.Safe.Util in
    let blocks = json |> member "blocks" |> parse_entries in
    let preattestations = json |> member "preattestations" |> parse_entries in
    let attestations = json |> member "attestations" |> parse_entries in
    (* Combine into per-delegate activity *)
    let delegates = Hashtbl.create 17 in
    List.iter
      (fun (d, hwm) ->
        let entry =
          match Hashtbl.find_opt delegates d with
          | Some e -> {e with last_block = hwm}
          | None ->
              {
                delegate = d;
                last_block = hwm;
                last_preattestation = None;
                last_attestation = None;
              }
        in
        Hashtbl.replace delegates d entry)
      blocks ;
    List.iter
      (fun (d, hwm) ->
        let entry =
          match Hashtbl.find_opt delegates d with
          | Some e -> {e with last_preattestation = hwm}
          | None ->
              {
                delegate = d;
                last_block = None;
                last_preattestation = hwm;
                last_attestation = None;
              }
        in
        Hashtbl.replace delegates d entry)
      preattestations ;
    List.iter
      (fun (d, hwm) ->
        let entry =
          match Hashtbl.find_opt delegates d with
          | Some e -> {e with last_attestation = hwm}
          | None ->
              {
                delegate = d;
                last_block = None;
                last_preattestation = None;
                last_attestation = hwm;
              }
        in
        Hashtbl.replace delegates d entry)
      attestations ;
    Hashtbl.fold (fun _ v acc -> v :: acc) delegates []
  with _ -> []

(** Find highwatermarks file in a baker's base directory *)
let find_highwatermarks_file base_dir =
  try
    let files = Sys.readdir base_dir in
    Array.to_list files
    |> List.find_opt (fun f ->
        String.length f > 15
        && String.sub f (String.length f - 15) 15 = "_highwatermarks")
    |> Option.map (fun f -> Filename.concat base_dir f)
  with _ -> None

(** Get baker's base directory from env file *)
let get_baker_base_dir ~instance =
  match Node_env.read ~inst:instance with
  | Error _ -> None
  | Ok pairs -> (
      match List.assoc_opt "OCTEZ_BAKER_BASE_DIR" pairs with
      | None -> None
      | Some d ->
          let d = String.trim d in
          if d = "" then None else Some d)

(** Read highwatermarks for a baker instance (internal, does file I/O) *)
let read_from_disk ~instance =
  match get_baker_base_dir ~instance with
  | None -> []
  | Some base_dir -> (
      match find_highwatermarks_file base_dir with
      | None -> []
      | Some path -> read_file path)

(** {1 Cache for UI access} *)

(** Cache: instance -> delegate_activity list *)
let cache : (string, delegate_activity list) Hashtbl.t = Hashtbl.create 17

let cache_lock = Mutex.create ()

(** Get cached highwatermarks for display (never blocks on I/O) *)
let get ~instance =
  Mutex.protect cache_lock (fun () ->
      Hashtbl.find_opt cache instance |> Option.value ~default:[])

(** Update cache for an instance (called by background scheduler) *)
let refresh ~instance =
  let data = read_from_disk ~instance in
  Mutex.protect cache_lock (fun () -> Hashtbl.replace cache instance data)

(** Clear cache entry when instance is removed.
    Note: Currently not called automatically - stale entries persist until
    the app restarts. This is acceptable since instance names are unique
    and stale entries cause no harm (they're just not displayed). *)
let clear ~instance =
  Mutex.protect cache_lock (fun () -> Hashtbl.remove cache instance)

(** Deprecated: use [get] instead. This exists for backwards compatibility. *)
let read ~instance = get ~instance

(** Get the maximum level from a delegate's activity *)
let max_level activity =
  let levels =
    [
      activity.last_block;
      activity.last_preattestation;
      activity.last_attestation;
    ]
    |> List.filter_map (Option.map (fun h -> h.level))
  in
  match levels with [] -> None | l -> Some (List.fold_left max 0 l)

(** Format a short summary for display *)
let format_summary activities =
  if activities = [] then None
  else
    let parts =
      List.filter_map
        (fun a ->
          match max_level a with
          | None -> None
          | Some lvl ->
              let short_delegate =
                if String.length a.delegate > 8 then
                  String.sub a.delegate 0 8 ^ "…"
                else a.delegate
              in
              Some (Printf.sprintf "%s:L%d" short_delegate lvl))
        activities
    in
    if parts = [] then None else Some (String.concat " " parts)
