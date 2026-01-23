(******************************************************************************)
(*                                                                            *)
(* SPDX-License-Identifier: MIT                                               *)
(* Copyright (c) 2026 Nomadic Labs <contact@nomadic-labs.com>                 *)
(*                                                                            *)
(******************************************************************************)

open Rresult
open Installer_types
open Helpers

let history_mode_matches ~requested ~snapshot_mode =
  let requested_str = History_mode.to_string requested in
  let requested_lower = String.lowercase_ascii requested_str in
  let snapshot_lower = String.lowercase_ascii (String.trim snapshot_mode) in
  requested_lower = snapshot_lower

let resolve_snapshot_download ~network ~history_mode =
  let* network_slug =
    match Snapshots.slug_of_network network with
    | Some slug -> Ok slug
    | None ->
        R.error_msg
          "Unable to infer a tzinit network slug from --network. Provide \
           either a known alias (mainnet/ghostnet/...) or a teztnets JSON URL."
  in
  let* kind_slug, kind_label =
    match history_mode with
    | History_mode.Rolling -> Ok ("rolling", "rolling")
    | History_mode.Full -> Ok ("full", "full")
    | History_mode.Archive ->
        R.error_msg
          "Snapshots are not provided for archive history mode. Download a \
           full archive manually, extract it into your target data directory, \
           then rerun octez-manager without --snapshot but with --data-dir \
           pointing to that directory."
  in
  let* entry_opt =
    Snapshots.fetch_entry ~network_slug ~slug:kind_slug ~label:kind_label
  in
  match entry_opt with
  | None ->
      R.error_msgf
        "Snapshot kind '%s' is not advertised for %s. Run `octez-manager \
         list-snapshots --network %s` to inspect available downloads."
        kind_label
        network
        network
  | Some entry -> (
      match entry.history_mode with
      | Some snapshot_mode when String.trim snapshot_mode <> "" -> (
          if not (history_mode_matches ~requested:history_mode ~snapshot_mode)
          then
            R.error_msgf
              "Snapshot '%s' has history mode '%s' but requested history mode \
               is '%s'. Please select a snapshot with the correct history mode \
               or change your history mode selection to match."
              entry.label
              snapshot_mode
              (History_mode.to_string history_mode)
          else
            match entry.download_url with
            | Some url ->
                Ok {download_url = String.trim url; network_slug; kind_slug}
            | None ->
                R.error_msgf
                  "Snapshot kind '%s' for %s exposes no HTTPS download on \
                   tzinit."
                  entry.label
                  network)
      | Some _ | None -> (
          (* If history mode is not available in metadata, proceed with caution *)
          match entry.download_url with
          | Some url ->
              Ok {download_url = String.trim url; network_slug; kind_slug}
          | None ->
              R.error_msgf
                "Snapshot kind '%s' for %s exposes no HTTPS download on tzinit."
                entry.label
                network))

type snapshot_file = {path : string; cleanup : bool}

type snapshot_progress = {
  on_download_progress : (int -> int option -> unit) option;
}

let download_snapshot ?(quiet = false) ?on_log ?progress ?tmp_dir src =
  let tmp =
    match tmp_dir with
    | Some dir ->
        let name =
          Printf.sprintf "octez-manager.snapshot.%d.snap" (Unix.getpid ())
        in
        Filename.concat dir name
    | None -> Filename.temp_file "octez-manager.snapshot" ".snap"
  in
  let last_log_time = ref 0. in
  let res =
    match (progress, on_log) with
    | Some {on_download_progress}, _ ->
        Common.download_file_with_progress
          ~url:src
          ~dest_path:tmp
          ~on_progress:(fun downloaded total ->
            match on_download_progress with
            | Some f ->
                (* Convert byte counts to percentage for the callback *)
                let pct =
                  match total with
                  | Some t when t > 0 -> downloaded * 100 / t
                  | _ -> 0
                in
                f pct (Some 100)
            | None -> ())
    | None, Some log ->
        (* Use progress download and convert to log messages *)
        Common.download_file_with_progress
          ~url:src
          ~dest_path:tmp
          ~on_progress:(fun downloaded total ->
            (* Convert byte counts to percentage *)
            let pct =
              match total with
              | Some t when t > 0 -> downloaded * 100 / t
              | _ -> 0
            in
            (* Log every 5 seconds to avoid flooding *)
            let now = Unix.gettimeofday () in
            if now -. !last_log_time >= 5. then (
              last_log_time := now ;
              log (Printf.sprintf "Download progress: %d%%\n" pct)))
    | None, None -> Common.download_file ~quiet ~url:src ~dest_path:tmp ()
  in
  match res with
  | Ok () -> Ok {path = tmp; cleanup = true}
  | Error _ as e ->
      Common.remove_path tmp ;
      e

let prepare_snapshot_source ?(quiet = false) ?on_log ?progress ?tmp_dir src =
  let trimmed = String.trim src in
  if trimmed = "" then R.error_msg "Snapshot URI is empty"
  else
    match strip_file_uri trimmed with
    | Some path -> Ok {path; cleanup = false}
    | None when not (is_http_url trimmed) ->
        if Sys.file_exists trimmed then Ok {path = trimmed; cleanup = false}
        else R.error_msgf "Snapshot file %s does not exist" trimmed
    | _ -> download_snapshot ~quiet ?on_log ?progress ?tmp_dir trimmed

let snapshot_plan_of_request request =
  match request.bootstrap with
  | Genesis -> Ok No_snapshot
  | Snapshot {src} -> (
      match normalize_optional_string src with
      | Some uri -> Ok (Direct_snapshot {uri})
      | None ->
          resolve_snapshot_download
            ~network:request.network
            ~history_mode:request.history_mode
          |> Result.map (fun res -> Tzinit_snapshot res))

let snapshot_metadata_of_plan ~no_check = function
  | No_snapshot ->
      {
        auto = false;
        uri = None;
        network_slug = None;
        kind_slug = None;
        no_check;
      }
  | Direct_snapshot {uri} ->
      {
        auto = true;
        uri = Some uri;
        network_slug = None;
        kind_slug = None;
        no_check;
      }
  | Tzinit_snapshot res ->
      {
        auto = true;
        uri = None;
        network_slug = Some res.network_slug;
        kind_slug = Some res.kind_slug;
        no_check;
      }

let import_snapshot ?(quiet = false) ?on_log ~app_bin_dir ~data_dir
    ~snapshot_path ~no_check () =
  let octez_node = Filename.concat app_bin_dir "octez-node" in
  let args =
    let base =
      [octez_node; "snapshot"; "import"; "--data-dir"; data_dir; snapshot_path]
    in
    if no_check then base @ ["--no-check"] else base
  in
  (* Use streaming for real-time output when on_log is provided *)
  match on_log with
  | Some log ->
      (* Wrap log to detect and announce phases *)
      let current_phase = ref "" in
      let phase_log line =
        let announce phase =
          if !current_phase <> phase then (
            current_phase := phase ;
            log (Printf.sprintf "\n=== %s ===\n" phase))
        in
        (if String.length line > 0 then
           let lower = String.lowercase_ascii line in
           if
             String.sub lower 0 (min 10 (String.length lower)) |> fun s ->
             String.ends_with ~suffix:"retrieving" s
           then announce "Importing context"
           else if
             String.ends_with ~suffix:"integrity_check." lower
             || String.ends_with ~suffix:"integrity check" lower
           then announce "Integrity check"
           else if String.starts_with ~prefix:"storing floating" lower then
             announce "Storing blocks") ;
        log line
      in
      Common.run_streaming ~on_log:phase_log args
  | None -> Common.run ~quiet args

let import_snapshot_file ?(quiet = false) ?on_log ~app_bin_dir ~data_dir
    ~snapshot_file ~no_check () =
  import_snapshot
    ~quiet
    ?on_log
    ~app_bin_dir
    ~data_dir
    ~snapshot_path:snapshot_file.path
    ~no_check
    ()

let perform_snapshot_plan ?(quiet = false) ?on_log ?tmp_dir
    ?(keep_snapshot = false) ~plan ~app_bin_dir ~data_dir ~no_check () =
  let log msg = match on_log with Some f -> f msg | None -> () in
  let should_cleanup snapshot_file =
    snapshot_file.cleanup && not keep_snapshot
  in
  match plan with
  | No_snapshot -> Ok ()
  | Direct_snapshot {uri} ->
      log "\n=== Downloading snapshot ===\n" ;
      log (Printf.sprintf "From: %s\n" uri) ;
      let* snapshot_file =
        prepare_snapshot_source ~quiet ?on_log ?tmp_dir uri
      in
      log "\n=== Importing snapshot ===\n" ;
      Fun.protect
        ~finally:(fun () ->
          if should_cleanup snapshot_file then
            Common.remove_path snapshot_file.path)
        (fun () ->
          import_snapshot_file
            ~quiet
            ?on_log
            ~app_bin_dir
            ~data_dir
            ~snapshot_file
            ~no_check
            ())
  | Tzinit_snapshot res ->
      log "\n=== Downloading snapshot ===\n" ;
      log (Printf.sprintf "From: %s\n" res.download_url) ;
      let* snapshot_file =
        download_snapshot ~quiet ?on_log ?tmp_dir res.download_url
      in
      log "\n=== Importing snapshot ===\n" ;
      Fun.protect
        ~finally:(fun () ->
          if should_cleanup snapshot_file then
            Common.remove_path snapshot_file.path)
        (fun () ->
          import_snapshot_file
            ~quiet
            ?on_log
            ~app_bin_dir
            ~data_dir
            ~snapshot_file
            ~no_check
            ())

let perform_bootstrap ?(quiet = false) ?on_log ?tmp_dir ~plan
    ~(request : node_request) ~data_dir () =
  perform_snapshot_plan
    ~quiet
    ?on_log
    ?tmp_dir
    ~keep_snapshot:request.keep_snapshot
    ~plan
    ~app_bin_dir:request.app_bin_dir
    ~data_dir
    ~no_check:request.snapshot_no_check
    ()
