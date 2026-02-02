(******************************************************************************)
(*                                                                            *)
(* SPDX-License-Identifier: MIT                                               *)
(* Copyright (c) 2026 Nomadic Labs <contact@nomadic-labs.com>                 *)
(*                                                                            *)
(******************************************************************************)

open Octez_manager_lib

type status = NotDownloaded | Downloading | Ready | Error of string

let current_status = ref NotDownloaded

let get_status () = !current_status

let openapi_dir () =
  Filename.concat (Common.xdg_data_home ()) "octez-manager/openapi"

let openapi_path () = Filename.concat (openapi_dir ()) "rpc-openapi.json"

let needs_download () = not (Sys.file_exists (openapi_path ()))

(* OpenAPI spec URLs from GitLab *)
let openapi_urls =
  [
    ( "https://gitlab.com/tezos/tezos/-/raw/master/docs/api/rpc-openapi.json",
      "rpc-openapi.json" );
  ]

let ensure_dir path =
  if not (Sys.file_exists path) then
    try Unix.mkdir path 0o755 with Unix.Unix_error (Unix.EEXIST, _, _) -> ()

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
  Common.run cmd

let download_sync () =
  let dir = openapi_dir () in
  (* Create parent directories *)
  let parent = Filename.dirname dir in
  ensure_dir parent ;
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
