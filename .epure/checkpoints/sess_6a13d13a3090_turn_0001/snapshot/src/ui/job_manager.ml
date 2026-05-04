(******************************************************************************)
(*                                                                            *)
(* SPDX-License-Identifier: MIT                                               *)
(* Copyright (c) 2025-2026 Nomadic Labs <contact@nomadic-labs.com>            *)
(*                                                                            *)
(******************************************************************************)

open Rresult
module Bg = Background_runner

type status = Pending | Running | Succeeded | Failed of string

type job = {
  id : int;
  description : string;
  mutable status : status;
  started_at : float;
  mutable finished_at : float option;
  mutable log : string list;
  mutable phase : string;
  timeout : float option;
  on_timeout : unit -> unit;
}

let jobs : job list ref = ref []

let next_id = ref 1

let submit ?(on_complete = fun _ -> ()) ?(timeout = Some 120.0) ~description
    action =
  let id = !next_id in
  incr next_id ;
  let on_complete_called = Atomic.make false in
  let call_on_complete status =
    if not (Atomic.exchange on_complete_called true) then on_complete status
  in
  let job =
    {
      id;
      description;
      status = Running;
      started_at = Unix.gettimeofday ();
      finished_at = None;
      log = [];
      phase = "";
      timeout;
      on_timeout = (fun () -> call_on_complete (Failed "Operation timed out"));
    }
  in
  jobs := job :: !jobs ;
  Bg.submit_blocking (fun () ->
      let append_log line =
        job.log <- line :: job.log ;
        (* Detect phase changes from === headers === *)
        let trimmed = String.trim line in
        if
          String.length trimmed > 6
          && String.sub trimmed 0 3 = "==="
          && String.sub trimmed (String.length trimmed - 3) 3 = "==="
        then
          let inner =
            String.sub trimmed 3 (String.length trimmed - 6) |> String.trim
          in
          if inner <> "" then job.phase <- inner
      in
      let result =
        try action ~append_log ()
        with exn -> Error (`Msg (Printexc.to_string exn))
      in
      job.finished_at <- Some (Unix.gettimeofday ()) ;
      match result with
      | Ok () ->
          job.status <- Succeeded ;
          call_on_complete job.status
      | Error (`Msg e) ->
          job.status <- Failed e ;
          call_on_complete job.status)

let list () = !jobs

let clear_finished () =
  jobs := List.filter (fun j -> j.status = Running || j.status = Pending) !jobs

let get_running_job () = List.find_opt (fun j -> j.status = Running) !jobs

let get_latest_job () = match !jobs with [] -> None | job :: _ -> Some job

let check_stuck_jobs () =
  let now = Unix.gettimeofday () in
  List.iter
    (fun job ->
      match (job.status, job.timeout) with
      | Running, Some timeout when now -. job.started_at > timeout ->
          job.status <- Failed "Operation timed out" ;
          job.finished_at <- Some now ;
          Context.toast_error
            (Printf.sprintf
               "Job \"%s\" timed out after %.0fs"
               job.description
               timeout) ;
          job.on_timeout ()
      | _ -> ())
    !jobs

let tick () = check_stuck_jobs ()

module For_tests = struct
  let inject_job job = jobs := job :: !jobs
end
