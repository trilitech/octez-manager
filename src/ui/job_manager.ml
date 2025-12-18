(******************************************************************************)
(*                                                                            *)
(* SPDX-License-Identifier: MIT                                               *)
(* Copyright (c) 2025 Nomadic Labs <contact@nomadic-labs.com>                 *)
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
}

let jobs : job list ref = ref []

let next_id = ref 1

let submit ?(on_complete = fun _ -> ()) ~description action =
  let id = !next_id in
  incr next_id ;
  let job =
    {
      id;
      description;
      status = Running;
      started_at = Unix.gettimeofday ();
      finished_at = None;
      log = [];
    }
  in
  jobs := job :: !jobs ;
  Bg.submit_blocking (fun () ->
      let result =
        try action () with exn -> Error (`Msg (Printexc.to_string exn))
      in
      job.finished_at <- Some (Unix.gettimeofday ()) ;
      match result with
      | Ok () ->
          job.status <- Succeeded ;
          on_complete job.status
      | Error (`Msg e) ->
          job.status <- Failed e ;
          on_complete job.status)

let list () = !jobs

let clear_finished () =
  jobs := List.filter (fun j -> j.status = Running || j.status = Pending) !jobs
