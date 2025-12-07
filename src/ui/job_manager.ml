open Rresult

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

let submit ~description action =
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
  let thread =
    Lwt_preemptive.detach
      (fun () ->
        try action () with exn -> Error (`Msg (Printexc.to_string exn)))
      ()
  in
  Lwt.async (fun () ->
      let open Lwt.Infix in
      thread >>= fun result ->
      job.finished_at <- Some (Unix.gettimeofday ()) ;
      (match result with
      | Ok () -> job.status <- Succeeded
      | Error (`Msg e) -> job.status <- Failed e) ;
      Lwt.return_unit)

let list () = !jobs

let clear_finished () =
  jobs := List.filter (fun j -> j.status = Running || j.status = Pending) !jobs
