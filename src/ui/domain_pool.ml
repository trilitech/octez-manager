(******************************************************************************)
(*                                                                            *)
(* SPDX-License-Identifier: MIT                                               *)
(* Copyright (c) 2026 Nomadic Labs <contact@nomadic-labs.com>                 *)
(*                                                                            *)
(******************************************************************************)

type t = {stream : (unit -> unit) Eio.Stream.t; stop : bool Atomic.t}

let pool_ref : t option Atomic.t = Atomic.make None

let set pool = Atomic.set pool_ref (Some pool)

let create ~sw ~domain_mgr ~num_domains =
  let stream = Eio.Stream.create 1024 in
  let stop = Atomic.make false in
  for _ = 1 to num_domains do
    Eio.Fiber.fork_daemon ~sw (fun () ->
        Eio.Domain_manager.run domain_mgr (fun () ->
            Eio.Switch.run @@ fun domain_sw ->
            let rec loop () =
              if Atomic.get stop then ()
              else
                (* Block efficiently on stream; wake every 1s to re-check
                   the stop flag. Eio.Fiber.first cancels the losing fiber,
                   so either a task arrives instantly or we time out. *)
                let task_opt =
                  Eio.Fiber.first
                    (fun () -> Some (Eio.Stream.take stream))
                    (fun () ->
                      Eio_unix.sleep 1.0 ;
                      None)
                in
                (match task_opt with
                | Some task ->
                    Eio.Fiber.fork ~sw:domain_sw (fun () ->
                        try task ()
                        with exn ->
                          (Printf.eprintf
                          [@allow_forbidden "error logging in domain pool"])
                            "[Domain_pool] task failed: %s\n%!"
                            (Printexc.to_string exn))
                | None -> ()) ;
                loop ()
            in
            loop ()) ;
        `Stop_daemon)
  done ;
  {stream; stop}

let submit fn =
  match Atomic.get pool_ref with
  | Some pool -> ( try Eio.Stream.add pool.stream fn with _ -> ())
  | None ->
      (* Fallback: run directly in a new domain (pre-init or CLI mode) *)
      ignore
        (Domain.spawn (fun () ->
             try fn ()
             with exn ->
               (Printf.eprintf
               [@allow_forbidden "error logging in domain pool"])
                 "[Domain_pool] task failed: %s\n%!"
                 (Printexc.to_string exn)))

(** Shutdown the pool by signaling all worker domains to stop.
    Workers finish their current task and exit within 1s (the stop-check
    interval).  The pool domains themselves are daemon fibers under the
    parent switch, so they are cancelled automatically when the switch
    exits — no explicit join is needed. *)
let shutdown () =
  match Atomic.get pool_ref with
  | Some pool -> Atomic.set pool.stop true
  | None -> ()
