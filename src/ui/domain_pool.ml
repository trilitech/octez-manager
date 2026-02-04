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
            let rec loop () =
              if Atomic.get stop then ()
              else
                match Eio.Stream.take_nonblocking stream with
                | Some task ->
                    (try task () with exn -> ignore (Printexc.to_string exn)) ;
                    loop ()
                | None ->
                    Eio_unix.sleep 0.01 ;
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
      ignore (Domain.spawn (fun () -> try fn () with _ -> ()))

let shutdown () =
  match Atomic.get pool_ref with
  | Some pool -> Atomic.set pool.stop true
  | None -> ()
