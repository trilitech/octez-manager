(* SPDX-License-Identifier: MIT *)
(* Simple flash message store used by the headless test driver. *)

[@@@warning "-32-34-37-69"]

type level = Info | Success | Warn | Error

let now () = Unix.gettimeofday ()

let store : (float * level * string) list ref = ref []

let push ?(level = Info) ?(duration = 2.5) msg =
  let expiry = now () +. duration in
  store := (expiry, level, msg) :: !store

let prune () =
  let t = now () in
  store := List.filter (fun (e, _, _) -> e > t) !store

let snapshot () =
  prune () ;
  List.rev_map (fun (_, l, m) -> (l, m)) (List.rev !store)

let tick () = prune ()
