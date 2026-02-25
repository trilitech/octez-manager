(******************************************************************************)
(*                                                                            *)
(* SPDX-License-Identifier: MIT                                               *)
(* Copyright (c) 2026 Nomadic Labs <contact@nomadic-labs.com>                 *)
(*                                                                            *)
(******************************************************************************)

(** Continual payout mode: automatic payouts when new cycles complete. *)

(* ── Active state per instance ───────────────────────────── *)

let active_instances : (string, bool) Hashtbl.t = Hashtbl.create 4

let active_lock = Mutex.create ()

let is_active ~instance =
  Mutex.protect active_lock (fun () ->
      Hashtbl.find_opt active_instances instance |> Option.value ~default:false)

let enable ~instance =
  Mutex.protect active_lock (fun () ->
      Hashtbl.replace active_instances instance true)

let disable ~instance =
  Mutex.protect active_lock (fun () ->
      Hashtbl.replace active_instances instance false)

(* ── Cycle matching ──────────────────────────────────────── *)

let cycles_due ~instance ~current_cycle ~interval ~offset =
  (* Find all unpaid cycles that match the interval pattern *)
  let due = ref [] in
  (* Check cycles from oldest possible to current-1 (current cycle not yet complete) *)
  let check_from = max 0 (current_cycle - 20) in
  for c = check_from to current_cycle - 1 do
    if (c - offset) mod interval = 0 then
      if not (Payout_report.cycle_is_paid ~instance ~cycle:c) then
        due := c :: !due
  done ;
  List.rev !due

(* ── Execute due cycles ──────────────────────────────────── *)

let pay_due_cycles ~ctx ~baker ~network ~current_cycle ~interval ~offset =
  let instance = ctx.Payout_executor.instance in
  let due = cycles_due ~instance ~current_cycle ~interval ~offset in
  List.map
    (fun cycle ->
      let result =
        match
          Payout_blueprint.generate
            ~instance
            ~baker
            ~network
            ~cycle
            ~force:false
            ()
        with
        | Error msg -> Error msg
        | Ok blueprint -> (
            match Payout_executor.execute ~ctx ~blueprint () with
            | Error msg -> Error msg
            | Ok (_results, summary) ->
                (* Send notifications *)
                let channels =
                  match Payout_config.load ~instance with
                  | Ok c -> c.notifications
                  | Error _ -> []
                in
                if channels <> [] then
                  ignore (Payout_notifier.notify_all ~channels ~summary) ;
                Ok ())
      in
      (cycle, result))
    due
