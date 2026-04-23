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

let delay_file ~instance =
  Filename.concat (Payout_config.rewards_dir ~instance) "delay_until"

let read_delay_until ~instance =
  let path = delay_file ~instance in
  if not (Sys.file_exists path) then None
  else
    try
      let ic = open_in path in
      let line = input_line ic in
      close_in ic ;
      Float.of_string_opt (String.trim line)
    with Sys_error _ -> None

(** Pure trigger check: is this cycle on an interval boundary? *)
let is_trigger_cycle ~current_cycle ~interval ~offset =
  interval <= 1 || (current_cycle - offset) mod interval = 0

(** Pure cycle collection: given a predicate for "is paid", return due cycles. *)
let collect_due_cycles ~current_cycle ~is_paid =
  let due = ref [] in
  let check_from = max 0 (current_cycle - 20) in
  for c = check_from to current_cycle - 1 do
    if not (is_paid c) then due := c :: !due
  done ;
  List.rev !due

let cycles_due ~instance ~current_cycle ~interval ~offset =
  (* Only trigger on interval boundaries *)
  if not (is_trigger_cycle ~current_cycle ~interval ~offset) then []
  else
    collect_due_cycles ~current_cycle ~is_paid:(fun c ->
        Payout_report.cycle_is_paid ~instance ~cycle:c)

(* ── Execute due cycles ──────────────────────────────────── *)

let pay_due_cycles ~ctx ~baker ~network ~current_cycle ~interval ~offset =
  let instance = ctx.Payout_executor.instance in
  let config =
    match Payout_config.load ~instance with
    | Ok c -> c
    | Error _ -> Payout_config.default ~baker_pkh:baker
  in
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
            match
              Payout_executor.execute
                ~ctx
                ~blueprint
                ~batch_size:config.sim_batch_size
                ()
            with
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

module Internal_for_tests = struct
  let is_trigger_cycle = is_trigger_cycle

  let collect_due_cycles = collect_due_cycles
end
