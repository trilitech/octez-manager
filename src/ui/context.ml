(******************************************************************************)
(*                                                                            *)
(* SPDX-License-Identifier: MIT                                               *)
(* Copyright (c) 2025-2026 Nomadic Labs <contact@nomadic-labs.com>            *)
(*                                                                            *)
(******************************************************************************)

let pending_instance_detail : string option ref = ref None

let pending_external_service : Octez_manager_lib.External_service.t option ref =
  ref None

(* Use Atomic for cross-domain visibility - mark_instances_dirty can be called
   from background worker domains via Job_manager.on_complete *)
let instances_dirty = Atomic.make false

let set_pending_instance_detail inst = pending_instance_detail := Some inst

let take_pending_instance_detail () =
  let value = !pending_instance_detail in
  pending_instance_detail := None ;
  value

let set_pending_external_service svc = pending_external_service := Some svc

let take_pending_external_service () =
  let value = !pending_external_service in
  pending_external_service := None ;
  value

(* Edit mode: service being edited and list of stopped dependents *)
type edit_context = {
  service : Octez_manager_lib.Service.t;
  stopped_dependents : string list;
}

let pending_edit_service : edit_context option ref = ref None

let set_pending_edit_service ~service ~stopped_dependents =
  pending_edit_service := Some {service; stopped_dependents}

let take_pending_edit_service () =
  let value = !pending_edit_service in
  pending_edit_service := None ;
  value

let has_pending_edit_service () = Option.is_some !pending_edit_service

(* Pending restart: list of dependents that were stopped during edit and need restart *)
let pending_restart_dependents : string list ref = ref []

let set_pending_restart_dependents deps = pending_restart_dependents := deps

let take_pending_restart_dependents () =
  let value = !pending_restart_dependents in
  pending_restart_dependents := [] ;
  value

let mark_instances_dirty () = Atomic.set instances_dirty true

let consume_instances_dirty () = Atomic.exchange instances_dirty false

let pending_navigation : string option ref = ref None

let navigate page = pending_navigation := Some page

let consume_navigation () =
  let value = !pending_navigation in
  pending_navigation := None ;
  value

(* Flag to indicate that navigation.back should be skipped once.
   Used when form exits to prevent back-navigation loop. *)
let skip_back_once = ref false

let set_skip_back_once () = skip_back_once := true

let consume_skip_back_once () =
  let value = !skip_back_once in
  skip_back_once := false ;
  value

(* Global toast state *)
let toasts_lock = Mutex.create ()

let toasts =
  ref (Miaou_widgets_layout.Toast_widget.empty ~position:`Bottom_right ())

(* Avoid duplicate consecutive toasts with the same severity/message. *)
let push_toast severity msg =
  Mutex.lock toasts_lock ;
  Fun.protect
    ~finally:(fun () -> Mutex.unlock toasts_lock)
    (fun () ->
      let current = !toasts in
      let last =
        match Miaou_widgets_layout.Toast_widget.to_list current |> List.rev with
        | [] -> None
        | t :: _ -> Some (t.severity, t.message)
      in
      let duplicate =
        match last with
        | Some (sev, m) -> sev = severity && String.equal m msg
        | None -> false
      in
      if not duplicate then
        toasts := Miaou_widgets_layout.Toast_widget.enqueue current severity msg)

let toast_info msg = push_toast Info msg

let toast_success msg = push_toast Success msg

let toast_warn msg = push_toast Warn msg

let toast_error msg = push_toast Error msg

let tick_toasts () =
  Mutex.lock toasts_lock ;
  Fun.protect
    ~finally:(fun () -> Mutex.unlock toasts_lock)
    (fun () -> toasts := Miaou_widgets_layout.Toast_widget.tick !toasts)

let render_toasts ~cols =
  Mutex.lock toasts_lock ;
  Fun.protect
    ~finally:(fun () -> Mutex.unlock toasts_lock)
    (fun () -> Miaou_widgets_layout.Toast_widget.render !toasts ~cols)

(* Global spinner for loading states *)
let spinner = ref (Miaou_widgets_layout.Spinner_widget.open_centered ())

let tick_spinner () =
  spinner := Miaou_widgets_layout.Spinner_widget.tick !spinner

let render_spinner label =
  Miaou_widgets_layout.Spinner_widget.render
    (Miaou_widgets_layout.Spinner_widget.set_label !spinner (Some label))

(* Progress bar for long-running actions *)
module Progress_widget = Miaou_widgets_layout.Progress_widget

type progress_state = {
  widget : Progress_widget.t;
  started_at : float;
  estimate_secs : float;
  label : string option;
  finished_at : float option;
  manual_progress : float option;
}

let progress : progress_state option ref = ref None

let progress_linger_secs = 2.0

let progress_start ~label ~estimate_secs ~width =
  let widget = Progress_widget.open_inline ~width ~label () in
  progress :=
    Some
      {
        widget;
        started_at = Unix.gettimeofday ();
        estimate_secs;
        label = Some label;
        finished_at = None;
        manual_progress = None;
      }

let progress_finish () =
  progress :=
    Option.map
      (fun p ->
        let widget =
          p.widget |> fun w ->
          Progress_widget.set_label w p.label |> fun w ->
          Progress_widget.set_progress w 1.0
        in
        {
          p with
          widget;
          finished_at = Some (Unix.gettimeofday ());
          manual_progress = Some 1.0;
        })
      !progress

let progress_set ?label ~progress:ratio () =
  let ratio = max 0.0 (min 1.0 ratio) in
  progress :=
    Option.map
      (fun p ->
        let widget =
          p.widget |> fun w ->
          Progress_widget.set_label
            w
            (match label with Some l -> Some l | None -> p.label)
          |> fun w -> Progress_widget.set_progress w ratio
        in
        {
          p with
          widget;
          label = (match label with Some l -> Some l | None -> p.label);
          manual_progress = Some ratio;
        })
      !progress

let tick_progress () =
  progress :=
    Option.map
      (fun p ->
        let now = Unix.gettimeofday () in
        match p.finished_at with
        | Some finished when now -. finished > progress_linger_secs -> p
        | _ ->
            let ratio =
              match p.manual_progress with
              | Some r -> r
              | None ->
                  let elapsed = now -. p.started_at in
                  if p.estimate_secs <= 0. then 0.0
                  else min 1.0 (elapsed /. p.estimate_secs)
            in
            let widget =
              p.widget |> fun w ->
              Progress_widget.set_label w p.label |> fun w ->
              Progress_widget.set_progress w ratio
            in
            {p with widget})
      !progress

let render_progress ~cols =
  tick_progress () ;
  match !progress with
  | None -> ""
  | Some p -> (
      let now = Unix.gettimeofday () in
      match p.finished_at with
      | Some finished when now -. finished > progress_linger_secs ->
          progress := None ;
          ""
      | _ -> Progress_widget.render ~cols p.widget)
