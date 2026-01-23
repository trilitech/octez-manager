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

(** Multi-file progress for binary downloads *)

type speed_sample = {timestamp : float; bytes : int64}

type speed_tracker = {
  samples : speed_sample list; (* Keep last 5 samples *)
  current_speed : float option; (* MB/s *)
}

type binary_progress = {
  name : string;
  widget : Progress_widget.t;
  downloaded : int64 option;
  total : int64 option;
  status : [`Pending | `InProgress | `Complete];
  speed_tracker : speed_tracker;
}

type multi_progress_state = {
  version : string;
  binaries : binary_progress list;
  finished_at : float option;
  checksum_message : string option;
}

let multi_progress : multi_progress_state option ref = ref None

let multi_progress_linger_secs = 3.0

(* Helper: calculate download speed from samples *)
let calculate_speed tracker =
  match tracker.samples with
  | [] | [_] -> None
  | samples ->
      let first = List.hd samples in
      let last = List.hd (List.rev samples) in
      let time_diff = last.timestamp -. first.timestamp in
      let bytes_diff = Int64.sub last.bytes first.bytes in
      if time_diff > 0.1 then
        (* MB/s = bytes/sec / 1024 / 1024 *)
        Some (Int64.to_float bytes_diff /. time_diff /. 1024. /. 1024.)
      else None

(* Helper: add sample and update speed *)
let update_speed_tracker tracker ~downloaded =
  let now = Unix.gettimeofday () in
  let new_sample = {timestamp = now; bytes = downloaded} in
  (* Keep last 5 samples for smoothing *)
  let new_samples =
    new_sample :: tracker.samples |> fun l -> List.filteri (fun i _ -> i < 5) l
  in
  let new_speed =
    calculate_speed {samples = new_samples; current_speed = None}
  in
  {samples = new_samples; current_speed = new_speed}

(* Helper: format file size *)
let format_size bytes =
  let open Int64 in
  let kb = div bytes 1024L in
  let mb = div kb 1024L in
  let gb = div mb 1024L in
  if gb > 0L then Printf.sprintf "%Ld GB" gb
  else if mb > 0L then Printf.sprintf "%Ld MB" mb
  else if kb > 0L then Printf.sprintf "%Ld KB" kb
  else Printf.sprintf "%Ld bytes" bytes

(* Helper: format binary name (15 chars, truncate with ellipsis if longer) *)
let format_binary_name name =
  let max_len = 15 in
  if String.length name <= max_len then Printf.sprintf "%-15s" name
  else
    let truncated = String.sub name 0 (max_len - 3) in
    Printf.sprintf "%s..." truncated

(* Helper: format status icon *)
let format_status_icon status =
  match status with
  | `Complete -> "[\xe2\x9c\x93]" (* [✓] *)
  | `InProgress -> "[\xe2\x86\x92]" (* [→] *)
  | `Pending -> "[ ]"

(* Start multi-progress *)
let multi_progress_start ~version ~binaries =
  let widgets =
    List.map
      (fun name ->
        {
          name;
          widget = Progress_widget.open_inline ~width:30 ();
          downloaded = None;
          total = None;
          status = `Pending;
          speed_tracker = {samples = []; current_speed = None};
        })
      binaries
  in
  multi_progress :=
    Some
      {version; binaries = widgets; finished_at = None; checksum_message = None}

(* Update progress for a specific binary *)
let multi_progress_update ~binary ~downloaded ~total =
  multi_progress :=
    Option.map
      (fun mp ->
        let binaries =
          List.map
            (fun bp ->
              if bp.name = binary then
                let pct =
                  match total with
                  | Some t when t > 0L ->
                      Int64.(to_float downloaded /. to_float t)
                  | _ -> 0.0
                in
                let new_tracker =
                  update_speed_tracker bp.speed_tracker ~downloaded
                in
                {
                  bp with
                  widget = Progress_widget.set_progress bp.widget pct;
                  downloaded = Some downloaded;
                  total;
                  status = `InProgress;
                  speed_tracker = new_tracker;
                }
              else bp)
            mp.binaries
        in
        {mp with binaries})
      !multi_progress

(* Mark a binary as complete *)
let multi_progress_complete ~binary ~size =
  multi_progress :=
    Option.map
      (fun mp ->
        let binaries =
          List.map
            (fun bp ->
              if bp.name = binary then
                {
                  bp with
                  widget = Progress_widget.set_progress bp.widget 1.0;
                  downloaded = Some size;
                  total = Some size;
                  status = `Complete;
                }
              else bp)
            mp.binaries
        in
        {mp with binaries})
      !multi_progress

(* Set checksum message *)
let multi_progress_checksum msg =
  multi_progress :=
    Option.map (fun mp -> {mp with checksum_message = Some msg}) !multi_progress

(* Finish multi-progress *)
let multi_progress_finish () =
  multi_progress :=
    Option.map
      (fun mp -> {mp with finished_at = Some (Unix.gettimeofday ())})
      !multi_progress

(* Render multi-progress display *)
let render_multi_progress ~cols:_ =
  match !multi_progress with
  | None -> ""
  | Some mp -> (
      let now = Unix.gettimeofday () in
      match mp.finished_at with
      | Some finished when now -. finished > multi_progress_linger_secs ->
          multi_progress := None ;
          ""
      | _ ->
          let lines = ref [] in
          (* Header *)
          lines :=
            Printf.sprintf "Downloading Octez v%s..." mp.version :: !lines ;
          lines := "" :: !lines ;
          (* Render each binary *)
          List.iter
            (fun bp ->
              let icon = format_status_icon bp.status in
              let name = format_binary_name bp.name in
              let bar = Progress_widget.render ~cols:80 bp.widget in
              (* Build size info *)
              let size_info =
                match
                  (bp.downloaded, bp.total, bp.speed_tracker.current_speed)
                with
                | Some dl, Some t, Some speed ->
                    Printf.sprintf
                      " (%s / %s) @ %.1f MB/s"
                      (format_size dl)
                      (format_size t)
                      speed
                | Some dl, Some t, None ->
                    Printf.sprintf " (%s / %s)" (format_size dl) (format_size t)
                | Some dl, None, Some speed ->
                    Printf.sprintf " (%s) @ %.1f MB/s" (format_size dl) speed
                | Some dl, None, None -> Printf.sprintf " (%s)" (format_size dl)
                | _ -> ""
              in
              let line = Printf.sprintf "%s %s %s%s" icon name bar size_info in
              lines := line :: !lines)
            mp.binaries ;
          (* Checksum message if present *)
          (match mp.checksum_message with
          | Some msg ->
              lines := "" :: !lines ;
              lines := msg :: !lines
          | None -> ()) ;
          String.concat "\n" (List.rev !lines))
