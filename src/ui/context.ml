let pending_instance_detail : string option ref = ref None

let instances_dirty = ref false

let set_pending_instance_detail inst = pending_instance_detail := Some inst

let take_pending_instance_detail () =
  let value = !pending_instance_detail in
  pending_instance_detail := None ;
  value

let mark_instances_dirty () = instances_dirty := true

let consume_instances_dirty () =
  let flag = !instances_dirty in
  instances_dirty := false ;
  flag

let pending_navigation : string option ref = ref None

let navigate page = pending_navigation := Some page

let consume_navigation () =
  let value = !pending_navigation in
  pending_navigation := None ;
  value

(* Global toast state *)
let toasts =
  ref (Miaou_widgets_layout.Toast_widget.empty ~position:`Bottom_right ())

(* Avoid duplicate consecutive toasts with the same severity/message. *)
let push_toast severity msg =
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
    toasts := Miaou_widgets_layout.Toast_widget.enqueue current severity msg

let toast_info msg = push_toast Info msg

let toast_success msg = push_toast Success msg

let toast_warn msg = push_toast Warn msg

let toast_error msg = push_toast Error msg

let tick_toasts () = toasts := Miaou_widgets_layout.Toast_widget.tick !toasts

let render_toasts ~cols = Miaou_widgets_layout.Toast_widget.render !toasts ~cols

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
