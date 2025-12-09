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
