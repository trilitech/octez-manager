(*****************************************************************************)
(*                                                                           *)
(* SPDX-License-Identifier: MIT                                              *)
(* Copyright (c) 2025 Nomadic Labs <contact@nomadic-labs.com>                *)
(*                                                                           *)
(*****************************************************************************)

module Widgets = Miaou_widgets_display.Widgets
module Progress_widget = Miaou_widgets_layout.Progress_widget
module Keys = Miaou.Core.Keys

type step_status = Pending | InProgress | Complete | Failed

type install_step = {
  id : string;
  label : string;
  mutable status : step_status;
}

type state = {
  steps : install_step array;
  current_step : int ref;
  progress : float ref;
  show_logs : bool ref;
  logs : string list ref;
  max_log_lines : int;
}

(* Global modal state ref for updates from background thread *)
let current_modal_state : state option ref = ref None

type msg = unit

let create_steps has_snapshot =
  if has_snapshot then
    [|
      {id = "setup"; label = "Setup directories and config"; status = Pending};
      {
        id = "download";
        label = "Download snapshot";
        status = Pending;
      };
      {id = "import"; label = "Import snapshot"; status = Pending};
      {id = "configure"; label = "Configure service"; status = Pending};
    |]
  else
    [|
      {id = "setup"; label = "Setup directories and config"; status = Pending};
      {id = "configure"; label = "Configure service"; status = Pending};
    |]

let init ~has_snapshot () =
  {
    steps = create_steps has_snapshot;
    current_step = ref 0;
    progress = ref 0.0;
    show_logs = ref false;
    logs = ref [];
    max_log_lines = 10;
  }

let update s _ = s

let view s ~focus:_ ~size =
  let open Widgets in
  let width = min 70 (size.LTerm_geom.cols - 4) in
  let header = [title_highlight " Installing Node Instance "; ""] in
  (* Render checklist *)
  let checklist =
    Array.to_list s.steps
    |> List.mapi (fun i step ->
        let marker =
          match step.status with
          | Pending -> dim "○"
          | InProgress -> fg 33 "◐"
          | Complete -> fg 22 "✓"
          | Failed -> fg 160 "✗"
        in
        let label =
          match step.status with
          | InProgress -> bold step.label
          | Complete -> dim step.label
          | Failed -> fg 160 step.label
          | Pending -> step.label
        in
        Printf.sprintf "  %s %s" marker label)
  in
  (* Get current step label for progress bar *)
  let current_label =
    if !(s.current_step) < Array.length s.steps then
      s.steps.(!(s.current_step)).label
    else if Array.length s.steps > 0 then s.steps.(Array.length s.steps - 1).label
    else "Installing..."
  in
  (* Render progress bar *)
  let progress_widget =
    Progress_widget.open_inline ~width:48 ~label:current_label ()
  in
  let progress_widget =
    Progress_widget.set_progress progress_widget !(s.progress)
  in
  let progress_str = Progress_widget.render ~cols:width progress_widget in
  (* Render log output if enabled *)
  let log_section =
    if !(s.show_logs) then
      let recent_logs =
        List.rev !(s.logs)
        |> (fun l ->
             if List.length l > s.max_log_lines then
               List.filteri (fun i _ -> i < s.max_log_lines) l
             else l)
        |> List.rev
      in
      [""; dim "Recent logs:"]
      @ List.map (fun line -> dim ("  " ^ line)) recent_logs
    else []
  in
  let footer =
    [
      "";
      dim
        (if !(s.show_logs) then "l: hide logs  Esc: cancel (background)"
         else "l: show logs  Esc: cancel (background)");
    ]
  in
  String.concat "\n" (header @ checklist @ [""; progress_str] @ log_section @ footer)

let move s _ = s

let refresh s = s

let enter s = s

let service_select s _ = s

let service_cycle s _ = s

let back s = s

let keymap _ = []

let handled_keys () = [Keys.Char "l"; Keys.Char "L"; Keys.Escape]

let toggle_logs s =
  s.show_logs := not !(s.show_logs) ;
  s

let handle_modal_key s key ~size:_ =
  match Keys.of_string key with
  | Some (Keys.Char "l") | Some (Keys.Char "L") -> toggle_logs s
  | Some Keys.Escape ->
      (* Allow cancelling but installation continues in background *)
      Miaou.Core.Modal_manager.close_top `Cancel ;
      s
  | _ -> s

let handle_key = handle_modal_key

let next_page _ = None

let has_modal _ = true

(* Public API for updating progress *)
let set_step_complete s step_id =
  let state = match !current_modal_state with Some st -> st | None -> s in
  Array.iteri
    (fun i step ->
      if step.id = step_id then (
        step.status <- Complete ;
        state.current_step := i + 1 ;
        if i + 1 < Array.length state.steps then
          state.steps.(i + 1).status <- InProgress ;
        state.progress :=
          float_of_int (i + 1) /. float_of_int (Array.length state.steps)))
    state.steps

let set_download_progress s pct =
  let state = match !current_modal_state with Some st -> st | None -> s in
  (* Find download step and update it *)
  Array.iteri
    (fun i step ->
      if step.id = "download" && step.status = InProgress then
        let base = float_of_int i in
        state.progress :=
          (base +. (Float.of_int pct /. 100.0))
          /. float_of_int (Array.length state.steps))
    state.steps

let add_log_line s line =
  let state = match !current_modal_state with Some st -> st | None -> s in
  let trimmed = String.trim line in
  if trimmed <> "" then state.logs := trimmed :: !(state.logs)

(* Open the modal *)
let open_modal ~has_snapshot () =
  let module Modal = struct
    type nonrec state = state

    type nonrec msg = msg

    let init = init ~has_snapshot

    let update = update

    let view = view

    let move = move

    let refresh = refresh

    let enter = enter

    let service_select = service_select

    let service_cycle = service_cycle

    let back = back

    let keymap = keymap

    let handled_keys = handled_keys

    let handle_modal_key = handle_modal_key

    let handle_key = handle_key

    let next_page = next_page

    let has_modal = has_modal
  end in
  let modal_state = init ~has_snapshot () in
  (* Mark first step as in progress *)
  if Array.length modal_state.steps > 0 then
    modal_state.steps.(0).status <- InProgress ;
  (* Store in global ref for background thread access *)
  current_modal_state := Some modal_state ;
  let ui : Miaou.Core.Modal_manager.ui =
    {
      title = "Installing Node";
      left = None;
      max_width = Some 76;
      dim_background = true;
    }
  in
  Miaou.Core.Modal_manager.push_default
    (module Modal)
    ~init:modal_state
    ~ui
    ~on_close:(fun _ _ -> current_modal_state := None) ;
  modal_state
