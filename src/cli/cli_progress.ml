(******************************************************************************)
(*                                                                            *)
(* SPDX-License-Identifier: MIT                                               *)
(* Copyright (c) 2026 Nomadic Labs <contact@nomadic-labs.com>                 *)
(*                                                                            *)
(******************************************************************************)

(** Standalone progress bar rendering for CLI output.
    
    Provides multi-line progress display for tracking multiple concurrent
    or sequential downloads without requiring TUI framework dependencies. *)

(** Progress bar rendering style *)
type style = ASCII | Unicode

(** Detect appropriate style based on terminal capabilities *)
let detect_style () =
  match Sys.getenv_opt "TERM" with
  | Some term when String.length term > 0 && term <> "dumb" -> (
      match Sys.getenv_opt "LANG" with
      | Some lang
        when String.lowercase_ascii lang |> fun s -> String.contains s 'u' ->
          Unicode
      | _ -> ASCII)
  | _ -> ASCII

(** Status of a binary download *)
type binary_status =
  | Pending
  | InProgress of {downloaded : int64; total : int64 option}
  | Complete of {size : int64}

(** State of multi-binary download display *)
type display_state = {
  binaries : (string * binary_status) list;
  style : style;
  lines_printed : int;
  checksum_status : string option;
}

(** Render a progress bar
    
    @param width Width in characters (e.g., 20)
    @param percentage Progress as 0.0 to 100.0
    @param style ASCII or Unicode rendering *)
let render_progress_bar ~width ~percentage ~style =
  let pct = max 0.0 (min 100.0 percentage) in
  let filled = int_of_float (float_of_int width *. pct /. 100.0) in
  let empty = width - filled in
  match style with
  | Unicode ->
      (* █ = U+2588 Full Block, ░ = U+2591 Light Shade *)
      let filled_str =
        String.concat "" (List.init filled (fun _ -> "\xe2\x96\x88"))
      in
      let empty_str =
        String.concat "" (List.init empty (fun _ -> "\xe2\x96\x91"))
      in
      filled_str ^ empty_str
  | ASCII -> String.make filled '=' ^ String.make empty '-'

(** Format file size in human-readable format *)
let format_size bytes =
  let kb = Int64.div bytes 1024L in
  let mb = Int64.div kb 1024L in
  let gb = Int64.div mb 1024L in
  if gb > 0L then Printf.sprintf "%Ld GB" gb
  else if mb > 0L then Printf.sprintf "%Ld MB" mb
  else if kb > 0L then Printf.sprintf "%Ld KB" kb
  else Printf.sprintf "%Ld bytes" bytes

(** Render a single binary line
    
    @param name Binary name (e.g., "octez-node")
    @param status Current download status
    @param bar_width Width of progress bar in characters
    @param style Rendering style *)
let render_binary_line ~name ~status ~bar_width ~style =
  match status with
  | Pending ->
      let bar =
        if style = Unicode then
          String.concat "" (List.init bar_width (fun _ -> "\xe2\x96\x91"))
        else String.make bar_width '-'
      in
      Printf.sprintf "[ ] %-15s %s   0%%" name bar
  | InProgress {downloaded; total = Some t} when t > 0L ->
      let pct = Int64.(to_float downloaded /. to_float t *. 100.0) in
      let bar = render_progress_bar ~width:bar_width ~percentage:pct ~style in
      (* → = U+2192 Rightwards Arrow *)
      Printf.sprintf
        "[\xe2\x86\x92] %-15s %s %3.0f%% (%s / %s)"
        name
        bar
        pct
        (format_size downloaded)
        (format_size t)
  | InProgress {downloaded; total = None} ->
      let bar =
        if style = Unicode then
          String.concat "" (List.init bar_width (fun _ -> "\xe2\x96\x91"))
        else String.make bar_width '-'
      in
      Printf.sprintf
        "[\xe2\x86\x92] %-15s %s      (%s)"
        name
        bar
        (format_size downloaded)
  | InProgress {downloaded; total = Some _} ->
      (* total is 0 - shouldn't happen but handle gracefully *)
      let bar =
        if style = Unicode then
          String.concat "" (List.init bar_width (fun _ -> "\xe2\x96\x91"))
        else String.make bar_width '-'
      in
      Printf.sprintf
        "[\xe2\x86\x92] %-15s %s      (%s)"
        name
        bar
        (format_size downloaded)
  | Complete {size} ->
      let bar =
        if style = Unicode then
          String.concat "" (List.init bar_width (fun _ -> "\xe2\x96\x88"))
        else String.make bar_width '='
      in
      (* ✓ = U+2713 Check Mark *)
      Printf.sprintf
        "[\xe2\x9c\x93] %-15s %s 100%% (%s)"
        name
        bar
        (format_size size)

(** Render the entire multi-line display
    
    Uses ANSI escape sequences to move cursor up and overwrite previous output.
    Returns the number of lines printed. *)
let render_display state =
  let bar_width = 20 in
  (* Move cursor up if we've printed before *)
  if state.lines_printed > 0 then Printf.printf "\x1b[%dA" state.lines_printed ;
  (* Clear and render header *)
  Printf.printf "\x1b[K\n" ;
  (* Render each binary *)
  List.iter
    (fun (name, status) ->
      let line =
        render_binary_line ~name ~status ~bar_width ~style:state.style
      in
      Printf.printf "\x1b[K%s\n" line)
    state.binaries ;
  (* Render checksum status if present *)
  (match state.checksum_status with
  | Some msg -> Printf.printf "\x1b[K%s\n" msg
  | None -> ()) ;
  Printf.printf "%!" ;
  (* Return new line count *)
  let new_lines =
    1 + List.length state.binaries
    + if state.checksum_status <> None then 1 else 0
  in
  new_lines

(** Initialize display state for a list of binaries *)
let init_display binaries =
  let style = detect_style () in
  {
    binaries = List.map (fun name -> (name, Pending)) binaries;
    style;
    lines_printed = 0;
    checksum_status = None;
  }

(** Update the status of a specific binary *)
let update_binary state ~binary ~status =
  {
    state with
    binaries =
      List.map
        (fun (name, old_status) ->
          if name = binary then (name, status) else (name, old_status))
        state.binaries;
  }

(** Mark a binary as in progress *)
let set_in_progress state ~binary ~downloaded ~total =
  update_binary state ~binary ~status:(InProgress {downloaded; total})

(** Mark a binary as complete *)
let set_complete state ~binary ~size =
  update_binary state ~binary ~status:(Complete {size})

(** Set checksum verification status message *)
let set_checksum_status state msg = {state with checksum_status = Some msg}

(** Clear checksum status *)
let clear_checksum_status state = {state with checksum_status = None}
