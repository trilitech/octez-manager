(*****************************************************************************)
(*                                                                           *)
(* SPDX-License-Identifier: MIT                                              *)
(* Copyright (c) 2025 Nomadic Labs <contact@nomadic-labs.com>                *)
(*                                                                           *)
(*****************************************************************************)
[@@@warning "-32-34-37-69"]

[@@@coverage off]

module Logger_capability = Miaou_interfaces.Logger_capability
open Miaou_core.Tui_page
module Navigation = Miaou_core.Navigation
module Capture = Miaou_core.Tui_capture
module Khs = Miaou_internals.Key_handler_stack
module Modal_manager = Miaou_core.Modal_manager
module Narrow_modal = Miaou_core.Narrow_modal
module Quit_flag = Miaou_core.Quit_flag
module Help_hint = Miaou_core.Help_hint
module Driver_common = Miaou_driver_common.Driver_common
module Fibers = Miaou_helpers.Fiber_runtime
module Helpers = Miaou_helpers.Helpers

(* Persistent session flags *)
let narrow_warned = ref false

(* Debug overlay - shows TPS when MIAOU_OVERLAY is set *)
let overlay_enabled =
  lazy
    (match Sys.getenv_opt "MIAOU_OVERLAY" with
    | Some ("1" | "true" | "TRUE" | "yes" | "YES") -> true
    | _ -> false)

type fps_tracker = {
  mutable loop_count : int;
  mutable render_count : int;
  mutable last_time : float;
  mutable current_loop_fps : float;
  mutable current_render_fps : float;
  mutable current_tps : float;
}

let create_fps_tracker () =
  {
    loop_count = 0;
    render_count = 0;
    last_time = Unix.gettimeofday ();
    current_loop_fps = 0.0;
    current_render_fps = 0.0;
    current_tps = 0.0;
  }

let update_loop_fps tracker =
  tracker.loop_count <- tracker.loop_count + 1 ;
  let now = Unix.gettimeofday () in
  let elapsed = now -. tracker.last_time in
  if elapsed >= 1.0 then begin
    tracker.current_loop_fps <- float_of_int tracker.loop_count /. elapsed ;
    tracker.current_render_fps <- float_of_int tracker.render_count /. elapsed ;
    tracker.current_tps <- tracker.current_loop_fps ;
    tracker.loop_count <- 0 ;
    tracker.render_count <- 0 ;
    tracker.last_time <- now
  end

let record_render tracker = tracker.render_count <- tracker.render_count + 1

let render_overlay_ansi ~loop_fps ~render_fps ~tps ~cols =
  (* Render "lterm L:XX R:XX T:XX" in top-right corner with dim style
     L = Loop FPS (cap), R = Render FPS (actual), T = TPS *)
  let text =
    Printf.sprintf "lterm L:%.0f R:%.0f T:%.0f" loop_fps render_fps tps
  in
  let len = String.length text in
  let start_col = cols - len - 1 in
  if start_col > 0 then
    (* Move to row 1, col start_col, dim style, print, reset, clear rest of line *)
    Printf.sprintf "\027[1;%dH\027[2;38;5;245m%s\027[0m\027[K" start_col text
  else ""

(* Split lines while preserving a trailing empty line when the input ends with '\n'.
   This keeps row counts consistent for incremental re-rendering. *)
let split_lines_preserve s =
  let lines = String.split_on_char '\n' s in
  if String.length s > 0 && s.[String.length s - 1] = '\n' then
    Array.of_list (lines @ [""])
  else Array.of_list lines

(* Render only the lines that changed compared to the previous frame.
   Uses absolute cursor positioning + ESC[K to clear rest of line, avoiding
   a full-screen wipe that causes flicker over slow SSH links. *)
let render_diff ~rows last_lines next_lines =
  let buf = Buffer.create 1024 in
  let last_len = Array.length last_lines in
  let next_len = Array.length next_lines in
  let max_lines = min rows (max last_len next_len) in
  for row = 0 to max_lines - 1 do
    let prev = if row < last_len then Some last_lines.(row) else None in
    let next = if row < next_len then Some next_lines.(row) else None in
    if prev <> next then (
      Buffer.add_string buf (Printf.sprintf "\027[%d;1H" (row + 1)) ;
      match next with
      | Some line ->
          Buffer.add_string buf line ;
          Buffer.add_string buf "\027[K"
      | None -> Buffer.add_string buf "\027[K")
  done ;
  Buffer.contents buf

module LT = LTerm

type t = private T

let available = true

let size () = (Obj.magic 0 : t)

module Events = Term_events

type driver_key = Events.driver_key =
  | Quit
  | Refresh
  | Enter
  | NextPage
  | PrevPage
  | Up
  | Down
  | Left
  | Right
  | Other of string

let clear = Events.clear

let run_with_key_source_for_tests = Term_test_runner.run_with_key_source

let run (initial_page : (module PAGE_SIG)) : [`Quit | `SwitchTo of string] =
  let run_with_page (module Page : PAGE_SIG) =
    Fibers.with_page_switch (fun _env _page_sw ->
        (* Ensure widgets render with terminal-friendly glyphs when using the lambda-term backend. *)
        Miaou_widgets_display.Widgets.set_backend `Terminal ;
        let fd, enter_raw, cleanup, install_signal_handlers, signal_exit_flag =
          Term_terminal_setup.setup_and_cleanup ()
        in
        let () = at_exit cleanup in
        install_signal_handlers () ;
        (* Track terminal resizes via SIGWINCH to force immediate refresh. *)
        let resize_pending = Atomic.make false in
        (try
           (* Linux SIGWINCH is 28; Sys doesn't expose a constant on all versions. *)
           let sigwinch = 28 in
           Sys.set_signal
             sigwinch
             (Sys.Signal_handle
                (fun _ ->
                  Term_size_detection.invalidate_cache () ;
                  Atomic.set resize_pending true))
         with _ -> ()) ;

        (* Cache the last rendered frame to avoid unnecessary redraws (reduces flicker). *)
        let last_out_ref = ref "" in
        let last_lines_ref : string array ref = ref [||] in
        (* Track last known terminal size and detect changes by polling on each
    render tick. This avoids depending on SIGWINCH being available at
    compile-time across different platforms. *)
        let last_size = ref {LTerm_geom.rows = 24; cols = 80} in
        (* Cache the last base view seen when a modal is active so we don't keep
           re-rendering the background on every modal keystroke (reduces flicker). *)
        let modal_base_ref : string option ref = ref None in
        (* FPS tracker for debug overlay *)
        let fps_tracker = create_fps_tracker () in
        (* Portable size detection used by render and key handlers.
       First, try lambda-term directly (in-process, no subprocess TTY issues).
       Then fall back to external tools. Avoid touching stdin to not interfere
       with input handling. *)
        let detect_size = Term_size_detection.detect_size in
        let footer_ref : string option ref = ref None in
        let clear_and_render ps key_stack =
          (* Log driver render tick using the Miaou TUI logger if available. *)
          (match Logger_capability.get () with
          | Some logger when Sys.getenv_opt "MIAOU_DEBUG" = Some "1" ->
              logger.logf Debug "DRIVER: clear_and_render tick"
          | _ -> ()) ;
          (* Build footer from key handler stack top frame bindings if available. *)
          let size = detect_size () in
          (* Persistent narrow banner: show a small header warning on every render while cols < 80. *)
          let header_lines =
            if size.cols < 80 then
              [
                Miaou_widgets_display.Widgets.warning_banner
                  ~cols:size.cols
                  (Printf.sprintf
                     "Narrow terminal: %d cols (< 80). Some UI may be \
                      truncated."
                     size.cols);
              ]
            else []
          in
          (* One-time narrow terminal warning (only once per session). *)
          (* Trigger warning when starting narrow or when crossing from >=80 to <80. *)
          let prev_cols = !last_size.LTerm_geom.cols in
          if
            (size.cols < 80 && not !narrow_warned)
            || (size.cols < 80 && prev_cols >= 80 && not !narrow_warned)
          then (
            (* Structured log for width crossing / initial narrow state *)
            (match Logger_capability.get () with
            | Some logger ->
                logger.logf
                  Warning
                  (Printf.sprintf
                     "WIDTH_CROSSING: prev=%d new=%d (showing narrow modal)"
                     prev_cols
                     size.cols)
            | None -> ()) ;
            narrow_warned := true ;
            Modal_manager.push
              (module Narrow_modal.Page)
              ~init:(Narrow_modal.Page.init ())
              ~ui:
                {
                  title = "Narrow terminal";
                  left = Some 2;
                  max_width = None;
                  dim_background = true;
                }
              ~commit_on:[]
              ~cancel_on:[]
              ~on_close:(fun
                  (_ : Narrow_modal.Page.state Miaou_core.Navigation.t) _ -> ()) ;
            (* Mark the next key as consumed so Enter/Esc won't propagate. *)
            Modal_manager.set_consume_next_key () ;
            (* Auto-dismiss after 5s. We only close the modal if it's still the
           same top modal title to avoid racing with other modals. *)
            let my_title = "Narrow terminal" in
            Fibers.spawn (fun env ->
                Eio.Time.sleep env#clock 5.0 ;
                match Modal_manager.top_title_opt () with
                | Some t when t = my_title -> Modal_manager.close_top `Cancel
                | _ -> ())) ;
          (* If terminal geometry changed since last render, force a redraw and
      update the modal snapshot size so overlays render correctly. *)
          (* Log size changes for diagnostics and force a redraw when geometry changes. *)
          if
            size.LTerm_geom.rows <> !last_size.LTerm_geom.rows
            || size.LTerm_geom.cols <> !last_size.LTerm_geom.cols
          then (
            last_out_ref := "" ;
            last_lines_ref := [||]) ;
          (* Publish current size to modal machinery for overlays. *)
          Modal_manager.set_current_size
            size.LTerm_geom.rows
            size.LTerm_geom.cols ;
          let body = Page.view ps ~focus:true ~size in
          let title_opt =
            match String.index_opt body '\n' with
            | None -> None
            | Some idx -> Some (String.sub body 0 idx)
          in
          let main_out =
            match title_opt with
            | Some t when String.length t > 0 ->
                let wrapped_footer =
                  Miaou_widgets_display.Widgets.footer_hints_wrapped_capped
                    ~cols:size.cols
                    ~max_lines:3
                    (Khs.top_bindings key_stack)
                in
                Miaou_widgets_display.Widgets.render_frame
                  ~title:t
                  ~header:header_lines
                  ~cols:size.cols
                  ~body:
                    (String.sub
                       body
                       (min (String.length body) (String.length t + 1))
                       (max 0 (String.length body - (String.length t + 1))))
                  ~footer:wrapped_footer
                  ()
            | _ ->
                let wrapped_footer =
                  Miaou_widgets_display.Widgets.footer_hints_wrapped_capped
                    ~cols:size.cols
                    ~max_lines:3
                    (Khs.top_bindings key_stack)
                in
                let buf =
                  Buffer.create
                    (String.length body + String.length wrapped_footer + 64)
                in
                (match header_lines with
                | [] -> ()
                | lst ->
                    Buffer.add_string buf (Helpers.concat_lines lst) ;
                    Buffer.add_char buf '\n') ;
                Buffer.add_string buf body ;
                Buffer.add_char buf '\n' ;
                Buffer.add_string buf wrapped_footer ;
                Buffer.contents buf
          in
          (* Keep a stable base frame while a modal is open to avoid repainting
             the entire background on each modal refresh. *)
          let base_for_modal =
            if Modal_manager.has_active () then (
              match !modal_base_ref with
              | Some b -> b
              | None ->
                  modal_base_ref := Some main_out ;
                  main_out)
            else (
              modal_base_ref := None ;
              main_out)
          in
          let out =
            Driver_common.Modal_utils.render_with_modal_overlay
              ~view:base_for_modal
              ~rows:size.rows
              ~cols:size.cols
          in
          (* Trim output to the current terminal rows so height resizing is respected.
       Keep the top of the output and the final footer line when possible. *)
          let max_rows = size.LTerm_geom.rows in
          let lines = String.split_on_char '\n' out in
          let out_trimmed =
            if List.length lines <= max_rows then out
            else
              let head_count = max 1 (max_rows - 1) in
              let rec take n lst =
                if n <= 0 then []
                else match lst with [] -> [] | x :: xs -> x :: take (n - 1) xs
              in
              let last_line = List.nth lines (List.length lines - 1) in
              let head = take head_count lines in
              Helpers.concat_lines (head @ [last_line])
          in
          (* Update FPS tracker *)
          update_loop_fps fps_tracker ;

          (* Write only when output changed; keeps the terminal stable and avoids flicker. *)
          Capture.record_frame
            ~rows:size.LTerm_geom.rows
            ~cols:size.LTerm_geom.cols
            out_trimmed ;
          let full_out = out_trimmed ^ "\n" in
          if full_out <> !last_out_ref then (
            record_render fps_tracker ;
            let next_lines = split_lines_preserve full_out in
            let diff =
              render_diff ~rows:size.LTerm_geom.rows !last_lines_ref next_lines
            in
            (* Move cursor home first to avoid depending on previous position. *)
            print_string ("\027[H" ^ diff) ;
            (* Render debug overlay if enabled *)
            if Lazy.force overlay_enabled then
              print_string
                (render_overlay_ansi
                   ~loop_fps:fps_tracker.current_loop_fps
                   ~render_fps:fps_tracker.current_render_fps
                   ~tps:fps_tracker.current_tps
                   ~cols:size.LTerm_geom.cols) ;
            Stdlib.flush stdout ;
            last_lines_ref := next_lines ;
            last_out_ref := full_out)
          else () ;
          (* Update last_size at end of render tick so next iteration compares
      against the previously displayed geometry. *)
          last_size := size
        in

        (* Pager notify mechanism: we avoid calling clear_and_render directly
     from background threads. Instead background appenders set an atomic
     flag which the main loop polls; when seen, the main loop performs the
     render from the main thread. This prevents unsafe cross-thread UI
     calls while keeping responsiveness. *)
        (* Use the common Pager_notify module for debounced background updates *)
        let pager_notifier =
          Driver_common.Pager_notify.create ~debounce_s:0.08 ()
        in

        (* Notification callback for pager widgets to request render when content changes.
       Applications creating pager widgets should pass this to ~notify_render parameter. *)
        let notify_render_from_pager_flag () =
          Driver_common.Pager_notify.notify pager_notifier
        in
        let () = ignore notify_render_from_pager_flag in

        (* Buffered reader using Unix.read: keep a pending string of bytes read from
       the fd. Block until at least one byte is available, then for ESC-starting
       sequences poll briefly to gather additional bytes so common CSI arrow
       sequences (ESC '[' A/B/C/D) are returned as a single token. *)
        (* We need to expose refs to the current page state and key_stack so the
     pager notifier can call clear_and_render with a consistent snapshot. *)
        let current_state_ref : Page.pstate option ref = ref None in
        (* key stack is always present (use empty as initial value) so notifier
      can dereference it without option handling. *)
        let current_key_stack_ref : Khs.t ref = ref Khs.empty in
        (* Handle for the page frame we push into the key handler stack, so we can replace it each loop. *)
        let page_frame_handle : Khs.handle option ref = ref None in
        let pending = ref "" in
        (* Helper: detect whether the transient narrow modal is currently active. *)
        let is_narrow_modal_active () =
          match Modal_manager.top_title_opt () with
          | Some t when t = "Narrow terminal" -> true
          | _ -> false
        in

        (* Helper: apply any pending navigation from modal callbacks to pstate *)
        let apply_pending_modal_nav ps =
          match Modal_manager.take_pending_navigation () with
          | Some page -> Navigation.goto page ps
          | None -> ps
        in

        (* Eio-aware input refill: uses short polling with Eio.Time.sleep to yield
       to scheduler and check for signals frequently. *)
        let refill timeout =
          let env, _ = Fibers.require_runtime () in
          let poll_interval = 0.05 in
          let deadline = Unix.gettimeofday () +. timeout in
          let rec poll_loop () =
            (* Check for signal exit first *)
            if Atomic.get signal_exit_flag then 0
            else
              let remaining = deadline -. Unix.gettimeofday () in
              if remaining <= 0.0 then 0
              else
                (* Non-blocking check for input *)
                let r, _, _ = Unix.select [fd] [] [] 0.0 in
                if r <> [] then
                  (* Data available, read it *)
                  let b = Bytes.create 256 in
                  try
                    let n = Unix.read fd b 0 256 in
                    if n <= 0 then 0
                    else (
                      pending := !pending ^ Bytes.sub_string b 0 n ;
                      n)
                  with Unix.Unix_error (Unix.EINTR, _, _) -> poll_loop ()
                else (
                  (* No data, sleep briefly and retry *)
                  Eio.Time.sleep env#clock (min poll_interval remaining) ;
                  poll_loop ())
          in
          try poll_loop () with
          | Unix.Unix_error (Unix.EINTR, _, _) -> 0
          | Eio.Cancel.Cancelled _ -> 0
        in

        (* ASCII keycodes as named constants for clarity *)
        let esc_keycode = 27 in
        let tab_keycode = 9 in
        let backspace_keycode = 127 in

        (* Parse a key from a buffer string without consuming it.
       
       This shared parsing logic is used by both peek_next_key (non-consuming)
       and read_key_blocking (consuming). Returns None if the buffer is empty
       or contains an incomplete escape sequence.
       
       Handles:
       - Simple keys (Tab, Enter, Backspace, printable chars, Ctrl+letter)
       - ESC sequences for arrow keys (ESC [ A/B/C/D and ESC O A/B/C/D)
       - Mouse events (SGR and X10 formats)
       - Special sequences (Delete: ESC [ 3 ~)
       
       Note: This only *parses*, it does not consume bytes from the buffer. *)
        let parse_key_from_buffer buffer =
          if String.length buffer = 0 then None
          else
            let first = String.get buffer 0 in
            if Char.code first <> esc_keycode then
              (* Simple non-ESC key *)
              if first = '\000' then Some `Refresh
              else if first = '\n' || first = '\r' then Some `Enter
              else if Char.code first = tab_keycode then Some `NextPage
              else if Char.code first = backspace_keycode then
                Some (`Other "Backspace")
              else
                let code = Char.code first in
                if code >= 1 && code <= 26 then
                  (* Ctrl+letter: code 1='a', 2='b', etc. *)
                  let letter = Char.chr (code + 96) in
                  Some (`Other ("C-" ^ String.make 1 letter))
                else Some (`Other (String.make 1 first))
            else
              (* ESC sequence - need at least 3 chars for complete arrow keys *)
              let len = String.length buffer in
              if len >= 3 && String.get buffer 1 = '[' then
                let code = String.get buffer 2 in
                match code with
                | '<' ->
                    (* Mouse event (SGR format): needs more complex parsing *)
                    Some (`Other "")
                | 'M' ->
                    (* Mouse event (X10 format): needs more bytes *)
                    Some (`Other "")
                | 'A' -> Some `Up
                | 'B' -> Some `Down
                | 'C' -> Some `Right
                | 'D' -> Some `Left
                | '3' ->
                    (* Delete key: ESC [ 3 ~ *)
                    if len >= 4 && String.get buffer 3 = '~' then
                      Some (`Other "Delete")
                    else Some (`Other "3")
                | _ -> Some (`Other (String.make 1 code))
              else if len >= 3 && String.get buffer 1 = 'O' then
                let code = String.get buffer 2 in
                match code with
                | 'A' -> Some `Up
                | 'B' -> Some `Down
                | 'C' -> Some `Right
                | 'D' -> Some `Left
                | _ -> Some (`Other (String.make 1 code))
              else if len = 1 then
                (* Just ESC alone *)
                Some (`Other "Esc")
              else
                (* Incomplete ESC sequence *)
                None
        in

        (* Helper: Parse the next key from pending buffer without consuming it.
       Returns None if buffer is empty or incomplete sequence. *)
        let peek_next_key () = parse_key_from_buffer !pending in

        (* Drain consecutive identical navigation keys from the pending buffer.
       
       Problem: When users hold down arrow keys and release, the terminal's input
       buffer may contain dozens of identical key events. Processing each one leads
       to scroll lag - the UI continues scrolling for ~0.5s after key release.
       
       Solution: After receiving a navigation key (Up/Down/Left/Right/Tab),
       check the pending buffer for additional identical keys and skip them. This
       "coalescing" ensures we only process the final position, making the UI
       feel responsive.
       
       Implementation: Uses peek_next_key to inspect without consuming, then manually
       consumes the appropriate bytes (3 for ESC sequences, 1 for Tab, 4 for Delete).
       Returns the count of drained keys for debug logging.
       
       Note: We use refill(0.0) with zero timeout to avoid blocking - only drain
       what's already buffered, don't wait for more input.
       
       TODO: PrevPage (Shift-Tab) is defined in the key type but not currently
       parsed by read_key_blocking. Consider adding support or documenting why
       it's excluded (e.g., reserved for widget-level focus navigation). *)
        let drain_consecutive_nav_keys current_key =
          (* Determine bytes to consume for each navigation key type *)
          let bytes_to_consume_for_key k =
            match k with
            | `Up | `Down | `Left | `Right ->
                (* Arrow keys: ESC [ A/B/C/D or ESC O A/B/C/D - always 3 bytes *)
                Some 3
            | `NextPage ->
                (* Tab is a single byte (ASCII 9) *)
                Some 1
            | `Other "Delete" ->
                (* Delete: ESC [ 3 ~ - 4 bytes *)
                Some 4
            | _ -> None
          in
          match bytes_to_consume_for_key current_key with
          | None -> 0 (* Not a drainable navigation key *)
          | Some bytes_per_key ->
              let drained = ref 0 in
              let rec drain_loop () =
                (* Ensure any pending input is read into the buffer (non-blocking) *)
                ignore (refill 0.0) ;
                match peek_next_key () with
                | Some next when next = current_key ->
                    (* Found another identical key - consume it *)
                    if String.length !pending >= bytes_per_key then (
                      pending :=
                        String.sub
                          !pending
                          bytes_per_key
                          (String.length !pending - bytes_per_key) ;
                      drained := !drained + 1 ;
                      drain_loop ())
                    else ()
                | _ -> ()
              in
              drain_loop () ;
              !drained
        in

        (* Read next key or emit a periodic refresh tick when idle. *)
        let read_key_blocking () =
          try
            (* Check if signal handler requested exit *)
            if Atomic.get signal_exit_flag then `Quit
            else if
              (* Prioritize a pending resize event to redraw immediately. *)
              Atomic.get resize_pending
            then (
              Atomic.set resize_pending false ;
              `Refresh)
            else if
              (* If a background append requested a render, service it as a refresh
           tick but only when the debounce window has elapsed.
           This coalesces bursts from background threads. *)
              Driver_common.Pager_notify.should_refresh pager_notifier
            then (
              Driver_common.Pager_notify.mark_refreshed pager_notifier ;
              `Refresh)
            else if
              (* Check global render notification (used by widgets like validated_textbox) *)
              Miaou_helpers.Render_notify.should_render ()
            then `Refresh
            else (
              (* Ensure at least one byte: wait a short time; if none, emit a refresh tick to drive pages.
                 Use ~33ms timeout for 30 TPS refresh rate. *)
              if String.length !pending = 0 then ignore (refill 0.033) ;
              if String.length !pending = 0 then
                (* Inject a synthetic refresh marker into the pending buffer to signal an idle tick. *)
                pending := "\000" ^ !pending ;
              if String.length !pending = 0 then raise End_of_file ;
              let first = String.get !pending 0 in
              (* If not ESC, consume single byte and return it. *)
              if Char.code first <> 27 then (
                pending :=
                  if String.length !pending > 1 then
                    String.sub !pending 1 (String.length !pending - 1)
                  else "" ;
                if first = '\000' then `Refresh
                else if first = '\n' || first = '\r' then `Enter
                else if Char.code first = 9 then `NextPage
                else if Char.code first = 127 then `Other "Backspace"
                else
                  let code = Char.code first in
                  if code >= 1 && code <= 26 then
                    let letter = Char.chr (code + 96) in
                    `Other ("C-" ^ String.make 1 letter)
                  else `Other (String.make 1 first))
              else (
                (* first == ESC (27) *)
                (* Gather a short window for sequence completion. *)
                for _ = 1 to 5 do
                  if String.length !pending >= 3 then ()
                  else ignore (refill 0.02)
                done ;
                let len = String.length !pending in
                if len = 1 then (
                  pending := "" ;
                  `Other "Esc")
                else if len >= 3 && String.get !pending 1 = '[' then (
                  if
                    (* Handle SGR mouse: ESC [ < btn;col;row (M|m) *)
                    len >= 6 && String.get !pending 2 = '<'
                  then (
                    (* Read until trailing 'M' or 'm' arrives. *)
                    let rec ensure_full_mouse timeout =
                      if timeout <= 0 then ()
                      else
                        let l = String.length !pending in
                        if l > 0 then (
                          let last = String.get !pending (l - 1) in
                          if last = 'M' || last = 'm' then ()
                          else ignore (refill 0.02) ;
                          ensure_full_mouse (timeout - 1))
                        else (
                          ignore (refill 0.02) ;
                          ensure_full_mouse (timeout - 1))
                    in
                    ensure_full_mouse 20 ;
                    let seq = !pending in
                    (* Find terminating M/m *)
                    let l = String.length seq in
                    let term_idx =
                      let rec find i =
                        if i >= l then l - 1
                        else
                          let c = String.get seq i in
                          if c = 'M' || c = 'm' then i else find (i + 1)
                      in
                      find 0
                    in
                    let chunk = String.sub seq 0 (min (term_idx + 1) l) in
                    (* Consume chunk from pending *)
                    pending :=
                      if String.length seq > String.length chunk then
                        String.sub
                          seq
                          (String.length chunk)
                          (String.length seq - String.length chunk)
                      else "" ;
                    (* Parse ESC [ < btn;col;row (M|m) *)
                    let parsed =
                      try
                        if String.length chunk < 6 then None
                        else if String.get chunk 0 <> '\027' then None
                        else if String.get chunk 1 <> '[' then None
                        else if String.get chunk 2 <> '<' then None
                        else
                          let body =
                            String.sub chunk 3 (String.length chunk - 4)
                          in
                          (* body like: "b;c;rM" or "b;c;rm" *)
                          let lastc =
                            String.get chunk (String.length chunk - 1)
                          in
                          let parts = String.split_on_char ';' body in
                          match parts with
                          | [b; c; r] -> (
                              let btn = int_of_string_opt b in
                              let col = int_of_string_opt c in
                              let row =
                                let r' = String.sub r 0 (String.length r - 0) in
                                int_of_string_opt (String.trim r')
                              in
                              match (btn, col, row) with
                              | Some _btn, Some col, Some row ->
                                  Some (row, col, lastc)
                              | _ -> None)
                          | _ -> None
                      with _ -> None
                    in
                    match parsed with
                    | Some (row, col, lastc) ->
                        (match Logger_capability.get () with
                        | Some logger ->
                            logger.logf
                              Debug
                              (Printf.sprintf "MOUSE: row=%d col=%d" row col)
                        | None -> ()) ;
                        (* Emit click only on button release to avoid flooding on motion. *)
                        if lastc = 'm' then
                          `Other (Printf.sprintf "Mouse:%d:%d" row col)
                        else `Other "MouseMove"
                    | None -> `Other "Esc")
                  else
                    let code = String.get !pending 2 in
                    (* consume ESC,[,code *)
                    pending :=
                      if len > 3 then String.sub !pending 3 (len - 3) else "" ;
                    match code with
                    | 'M' ->
                        (* X10 mouse tracking: ESC [ M b x y, with x,y,btn encoded +32 *)
                        let rec ensure_bytes n timeout =
                          if timeout <= 0 then false
                          else if String.length !pending >= n then true
                          else (
                            ignore (refill 0.02) ;
                            ensure_bytes n (timeout - 1))
                        in
                        if ensure_bytes 3 20 then (
                          let _b = String.get !pending 0 in
                          let x = String.get !pending 1 in
                          let y = String.get !pending 2 in
                          pending :=
                            if String.length !pending > 3 then
                              String.sub !pending 3 (String.length !pending - 3)
                            else "" ;
                          let col = max 1 (Char.code x - 32) in
                          let row = max 1 (Char.code y - 32) in
                          (match Logger_capability.get () with
                          | Some logger ->
                              logger.logf
                                Debug
                                (Printf.sprintf
                                   "MOUSE_X10: row=%d col=%d"
                                   row
                                   col)
                          | None -> ()) ;
                          `Other (Printf.sprintf "Mouse:%d:%d" row col))
                        else `Other "Esc"
                    | 'A' -> `Up
                    | 'B' -> `Down
                    | 'C' -> `Right
                    | 'D' -> `Left
                    | '3' ->
                        (* Common Delete sequence: ESC [ 3 ~ *)
                        if
                          String.length !pending > 0
                          && String.get !pending 0 = '~'
                        then (
                          pending :=
                            if String.length !pending > 1 then
                              String.sub !pending 1 (String.length !pending - 1)
                            else "" ;
                          `Other "Delete")
                        else `Other "3"
                    | c -> `Other (String.make 1 c))
                else if len >= 2 && String.get !pending 1 = 'O' then (
                  let code =
                    if len >= 3 then String.get !pending 2 else '\000'
                  in
                  pending :=
                    if len > 2 then String.sub !pending 2 (len - 2) else "" ;
                  match code with
                  | 'A' -> `Up
                  | 'B' -> `Down
                  | 'C' -> `Right
                  | 'D' -> `Left
                  | '\000' -> `Other "Esc"
                  | c -> `Other (String.make 1 c))
                else if
                  (* ESC followed by other single byte: consume ESC and next byte if present *)
                  len >= 2
                then (
                  let second = String.get !pending 1 in
                  pending :=
                    if len > 2 then String.sub !pending 2 (len - 2) else "" ;
                  `Other (String.make 1 second))
                else (
                  pending := "" ;
                  `Other "Esc")))
          with End_of_file -> `Quit
        in

        let handle_key_like ps key key_stack =
          (* Compute current size for handlers so pages can react to geometry during key handling. *)
          let size = detect_size () in
          Modal_manager.set_current_size
            size.LTerm_geom.rows
            size.LTerm_geom.cols ;
          if Modal_manager.has_active () then (
            Modal_manager.handle_key key ;
            ps)
          else if Page.has_modal ps then (
            let ps' = Page.handle_modal_key ps key ~size in
            clear_and_render ps' key_stack ;
            ps')
          else Page.handle_key ps key ~size
        in

        (* Key handler stack (pure) integration: thread alongside page state. *)
        (* Prepare key handler stack: push a frame for the page keymap once per page.
       We translate (key, state->state, desc) into a side-effect that records
       a pending state transformation applied after dispatch. *)
        let pending_update : (Page.pstate -> Page.pstate) option ref =
          ref None
        in
        let rec loop ps key_stack =
          (* Check if a signal (Ctrl+C) requested exit - if so, exit gracefully *)
          if Atomic.get signal_exit_flag then
            (* Don't call cleanup() here - let it run via at_exit for proper cleanup timing *)
            `SwitchTo "__EXIT__"
          else (
            (* Refresh refs for pager notifier to see current state/key_stack snapshots. *)
            current_state_ref := Some ps ;
            current_key_stack_ref := key_stack ;
            (* Rebuild page keymap frame each iteration so dynamic state (e.g. search mode) can adjust bindings. *)
            let key_stack =
              let merged = Page.keymap ps in
              match !page_frame_handle with
              | Some h ->
                  let ks = Khs.pop key_stack h in
                  let bindings =
                    List.map
                      (fun (kb : Page.key_binding) ->
                        let action =
                          if kb.display_only then None
                          else Some (fun () -> pending_update := Some kb.action)
                        in
                        ( kb.key,
                          Khs.
                            {
                              action;
                              help = kb.help;
                              display_only = kb.display_only;
                            } ))
                      merged
                  in
                  let ks', h' = Khs.push ks bindings in
                  page_frame_handle := Some h' ;
                  ks'
              | None -> key_stack
            in
            (* Update footer from top frame after rebuild. *)
            let () =
              let pairs = Khs.top_bindings key_stack in
              let pairs = if pairs = [] then [("q", "Quit")] else pairs in
              footer_ref :=
                Some (Miaou_widgets_display.Widgets.footer_hints pairs)
            in
            match read_key_blocking () with
            | `Quit -> `Quit
            | `Refresh -> (
                (* Periodic idle tick: let the page run its service cycle (for throttled refresh/background jobs). *)
                if Quit_flag.is_pending () then Quit_flag.clear_pending () ;
                let ps' =
                  Page.service_cycle (Page.refresh ps) 0
                  |> apply_pending_modal_nav
                in
                match Navigation.pending ps' with
                | Some page -> `SwitchTo page
                | None ->
                    clear_and_render ps' key_stack ;
                    loop ps' key_stack)
            | `Enter -> (
                if Quit_flag.is_pending () then Quit_flag.clear_pending () ;
                if Modal_manager.has_active () then
                  if
                    (* If the narrow modal is active, close it on any key as advertised. *)
                    is_narrow_modal_active ()
                  then (
                    Modal_manager.close_top `Cancel ;
                    clear_and_render ps key_stack ;
                    loop ps key_stack)
                  else (
                    (* Forward to modal; if it just closed and the page requested navigation, switch now. *)
                    Modal_manager.handle_key "Enter" ;
                    (* If the modal requested the key be consumed, stop here and do not
               propagate Enter to the underlying page. *)
                    if Modal_manager.take_consume_next_key () then
                      if not (Modal_manager.has_active ()) then (
                        let ps' =
                          Page.service_cycle (Page.refresh ps) 0
                          |> apply_pending_modal_nav
                        in
                        match Navigation.pending ps' with
                        | Some page -> `SwitchTo page
                        | None ->
                            clear_and_render ps' key_stack ;
                            loop ps' key_stack)
                      else (
                        clear_and_render ps key_stack ;
                        loop ps key_stack)
                    else if not (Modal_manager.has_active ()) then (
                      let ps' =
                        Page.service_cycle (Page.refresh ps) 0
                        |> apply_pending_modal_nav
                      in
                      match Navigation.pending ps' with
                      | Some page -> `SwitchTo page
                      | None ->
                          clear_and_render ps' key_stack ;
                          loop ps' key_stack)
                    else (
                      clear_and_render ps key_stack ;
                      loop ps key_stack))
                else if Page.has_modal ps then (
                  let size = detect_size () in
                  Modal_manager.set_current_size
                    size.LTerm_geom.rows
                    size.LTerm_geom.cols ;
                  let ps' = Page.handle_modal_key ps "Enter" ~size in
                  match Navigation.pending ps' with
                  | Some page -> `SwitchTo page
                  | None ->
                      clear_and_render ps' key_stack ;
                      loop ps' key_stack)
                else
                  (* Non-modal Enter: perform page.enter, then switch immediately if next_page set. *)
                  match Navigation.pending ps with
                  | Some page -> `SwitchTo page
                  | None -> (
                      let size = detect_size () in
                      let ps' = Page.handle_key ps "Enter" ~size in
                      match Navigation.pending ps' with
                      | Some page -> `SwitchTo page
                      | None ->
                          clear_and_render ps' key_stack ;
                          loop ps' key_stack))
            | (`Up | `Down | `Left | `Right | `NextPage | `PrevPage) as k -> (
                (* Drain consecutive identical navigation keys to prevent scroll lag.
             When arrow keys are held down and released, the terminal buffer may
             contain many identical events. Skip all but the last one. *)
                let drained_count = drain_consecutive_nav_keys k in
                (match Logger_capability.get () with
                | Some logger when Sys.getenv_opt "MIAOU_DEBUG" = Some "1" ->
                    if drained_count > 0 then
                      logger.logf
                        Debug
                        (Printf.sprintf
                           "NAV_KEY_DRAIN: drained %d consecutive events"
                           drained_count)
                | _ -> ()) ;
                let key =
                  match k with
                  | `Up -> "Up"
                  | `Down -> "Down"
                  | `Left -> "Left"
                  | `Right -> "Right"
                  | `NextPage -> "Tab"
                  | `PrevPage -> "Shift-Tab"
                  | _ -> ""
                in
                if key <> "" then (
                  if Quit_flag.is_pending () then Quit_flag.clear_pending () ;
                  let ps' = handle_key_like ps key key_stack in
                  match Navigation.pending ps' with
                  | Some page -> `SwitchTo page
                  | None ->
                      clear_and_render ps' key_stack ;
                      loop ps' key_stack)
                else if Modal_manager.has_active () then
                  if is_narrow_modal_active () then (
                    Modal_manager.close_top `Cancel ;
                    clear_and_render ps key_stack ;
                    loop ps key_stack)
                  else (
                    Modal_manager.handle_key key ;
                    clear_and_render ps key_stack ;
                    loop ps key_stack)
                else if Page.has_modal ps then (
                  let size = detect_size () in
                  Modal_manager.set_current_size
                    size.LTerm_geom.rows
                    size.LTerm_geom.cols ;
                  let ps' = Page.handle_modal_key ps key ~size in
                  clear_and_render ps' key_stack ;
                  loop ps' key_stack)
                else
                  (* First attempt stack-based dispatch. *)
                  let consumed, key_stack' = Khs.dispatch key_stack key in
                  if consumed then (
                    let ps' =
                      match !pending_update with
                      | Some f ->
                          let s' = f ps in
                          pending_update := None ;
                          s'
                      | None -> ps
                    in
                    match Navigation.pending ps' with
                    | Some page -> `SwitchTo page
                    | None ->
                        clear_and_render ps' key_stack ;
                        loop ps' key_stack')
                  else
                    let ps' = handle_key_like ps key key_stack in
                    match Navigation.pending ps' with
                    | Some page -> `SwitchTo page
                    | None ->
                        clear_and_render ps' key_stack ;
                        loop ps' key_stack')
            | `Other key ->
                if key = "?" then (
                  (* Build help text with optional contextual hint (markdown),
          shown above the key bindings. Title is "hints" with a subtitle
          for the bindings. *)
                  let size = detect_size () in
                  let cols = size.LTerm_geom.cols in
                  (* Use a conservative content width and align modal max width to it to
          prevent container re-wrapping (breaks words). *)
                  let content_width =
                    let cw = max 16 (cols - 20) in
                    min cw 72
                  in
                  let all = Khs.all_bindings key_stack in
                  let dedup = Hashtbl.create 97 in
                  List.iter
                    (fun (k, h) ->
                      if not (Hashtbl.mem dedup k) then Hashtbl.add dedup k h)
                    all ;
                  let entries =
                    Hashtbl.fold (fun k h acc -> (k, h) :: acc) dedup []
                  in
                  let entries =
                    List.sort (fun (a, _) (b, _) -> String.compare a b) entries
                  in
                  let key_lines =
                    List.map
                      (fun (k, h) -> Printf.sprintf "%-12s %s" k h)
                      entries
                  in
                  let contextual = Help_hint.get_active () in
                  let hint_block =
                    match contextual with
                    | None -> None
                    | Some {short; long} ->
                        let pick =
                          match (long, short) with
                          | Some l, Some s -> if cols >= 100 then l else s
                          | Some l, None -> l
                          | None, Some s -> s
                          | None, None -> ""
                        in
                        let pick = String.trim pick in
                        if pick = "" then None
                        else
                          let md =
                            Miaou_internals.Modal_utils.markdown_to_ansi pick
                          in
                          let wrapped =
                            Miaou_internals.Modal_utils
                            .wrap_content_to_width_words
                              md
                              content_width
                          in
                          Some wrapped
                  in
                  let body =
                    match hint_block with
                    | None ->
                        let header =
                          Miaou_widgets_display.Widgets.bold
                            (Miaou_widgets_display.Widgets.fg 81 "Key Bindings")
                        in
                        let keys = Helpers.concat_lines key_lines in
                        let buf =
                          Buffer.create
                            (String.length header + String.length keys + 2)
                        in
                        Buffer.add_string buf header ;
                        Buffer.add_char buf '\n' ;
                        Buffer.add_string buf keys ;
                        Buffer.contents buf
                    | Some hb ->
                        let sep =
                          Miaou_widgets_display.Widgets.fg
                            238
                            (Miaou_widgets_display.Widgets.hr
                               ~width:(min content_width (cols - 6))
                               ())
                        in
                        let header =
                          Miaou_widgets_display.Widgets.bold
                            (Miaou_widgets_display.Widgets.fg 81 "Key Bindings")
                        in
                        let keys = Helpers.concat_lines key_lines in
                        let buf =
                          Buffer.create
                            (String.length hb + String.length sep
                           + String.length header + String.length keys + 8)
                        in
                        Buffer.add_string buf hb ;
                        Buffer.add_char buf '\n' ;
                        Buffer.add_string buf sep ;
                        Buffer.add_char buf '\n' ;
                        Buffer.add_string buf header ;
                        Buffer.add_char buf '\n' ;
                        Buffer.add_string buf keys ;
                        Buffer.contents buf
                  in
                  let module Help_modal = struct
                    module Page : PAGE_SIG = struct
                      type state = unit

                      type key_binding = state key_binding_desc

                      type pstate = state Navigation.t

                      type msg = unit

                      let handle_modal_key ps _ ~size:_ = ps

                      let handle_key ps _ ~size:_ = ps

                      let update ps _ = ps

                      let move ps _ = ps

                      let refresh ps = ps

                      let service_select ps _ = ps

                      let service_cycle ps _ = ps

                      let back ps = ps

                      let has_modal _ = false

                      let init () = Navigation.make ()

                      let view ps ~focus:_ ~size:_ =
                        ignore ps ;
                        body

                      let keymap (_ : pstate) = []

                      let handled_keys () = []
                    end
                  end in
                  Modal_manager.push_default
                    (module Help_modal.Page)
                    ~init:(Help_modal.Page.init ())
                    ~ui:
                      {
                        title = "hints";
                        left = None;
                        max_width = Some (Fixed (content_width + 4));
                        dim_background = true;
                      }
                    ~on_close:(fun (_ : Help_modal.Page.pstate) _ -> ()) ;
                  clear_and_render ps key_stack ;
                  loop ps key_stack)
                else if key = "Esc" || key = "Escape" then
                  if Modal_manager.has_active () || Page.has_modal ps then (
                    (* Close modal if any; if page requested navigation, switch now. *)
                    Modal_manager.handle_key "Esc" ;
                    if Modal_manager.take_consume_next_key () then
                      if not (Modal_manager.has_active ()) then (
                        let ps' =
                          Page.service_cycle (Page.refresh ps) 0
                          |> apply_pending_modal_nav
                        in
                        match Navigation.pending ps' with
                        | Some page -> `SwitchTo page
                        | None ->
                            clear_and_render ps' key_stack ;
                            loop ps' key_stack)
                      else (
                        clear_and_render ps key_stack ;
                        loop ps key_stack)
                    else if not (Modal_manager.has_active ()) then (
                      let ps' =
                        Page.service_cycle (Page.refresh ps) 0
                        |> apply_pending_modal_nav
                      in
                      match Navigation.pending ps' with
                      | Some page -> `SwitchTo page
                      | None ->
                          clear_and_render ps' key_stack ;
                          loop ps' key_stack)
                    else (
                      clear_and_render ps key_stack ;
                      loop ps key_stack))
                  else
                    (* Let the current page override Esc/Escape. If it sets next_page,
                 navigate there; else fall back to default back behavior. *)
                    let size = detect_size () in
                    let ps' = Page.handle_key ps key ~size in
                    match Navigation.pending ps' with
                    | Some page -> `SwitchTo page
                    | None -> `SwitchTo "__BACK__"
                else if
                  (* If a modal is active, route all keys to the modal first and do not
               propagate them to the underlying page or key handler stack. This
               prevents page shortcuts (e.g. 'd' for delete) from triggering while
               typing in modal inputs. *)
                  Modal_manager.has_active ()
                then
                  if is_narrow_modal_active () then (
                    Modal_manager.close_top `Cancel ;
                    clear_and_render ps key_stack ;
                    loop ps key_stack)
                  else (
                    Modal_manager.handle_key key ;
                    clear_and_render ps key_stack ;
                    loop ps key_stack)
                else (
                  if Quit_flag.is_pending () then Quit_flag.clear_pending () ;
                  (* Stack dispatch first. *)
                  let consumed, key_stack' = Khs.dispatch key_stack key in
                  if consumed then (
                    let ps' =
                      match !pending_update with
                      | Some f ->
                          let s' = f ps in
                          pending_update := None ;
                          s'
                      | None -> ps
                    in
                    match Navigation.pending ps' with
                    | Some page -> `SwitchTo page
                    | None ->
                        clear_and_render ps' key_stack ;
                        loop ps' key_stack')
                  else
                    let ps' = handle_key_like ps key key_stack in
                    match Navigation.pending ps' with
                    | Some page -> `SwitchTo page
                    | None ->
                        clear_and_render ps' key_stack ;
                        loop ps' key_stack'))
        in

        enter_raw () ;
        (* Enable xterm mouse tracking: 1000 button events, 1006 SGR extended. *)
        (try
           print_string "\027[?1000h\027[?1006h" ;
           Stdlib.flush stdout
         with _ -> ()) ;
        (* Log initial terminal size on startup *)
        let initial_size = detect_size () in
        (match Logger_capability.get () with
        | Some logger when Sys.getenv_opt "MIAOU_DEBUG" = Some "1" ->
            logger.logf
              Info
              (Printf.sprintf
                 "STARTUP: terminal size %dx%d (cols=%d)"
                 initial_size.LTerm_geom.cols
                 initial_size.LTerm_geom.rows
                 initial_size.LTerm_geom.cols)
        | _ -> ()) ;
        last_size := initial_size ;
        let ps0 = Page.init () in
        (* Initialize refs for pager notifier and register hook. *)
        current_state_ref := Some ps0 ;
        (* Initialize stack after we have initial state. *)
        let init_stack =
          let bindings =
            List.map
              (fun (kb : Page.key_binding) ->
                let action =
                  if kb.display_only then None
                  else Some (fun () -> pending_update := Some kb.action)
                in
                ( kb.key,
                  Khs.{action; help = kb.help; display_only = kb.display_only}
                ))
              (Page.keymap ps0)
          in
          let key_stack, handle = Khs.push Khs.empty bindings in
          page_frame_handle := Some handle ;
          key_stack
        in
        current_key_stack_ref := init_stack ;
        (* Pager notification callback is now passed to Pager_widget.open_lines per-instance.
       Applications using pager widgets should pass notify_render_from_pager_flag when creating pagers. *)
        (* Footer cache updated each loop; initialize ref *)
        footer_ref := None ;
        clear_and_render ps0 init_stack ;
        let outcome =
          try loop ps0 init_stack
          with e ->
            (* Ensure cleanup runs even on exceptions *)
            cleanup () ;
            raise e
        in
        (* Pop page frame explicitly (semantic symmetry) *)
        (match !page_frame_handle with
        | Some h -> ignore (Khs.pop init_stack h)
        | None -> ()) ;
        (* Unified cleanup on exit. *)
        cleanup () ;
        outcome)
  in

  match initial_page with
  | (module Page) -> run_with_page (module Page : PAGE_SIG)
