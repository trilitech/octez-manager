(*****************************************************************************)
(*                                                                           *)
(* SPDX-License-Identifier: MIT                                              *)
(* Copyright (c) 2025 Nomadic Labs <contact@nomadic-labs.com>                *)
(*                                                                           *)
(*****************************************************************************)

[@@@warning "-32-34-37"]

open Miaou_core
module Narrow_modal = Miaou_core.Narrow_modal
module Logger_capability = Miaou_interfaces.Logger_capability

let available = true

(* One-time narrow terminal warning flag *)
let narrow_warned = ref false

(* Debug overlay - shows FPS/TPS when MIAOU_OVERLAY is set *)
let overlay_enabled =
  lazy
    (match Sys.getenv_opt "MIAOU_OVERLAY" with
    | Some ("1" | "true" | "TRUE" | "yes" | "YES") -> true
    | _ -> false)

type tps_tracker = {
  mutable tick_count : int;
  mutable last_time : float;
  mutable current_tps : float;
}

let create_tps_tracker () =
  {tick_count = 0; last_time = Unix.gettimeofday (); current_tps = 0.0}

let update_tps tracker =
  tracker.tick_count <- tracker.tick_count + 1 ;
  let now = Unix.gettimeofday () in
  let elapsed = now -. tracker.last_time in
  if elapsed >= 1.0 then begin
    tracker.current_tps <- float_of_int tracker.tick_count /. elapsed ;
    tracker.tick_count <- 0 ;
    tracker.last_time <- now
  end

let render_overlay_text ~loop_fps ~render_fps ~tps ~cols
    (ops : Matrix_buffer.batch_ops) =
  (* Render "matrix L:XX R:XX T:XX" in top-right corner with dim style
     L = Loop FPS (cap), R = Render FPS (actual), T = TPS *)
  let text =
    Printf.sprintf "matrix L:%.0f R:%.0f T:%.0f" loop_fps render_fps tps
  in
  let len = String.length text in
  let start_col = cols - len - 1 in
  if start_col > 0 then begin
    let style = {Matrix_cell.default_style with dim = true; fg = 245} in
    String.iteri
      (fun i c ->
        ops.set_char ~row:0 ~col:(start_col + i) ~char:(String.make 1 c) ~style)
      text
  end

(* Pack page and state together to avoid GADT escaping issues *)
type packed_state =
  | Packed :
      (module Tui_page.PAGE_SIG with type state = 's) * 's Navigation.t
      -> packed_state

module Fibers = Miaou_helpers.Fiber_runtime

let eio_sleep env seconds =
  if seconds > 0.001 then Eio.Time.sleep env#clock seconds

let run (initial_page : (module Tui_page.PAGE_SIG)) :
    [`Quit | `SwitchTo of string] =
  Fibers.with_page_switch (fun env _page_sw ->
      (* Load configuration *)
      let config = Matrix_config.load () in
      let tick_time_s = config.tick_time_ms /. 1000.0 in

      (* Setup terminal *)
      let terminal = Matrix_terminal.setup () in
      at_exit (fun () -> Matrix_terminal.cleanup terminal) ;

      (* Get terminal size *)
      let rows, cols = Matrix_terminal.size terminal in

      (* Create buffer *)
      let buffer = Matrix_buffer.create ~rows ~cols in

      (* Create parser and writer *)
      let parser = Matrix_ansi_parser.create () in
      let writer = Matrix_ansi_writer.create () in

      (* Create input handler *)
      let input = Matrix_input.create terminal in

      (* Create render loop *)
      let render_loop =
        Matrix_render_loop.create ~config ~buffer ~writer ~terminal
      in

      (* Enter raw mode and enable mouse *)
      Matrix_terminal.enter_raw terminal ;
      Matrix_terminal.enable_mouse terminal ;

      (* Hide cursor *)
      Matrix_terminal.write terminal Matrix_ansi_writer.cursor_hide ;

      (* Clear screen initially *)
      Matrix_terminal.write terminal "\027[2J\027[H" ;

      (* Start render domain - runs at 60 FPS in parallel *)
      Matrix_render_loop.start render_loop ;

      (* TPS tracker for debug overlay *)
      let tps_tracker = create_tps_tracker () in

      (* Track modal state to trigger full redraw on modal open/close *)
      let last_modal_active = ref false in

      (* Frame counter for periodic partial refresh (doesn't reset like tick_count) *)
      let frame_counter = ref 0 in

      (* Track last size for narrow warning detection *)
      let last_size = ref {LTerm_geom.rows = 24; cols = 80} in

      (* Main loop - runs in main domain, handles input and effects *)
      let rec loop packed =
        let tick_start = Unix.gettimeofday () in
        let (Packed ((module Page), ps)) = packed in

        (* Get current terminal size *)
        let rows, cols = Matrix_terminal.size terminal in
        let size = {LTerm_geom.rows; cols} in

        (* Check if we need to resize buffer *)
        let buf_rows, buf_cols = Matrix_buffer.size buffer in
        if rows <> buf_rows || cols <> buf_cols then begin
          Matrix_buffer.resize buffer ~rows ~cols ;
          Matrix_buffer.mark_all_dirty buffer
        end ;

        (* One-time narrow terminal warning (only once per session) *)
        let prev_cols = !last_size.LTerm_geom.cols in
        if
          (cols < 80 && not !narrow_warned)
          || (cols < 80 && prev_cols >= 80 && not !narrow_warned)
        then (
          (match Logger_capability.get () with
          | Some logger ->
              logger.logf
                Warning
                (Printf.sprintf
                   "WIDTH_CROSSING: prev=%d new=%d (showing narrow modal)"
                   prev_cols
                   cols)
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
            ~on_close:(fun (_ : Narrow_modal.Page.pstate) _ -> ()) ;
          Modal_manager.set_consume_next_key () ;
          let my_title = "Narrow terminal" in
          Fibers.spawn (fun env ->
              Eio.Time.sleep env#clock 5.0 ;
              match Modal_manager.top_title_opt () with
              | Some t when t = my_title -> Modal_manager.close_top `Cancel
              | _ -> ())) ;
        last_size := size ;

        (* Build header lines for narrow terminal warning banner *)
        let header_lines =
          if cols < 80 then
            [
              Miaou_widgets_display.Widgets.warning_banner
                ~cols
                (Printf.sprintf
                   "Narrow terminal: %d cols (< 80). Some UI may be truncated."
                   cols);
            ]
          else []
        in

        (* Render page view to ANSI string *)
        let view_output = Page.view ps ~focus:true ~size in

        (* Prepend header lines if any *)
        let view_output =
          match header_lines with
          | [] -> view_output
          | lines -> String.concat "\n" lines ^ "\n" ^ view_output
        in

        (* Check for modal state change *)
        let modal_active = Modal_manager.has_active () in
        let modal_just_changed = modal_active <> !last_modal_active in
        if modal_just_changed then last_modal_active := modal_active ;

        (* Render modal overlay if active *)
        let view_output =
          if modal_active then
            match
              Miaou_internals.Modal_renderer.render_overlay
                ~cols:(Some cols)
                ~base:view_output
                ~rows
                ()
            with
            | Some v -> v
            | None -> view_output
          else view_output
        in

        (* Update TPS tracker *)
        update_tps tps_tracker ;

        (* Update back buffer with new view - thread-safe batch operation *)
        Matrix_buffer.with_back_buffer buffer (fun ops ->
            (* Clear back buffer *)
            ops.clear () ;

            (* Parse ANSI output into buffer using batch set_char *)
            Matrix_ansi_parser.reset parser ;
            let _ =
              Matrix_ansi_parser.parse_into_batch
                parser
                ops
                ~row:0
                ~col:0
                view_output
            in

            (* Render debug overlay if enabled *)
            if Lazy.force overlay_enabled then
              render_overlay_text
                ~loop_fps:(Matrix_render_loop.loop_fps render_loop)
                ~render_fps:(Matrix_render_loop.current_fps render_loop)
                ~tps:tps_tracker.current_tps
                ~cols
                ops) ;

        (* On modal OPEN, do synchronous clear+render to avoid overlay artifacts.
           On modal CLOSE, just let diff handle it - no clear needed. *)
        if modal_just_changed && modal_active then begin
          Matrix_buffer.mark_all_dirty buffer ;
          Matrix_terminal.write terminal "\027[2J\027[H" ;
          Matrix_render_loop.force_render render_loop
        end ;

        (* Periodic full refresh every 120 frames (~2s at 60 TPS) to catch any artifacts *)
        incr frame_counter ;
        if !frame_counter mod 120 = 0 then Matrix_buffer.mark_all_dirty buffer ;

        (* Poll for input with short timeout to maintain TPS *)
        let timeout_ms = int_of_float (config.tick_time_ms *. 0.8) in
        match Matrix_input.poll input ~timeout_ms with
        | Matrix_input.Quit ->
            Matrix_render_loop.shutdown render_loop ;
            `Quit
        | Matrix_input.Resize ->
            Matrix_terminal.invalidate_size_cache terminal ;
            (* Clear terminal on resize to avoid artifacts from old layout *)
            Matrix_terminal.write terminal "\027[2J\027[H" ;
            Matrix_buffer.mark_all_dirty buffer ;
            loop packed
        | Matrix_input.Refresh ->
            let ps' = Page.service_cycle (Page.refresh ps) 0 in
            check_navigation (Packed ((module Page), ps')) tick_start
        | Matrix_input.Idle ->
            (* No input and not time for refresh - maintain TPS and continue *)
            let elapsed = Unix.gettimeofday () -. tick_start in
            let sleep_time = tick_time_s -. elapsed in
            eio_sleep env sleep_time ;
            loop packed
        | Matrix_input.Key key ->
            (* Debug: log received key if MIAOU_DEBUG is set *)
            if Sys.getenv_opt "MIAOU_DEBUG" = Some "1" then (
              let oc =
                open_out_gen
                  [Open_append; Open_creat]
                  0o644
                  "/tmp/miaou-keys.log"
              in
              Printf.fprintf
                oc
                "Key received: %S, modal_active=%b, has_modal=%b\n%!"
                key
                (Modal_manager.has_active ())
                (Page.has_modal ps) ;
              close_out oc) ;
            let _ = Matrix_input.drain_nav_keys input (Matrix_input.Key key) in
            (* Set modal size before handling keys *)
            Modal_manager.set_current_size rows cols ;
            (* Helper: detect whether the transient narrow modal is currently active *)
            let is_narrow_modal_active () =
              match Modal_manager.top_title_opt () with
              | Some t when t = "Narrow terminal" -> true
              | _ -> false
            in
            (* Check if modal is active - if so, send keys to modal instead of page *)
            if Modal_manager.has_active () then begin
              (* If the narrow modal is active, close it on any key as advertised *)
              if is_narrow_modal_active () then Modal_manager.close_top `Cancel
              else Modal_manager.handle_key key ;
              (* If modal was closed by Esc, drain any pending Esc keys to prevent
             double-navigation (modal close + page back) *)
              if
                (key = "Esc" || key = "Escape")
                && not (Modal_manager.has_active ())
              then ignore (Matrix_input.drain_esc_keys input) ;
              (* After modal handles key, check if navigation requested *)
              let ps' = Page.service_cycle (Page.refresh ps) 0 in
              check_navigation (Packed ((module Page), ps')) tick_start
            end
            else if Page.has_modal ps then begin
              (* Page has its own modal - use page's modal key handler *)
              let ps' = Page.handle_modal_key ps key ~size in
              (* If modal was closed by Esc, drain any pending Esc keys *)
              if (key = "Esc" || key = "Escape") && not (Page.has_modal ps')
              then ignore (Matrix_input.drain_esc_keys input) ;
              check_navigation (Packed ((module Page), ps')) tick_start
            end
            else
              (* All keys go through handle_key - Enter, Esc, navigation, etc.
                 Pages use Navigation.goto/back/quit for navigation. *)
              let ps' =
                let keymap = Page.keymap ps in
                let keymap_match =
                  List.find_opt
                    (fun (kb : Page.key_binding) -> kb.key = key)
                    keymap
                in
                match keymap_match with
                | Some kb when not kb.display_only -> kb.action ps
                | _ -> Page.handle_key ps key ~size
              in
              check_navigation (Packed ((module Page), ps')) tick_start
        | Matrix_input.Mouse (row, col) ->
            let mouse_key = Printf.sprintf "Mouse:%d:%d" row col in
            (* Set modal size before handling keys *)
            Modal_manager.set_current_size rows cols ;
            (* Check if modal is active - if so, send keys to modal instead of page *)
            if Modal_manager.has_active () then begin
              Modal_manager.handle_key mouse_key ;
              let ps' = Page.service_cycle (Page.refresh ps) 0 in
              check_navigation (Packed ((module Page), ps')) tick_start
            end
            else if Page.has_modal ps then
              (* Page has its own modal - use page's modal key handler *)
              let ps' = Page.handle_modal_key ps mouse_key ~size in
              check_navigation (Packed ((module Page), ps')) tick_start
            else
              let ps' = Page.handle_key ps mouse_key ~size in
              check_navigation (Packed ((module Page), ps')) tick_start
      and check_navigation packed tick_start =
        let (Packed ((module Page), ps)) = packed in
        (* Check for pending navigation from modal callbacks *)
        let ps =
          match Modal_manager.take_pending_navigation () with
          | Some page -> Navigation.goto page ps
          | None -> ps
        in
        let next = Navigation.pending ps in
        (* Debug: log navigation if MIAOU_DEBUG is set *)
        (if Sys.getenv_opt "MIAOU_DEBUG" = Some "1" then
           match next with
           | Some name ->
               let oc =
                 open_out_gen
                   [Open_append; Open_creat]
                   0o644
                   "/tmp/miaou-keys.log"
               in
               Printf.fprintf oc "Navigation requested: %S\n%!" name ;
               close_out oc
           | None -> ()) ;
        match next with
        | Some "__QUIT__" ->
            Matrix_render_loop.shutdown render_loop ;
            `Quit
        | Some name -> `SwitchTo name
        | None ->
            (* Maintain TPS by sleeping if we have time left *)
            (* Use Eio.Time.sleep to allow other fibers to run *)
            let elapsed = Unix.gettimeofday () -. tick_start in
            let sleep_time = tick_time_s -. elapsed in
            eio_sleep env sleep_time ;
            loop (Packed ((module Page), ps))
      in

      (* Start with initial page *)
      let (module P) = initial_page in
      let result = loop (Packed ((module P), P.init ())) in

      (* Cleanup *)
      Matrix_render_loop.shutdown render_loop ;
      Matrix_terminal.write terminal Matrix_ansi_writer.cursor_show ;
      Matrix_terminal.write terminal "\027[0m" ;
      (* Save screen content for debugging - will be printed after exit *)
      let screen_dump = Matrix_buffer.dump_to_string buffer in
      Matrix_terminal.set_exit_screen_dump terminal screen_dump ;
      Matrix_terminal.cleanup terminal ;

      result)
(* Close with_page_switch *)
