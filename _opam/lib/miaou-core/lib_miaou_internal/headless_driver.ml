(* SPDX-License-Identifier: MIT *)
(* Minimal headless TUI driver used for testing Miaou pages without a terminal. *)

[@@@warning "-32-34-37-69"]

module Tui_page = Miaou_core.Tui_page
module Navigation = Miaou_core.Navigation
module Modal_manager = Miaou_core.Modal_manager
module Capture = Miaou_core.Tui_capture
module Fibers = Miaou_helpers.Fiber_runtime
open LTerm_geom

(* Helper: apply any pending navigation from modal callbacks to pstate *)
let apply_pending_modal_nav ps =
  match Modal_manager.take_pending_navigation () with
  | Some page -> Navigation.goto page ps
  | None -> ps

module Key_queue = struct
  let q : string Queue.t = Queue.create ()

  let feed_keys ks = List.iter (fun k -> Queue.push k q) ks

  let take () = if Queue.is_empty q then None else Some (Queue.pop q)

  let clear () = Queue.clear q
end

module Screen = struct
  let buf = Buffer.create 4096

  let clear () = Buffer.clear buf

  let append s = Buffer.add_string buf s

  let get () = Buffer.contents buf
end

let size_ref : LTerm_geom.size ref = ref {rows = 24; cols = 80}

let set_size rows cols = size_ref := {rows; cols}

let get_size () = !size_ref

let current_page : (module Tui_page.PAGE_SIG) option ref = ref None

let set_page p = current_page := Some p

let feed_keys = Key_queue.feed_keys

let get_screen_content () = Screen.get ()

let render_page_with (type s) (module P : Tui_page.PAGE_SIG with type state = s)
    (ps : s Navigation.t) =
  let size = get_size () in
  let base = P.view ps ~focus:true ~size in
  let base =
    match
      Miaou_internals.Modal_renderer.render_overlay
        ~cols:(Some size.cols)
        ~base
        ~rows:size.rows
        ()
    with
    | Some overlay -> overlay
    | None -> base
  in
  let content =
    let flashes = Flash_bus.snapshot () in
    match flashes with
    | [] -> base
    | lst ->
        let toast_block =
          Flash_toast_renderer.render_snapshot
            ~position:`Bottom_right
            ~cols:size.cols
            lst
        in
        base ^ "\n" ^ toast_block
  in
  Screen.clear () ;
  Screen.append content ;
  Capture.record_frame ~rows:size.rows ~cols:size.cols content

let render_only (type s) (module P : Tui_page.PAGE_SIG with type state = s) :
    unit =
  let ps = P.init () in
  render_page_with (module P) ps

let max_iterations_ref = ref 20_000

let max_seconds_ref = ref 10.0

let set_limits ?iterations ?seconds () =
  Option.iter (fun i -> max_iterations_ref := i) iterations ;
  Option.iter (fun s -> max_seconds_ref := s) seconds

let with_page_scope f = Fibers.with_page_scope f

let run (initial_page : (module Tui_page.PAGE_SIG)) :
    [`Quit | `SwitchTo of string] =
  with_page_scope (fun () ->
      let module P : Tui_page.PAGE_SIG = (val initial_page) in
      let start_time = Unix.gettimeofday () in
      let exceed_guard iteration =
        let elapsed = Unix.gettimeofday () -. start_time in
        if iteration >= !max_iterations_ref || elapsed >= !max_seconds_ref then
          failwith
            (Printf.sprintf
               "Headless_driver timeout: iteration=%d elapsed=%.3fs (limits: \
                %d iter / %.1fs)"
               iteration
               elapsed
               !max_iterations_ref
               !max_seconds_ref)
      in
      let rec loop iteration (ps : P.pstate) : [`Quit | `SwitchTo of string] =
        exceed_guard iteration ;
        render_page_with (module P) ps ;
        match Key_queue.take () with
        | None -> (
            (try
               Printf.eprintf
                 "[driver][debug] No key in queue, refreshing page\n%!"
             with _ -> ()) ;
            let ps' = P.refresh ps |> apply_pending_modal_nav in
            match Navigation.pending ps' with
            | Some "__QUIT__" -> `Quit
            | Some name -> `SwitchTo name
            | None -> loop (iteration + 1) ps')
        | Some k -> (
            (try
               Printf.eprintf
                 "[driver][debug] Key event: '%s' modal_active=%b\n%!"
                 k
                 (Miaou_core.Modal_manager.has_active ())
             with _ -> ()) ;
            let forced_switch =
              String.length k > 11
              && String.sub k 0 11 = "__SWITCH__:"
              && Sys.getenv_opt "MIAOU_TEST_ALLOW_FORCED_SWITCH" = Some "1"
            in
            if not forced_switch then Capture.record_keystroke k ;
            if forced_switch then
              `SwitchTo (String.sub k 11 (String.length k - 11))
            else
              let ps' =
                if Miaou_core.Modal_manager.has_active () then (
                  (try
                     Printf.eprintf
                       "[driver][debug] Modal manager handling key: '%s'\n%!"
                       k
                   with _ -> ()) ;
                  Miaou_core.Modal_manager.handle_key k ;
                  ps)
                else
                  match k with
                  | "Up" -> P.move ps (-1)
                  | "Down" -> P.move ps 1
                  | "q" | "Q" -> Navigation.quit ps
                  | _ -> P.handle_key ps k ~size:(get_size ())
              in
              let ps' = apply_pending_modal_nav ps' in
              render_page_with (module P) ps' ;
              match Navigation.pending ps' with
              | Some "__QUIT__" -> `Quit
              | Some name -> `SwitchTo name
              | None -> loop (iteration + 1) ps')
      in
      set_page initial_page ;
      loop 0 (P.init ()))

module Stateful = struct
  let initialized = ref false

  let send_key_impl : (string -> unit) ref = ref (fun _ -> ())

  let refresh_impl : (unit -> unit) ref = ref (fun () -> ())

  let next_page_impl : (unit -> string option) ref = ref (fun () -> None)

  let init (type s) (module P : Tui_page.PAGE_SIG with type state = s) : unit =
    with_page_scope (fun () ->
        let ps = ref (P.init ()) in
        let render () = render_page_with (module P) !ps in
        let handle_modal_key k =
          if Miaou_core.Modal_manager.has_active () then
            Miaou_core.Modal_manager.handle_key k
        in
        let handle_key (k : string) =
          if Miaou_core.Modal_manager.has_active () then (
            handle_modal_key k ;
            render ())
          else
            let new_ps =
              match k with
              | "Up" -> P.move !ps (-1)
              | "Down" -> P.move !ps 1
              | "q" | "Q" -> Navigation.quit !ps
              | _ -> P.handle_key !ps k ~size:(get_size ())
            in
            ps := new_ps ;
            render ()
        in
        send_key_impl := handle_key ;
        (refresh_impl :=
           fun () ->
             ps := P.refresh !ps ;
             render ()) ;
        (next_page_impl := fun () -> Navigation.pending !ps) ;
        initialized := true ;
        render ())

  let ensure () =
    if not !initialized then invalid_arg "Stateful driver not initialised"

  let classify_next () =
    match !next_page_impl () with
    | Some "__QUIT__" -> `Quit
    | Some name -> `SwitchTo name
    | None -> `Continue

  let send_key k =
    ensure () ;
    let forced_switch =
      String.length k > 11
      && String.sub k 0 11 = "__SWITCH__:"
      && Sys.getenv_opt "MIAOU_TEST_ALLOW_FORCED_SWITCH" = Some "1"
    in
    if forced_switch then
      let target = String.sub k 11 (String.length k - 11) in
      `SwitchTo target
    else (
      Capture.record_keystroke k ;
      !send_key_impl k ;
      classify_next ())

  let idle_wait ?(iterations = 1) ?(sleep = 0.0) () =
    ensure () ;
    let rec loop i =
      if i <= 0 then `Continue
      else
        match classify_next () with
        | (`Quit | `SwitchTo _) as r -> r
        | `Continue ->
            !refresh_impl () ;
            (if sleep > 0.0 then try Unix.sleepf sleep with _ -> ()) ;
            loop (pred i)
    in
    loop iterations
end
