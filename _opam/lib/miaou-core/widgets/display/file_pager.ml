(*****************************************************************************)
(*                                                                           *)
(* SPDX-License-Identifier: MIT                                              *)
(* Copyright (c) 2025 Nomadic Labs <contact@nomadic-labs.com>                *)
(*                                                                           *)
(*****************************************************************************)

module Pager = Pager_widget
module Fibers = Miaou_helpers.Fiber_runtime

type tail_state = {
  path : string;
  mutable pos : int;
  mutable last_check : float;
  poll_interval_s : float;
  mutable closed : bool;
}

type t = {
  pager : Pager.t;
  mutable tail : tail_state option;
  mutable closed : bool;
  mutable cancel : (unit -> unit) option;
  notify_render : unit -> unit;
}

let runtime_error =
  Error
    "Eio runtime not initialized; call Miaou_helpers.Fiber_runtime.init inside \
     Eio_main.run"

let read_all_lines env path =
  let p = Eio.Path.(env#fs / path) in
  Eio.Path.with_open_in p @@ fun flow ->
  let buf = Eio.Buf_read.of_flow ~max_size:(16 * 1024 * 1024) flow in
  let rec loop acc =
    match Eio.Buf_read.line buf with
    | line -> loop (line :: acc)
    | exception End_of_file -> List.rev acc
  in
  loop []

let make_tail env path poll_interval_s =
  try
    let st = Unix.stat path in
    Some
      {
        path;
        pos = st.Unix.st_size;
        last_check = Eio.Time.now env#clock;
        poll_interval_s;
        closed = false;
      }
  with _ -> None

let close_tail (ts : tail_state) = ts.closed <- true

let read_new_lines (ts : tail_state) pager =
  match try Some (Unix.stat ts.path) with _ -> None with
  | None -> false
  | Some st -> (
      if st.Unix.st_size < ts.pos then ts.pos <- st.Unix.st_size ;
      if st.Unix.st_size <= ts.pos then false
      else
        try
          let ic = open_in_bin ts.path in
          seek_in ic ts.pos ;
          let rec loop acc =
            match input_line ic with
            | line -> loop (line :: acc)
            | exception End_of_file -> (List.rev acc, pos_in ic)
          in
          let lines, new_pos = loop [] in
          close_in_noerr ic ;
          ts.pos <- new_pos ;
          if lines <> [] then (
            Pager.append_lines_batched pager lines ;
            true)
          else false
        with _ -> false)

let rec tail_loop env (fp : t) (ts : tail_state) =
  if fp.closed || ts.closed || Fibers.is_shutdown () then close_tail ts
  else
    let now = Eio.Time.now env#clock in
    if now -. ts.last_check >= ts.poll_interval_s then (
      ts.last_check <- now ;
      let read_any = read_new_lines ts fp.pager in
      Pager.flush_pending_if_needed ~force:true fp.pager ;
      if read_any then fp.notify_render ()) ;
    if (not fp.closed) && not (Fibers.is_shutdown ()) then (
      Eio.Time.sleep env#clock ts.poll_interval_s ;
      tail_loop env fp ts)

let close (fp : t) =
  fp.closed <- true ;
  Option.iter (fun cancel -> cancel ()) fp.cancel ;
  fp.cancel <- None ;
  Option.iter
    (fun (ts : tail_state) ->
      close_tail ts ;
      fp.tail <- None ;
      Pager.stop_streaming fp.pager)
    fp.tail

let start_tail_watcher (fp : t) (ts : tail_state) =
  let env, sw = Fibers.require_env_and_switch () in
  let cancel_promise, cancel_resolver = Eio.Promise.create () in
  let resolved = ref false in
  fp.cancel <-
    Some
      (fun () ->
        if not !resolved then (
          resolved := true ;
          Eio.Promise.resolve cancel_resolver ())) ;
  Eio.Switch.on_release sw (fun () -> close fp) ;
  let tail_worker () =
    Fun.protect
      ~finally:(fun () ->
        fp.closed <- true ;
        close_tail ts ;
        fp.tail <- None ;
        Pager.stop_streaming fp.pager)
      (fun () ->
        Eio.Fiber.first
          (fun () -> tail_loop env fp ts)
          (fun () -> Eio.Promise.await cancel_promise))
  in
  Eio.Fiber.fork ~sw tail_worker

let pager (fp : t) = fp.pager

let open_file ?(follow = false) ?notify_render ?(poll_interval = 0.25) ?title
    path =
  match Fibers.env_opt () with
  | None -> runtime_error
  | Some env -> (
      try
        let lines = read_all_lines env path in
        let display_title = Option.value title ~default:path in
        let pager =
          Pager.open_lines ~title:display_title ?notify_render lines
        in
        let notify_cb =
          match notify_render with
          | Some f -> f
          | None -> (
              match pager.Pager.notify_render with
              | Some f -> f
              | None -> fun () -> ())
        in
        let fp =
          {
            pager;
            tail = None;
            closed = false;
            cancel = None;
            notify_render = notify_cb;
          }
        in
        if follow then (
          Pager.start_streaming fp.pager ;
          fp.pager.Pager.follow <- true ;
          match make_tail env path poll_interval with
          | Some ts ->
              fp.tail <- Some ts ;
              start_tail_watcher fp ts
          | None -> ()) ;
        Ok fp
      with exn -> Error (Printexc.to_string exn))
