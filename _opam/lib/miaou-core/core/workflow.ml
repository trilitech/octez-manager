(*****************************************************************************)
(*                                                                           *)
(* SPDX-License-Identifier: MIT                                              *)
(* Copyright (c) 2025 Nomadic Labs <contact@nomadic-labs.com>                *)
(*                                                                           *)
(*****************************************************************************)
(* Forward declare minimal PAGE signature (init + state) to decouple from
   full Tui_page dependency ordering for now. *)
module type PAGE = sig
  type state

  val init : unit -> state
end

module Ctx = struct
  type page

  type modal
end

open Ctx

(* Re‑expose type for users *)
type (_, _) t =
  | Return : 'a -> ('a, 'ctx) t
  | Bind : ('a, 'c) t * ('a -> ('b, 'c) t) -> ('b, 'c) t
  | Page : (module PAGE with type state = 's) * 's -> ('s, page) t
  | Feed_keys : string list * (unit, 'ctx) t -> (unit, 'ctx) t
  | Await_modal : {
      max_iters : int;
      sleep : float;
      screen_pred : (string -> bool) option;
      k : (unit, page) t;
    }
      -> (unit, modal) t
  | Await_no_modal : {
      max_iters : int;
      sleep : float;
      k : (unit, modal) t;
    }
      -> (unit, page) t
  | Seq_modal_to_page : (unit, modal) t * (unit, page) t -> (unit, page) t
  | Expect : (string -> bool) * (unit, 'ctx) t -> (unit, 'ctx) t
  | Capture_screen : (string -> 'a) * ('b, 'ctx) t -> ('a * 'b, 'ctx) t
  | Map : ('a -> 'b) * ('a, 'ctx) t -> ('b, 'ctx) t
  | When : (string -> bool) * (unit, 'ctx) t * (unit, 'ctx) t -> (unit, 'ctx) t
  | Loop_until : {
      max_iters : int;
      sleep : float;
      pred : string -> bool;
      k : (unit, 'ctx) t;
    }
      -> (unit, 'ctx) t

let return x = Return x

let ( let* ) m f = Bind (m, f)

let ( let+ ) m f = Map (f, m) |> fun x -> x

let start_page (type s) (module P : PAGE with type state = s) : (s, page) t =
  Page ((module P), P.init ())

let feed keys = Feed_keys (keys, Return ())

let await_modal ?(max_iters = 50) ?(sleep = 0.01) ?screen_pred () cont =
  Await_modal {max_iters; sleep; screen_pred; k = cont}

let await_no_modal ?(max_iters = 50) ?(sleep = 0.01) () cont =
  Await_no_modal {max_iters; sleep; k = cont}

let seq_modal_to_page m_modal m_page = Seq_modal_to_page (m_modal, m_page)

let expect p = Expect (p, Return ())

let when_ pred th el = When (pred, th, el)

let loop_until ?(max_iters = 100) ?(sleep = 0.01) pred =
  Loop_until {max_iters; sleep; pred; k = Return ()}

(* Driver abstraction *)
type driver = {
  feed_key : string -> unit;
  feed_keys : string list -> unit;
  screen : unit -> string;
  has_modal : unit -> bool;
  sleep : float -> unit;
  log : string -> unit;
}

let current_driver_ref : driver option ref = ref None

let register_driver d = current_driver_ref := Some d

let current_driver () =
  match !current_driver_ref with
  | Some d -> d
  | None -> failwith "Workflow: no driver registered"

let with_driver d f =
  let prev = !current_driver_ref in
  current_driver_ref := Some d ;
  Fun.protect ~finally:(fun () -> current_driver_ref := prev) f

type error = {
  step : string;
  message : string;
  attempt : int option;
  screen : string option;
}

exception Workflow_error of error

let pp_error e =
  Printf.sprintf
    "[%s]%s %s"
    e.step
    (match e.attempt with
    | None -> ""
    | Some i -> Printf.sprintf "(attempt=%d)" i)
    e.message

(* NOTE: For now we do not thread the phantom context dynamically; the interpreter
   treats all continuations as valid in the current runtime context. The type
   system already restricts illegal compositions statically. *)
let rec interpret : type a c. driver -> (a, c) t -> a =
 fun drv w ->
  match w with
  | Return x -> x
  | Bind (m, f) ->
      let v = interpret drv m in
      interpret drv (f v)
  | Map (f, m) -> f (interpret drv m)
  | Page (_p, st) -> st
  | Feed_keys (keys, k) ->
      (try drv.feed_keys keys with _ -> List.iter drv.feed_key keys) ;
      interpret drv k
  | Await_modal {max_iters; sleep; screen_pred; k} ->
      let rec loop i =
        let modal_active = drv.has_modal () in
        let screen_ok =
          match screen_pred with
          | None -> true
          | Some p -> ( try p (drv.screen ()) with _ -> false)
        in
        if modal_active && screen_ok then interpret drv k
        else if i >= max_iters then (
          let scr = try Some (drv.screen ()) with _ -> None in
          let err =
            {
              step = "await_modal";
              message = "timeout";
              attempt = Some i;
              screen = scr;
            }
          in
          drv.log (pp_error err) ;
          raise (Workflow_error err))
        else (
          drv.sleep sleep ;
          loop (i + 1))
      in
      loop 0
  | Await_no_modal {max_iters; sleep; k} ->
      let rec loop i =
        if not (drv.has_modal ()) then interpret drv k
        else if i >= max_iters then (
          let scr = try Some (drv.screen ()) with _ -> None in
          let err =
            {
              step = "await_no_modal";
              message = "timeout";
              attempt = Some i;
              screen = scr;
            }
          in
          drv.log (pp_error err) ;
          raise (Workflow_error err))
        else (
          drv.sleep sleep ;
          loop (i + 1))
      in
      loop 0
  | Seq_modal_to_page (m_modal, m_page) ->
      let _ = interpret drv m_modal in
      interpret drv m_page
  | Expect (pred, k) ->
      let s = drv.screen () in
      if pred s then interpret drv k
      else
        let err =
          {
            step = "expect";
            message = "predicate failed";
            attempt = None;
            screen = Some s;
          }
        in
        drv.log (pp_error err) ;
        raise (Workflow_error err)
  | Capture_screen (f, k) ->
      let s = drv.screen () in
      let captured = f s in
      let result = interpret drv k in
      (captured, result)
  | When (pred, th, el) ->
      let s = drv.screen () in
      interpret drv (if pred s then th else el)
  | Loop_until {max_iters; sleep; pred; k} ->
      let rec loop i =
        let s = drv.screen () in
        if pred s then interpret drv k
        else if i >= max_iters then (
          let err =
            {
              step = "loop_until";
              message = "timeout";
              attempt = Some i;
              screen = Some s;
            }
          in
          drv.log (pp_error err) ;
          raise (Workflow_error err))
        else (
          drv.sleep sleep ;
          loop (i + 1))
      in
      loop 0

let run_with drv w = interpret drv w

let run_modal_with drv w = interpret drv w

let run w = run_with (current_driver ()) w

let run_modal w = run_modal_with (current_driver ()) w

let run_result w = try Ok (run w) with Workflow_error e -> Error e

let run_modal_result w = try Ok (run_modal w) with Workflow_error e -> Error e

let simple_modal_flow ~open_keys ~confirm_keys : (unit, page) t =
  Bind
    ( feed open_keys,
      fun () ->
        Await_modal
          {max_iters = 200; sleep = 0.01; screen_pred = None; k = Return ()}
        |> fun _ ->
        Bind
          ( feed confirm_keys,
            fun () ->
              Await_no_modal {max_iters = 200; sleep = 0.01; k = Return ()}
              |> fun _ -> Return () ) )

(* Helper builders *)
let navigate_menu_item ~label:_ ~downs =
  let open_keys = downs @ ["Enter"] in
  feed open_keys

(* Helper: modal title predicate. We just search for the substring anywhere on the
   screen buffer to keep implementation decoupled from specific rendering. *)
let modal_title_pred ~substring screen =
  try
    let re = Str.regexp_string substring in
    Str.search_forward re screen 0 |> ignore ;
    true
  with _ -> false

(* Await that the screen (assumed to be an instances or services listing) shows
   the delegate alias param in some textual form. We rely on caller providing a
   predicate that indicates we are on the relevant page. We compose [loop_until]
   with a predicate searching for the alias token. *)
let await_delegate_alias_param ?(max_iters = 200) ?(sleep = 0.05) ~alias
    page_ready_pred =
  let pred screen =
    if not (page_ready_pred screen) then false
    else
      (* look for delegate_key_alias JSON or a simple alias token preceded by ':' or space *)
      let patterns =
        [
          "\"delegate_key_alias\"";
          (* JSON key *)
          alias;
          (* raw alias somewhere *)
        ]
      in
      List.exists
        (fun p ->
          try Str.search_forward (Str.regexp_string p) screen 0 >= 0
          with _ -> false)
        patterns
  in
  loop_until ~max_iters ~sleep pred

let await_env_args_fragment ?(max_iters = 200) ?(sleep = 0.05) ~fragment () =
  let pred screen =
    try
      Str.search_forward (Str.regexp_string fragment) screen 0 |> ignore ;
      true
    with _ -> false
  in
  loop_until ~max_iters ~sleep pred
