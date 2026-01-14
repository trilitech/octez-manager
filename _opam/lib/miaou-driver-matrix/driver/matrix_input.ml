(******************************************************************************)
(*                                                                            *)
(* SPDX-License-Identifier: MIT                                               *)
(* Copyright (c) 2025 Nomadic Labs <contact@nomadic-labs.com>                 *)
(*                                                                            *)
(******************************************************************************)

(** Matrix driver input handling - uses shared Input_parser. *)

module Parser = Miaou_driver_common.Input_parser

type event =
  | Key of string
  | Mouse of int * int
  | Resize
  | Refresh  (** Time for service_cycle - rate limited *)
  | Idle  (** No input, not time for refresh - just keep rendering *)
  | Quit

type t = {
  terminal : Matrix_terminal.t;
  parser : Parser.t;
  exit_flag : bool Atomic.t;
  mutable last_refresh_time : float;
}

(* Refresh interval in seconds - controls how often service_cycle is called *)
let refresh_interval = 1.0

let create terminal =
  let fd = Matrix_terminal.fd terminal in
  let exit_flag =
    Matrix_terminal.install_signals terminal (fun () ->
        Matrix_terminal.cleanup terminal)
  in
  {terminal; parser = Parser.create fd; exit_flag; last_refresh_time = 0.0}

(** Convert Parser.key to matrix event *)
let key_to_event = function
  | Parser.Mouse {row; col; release} ->
      if release then Mouse (row, col) else Refresh
  | Parser.Refresh -> Refresh
  | key -> Key (Parser.key_to_string key)

let poll t ~timeout_ms:_ =
  (* Check exit flag *)
  if Atomic.get t.exit_flag then Quit
  else if Matrix_terminal.resize_pending t.terminal then begin
    Matrix_terminal.clear_resize_pending t.terminal ;
    Resize
  end
  else if Miaou_helpers.Render_notify.should_render () then Refresh
  else begin
    (* Try to read input with minimal timeout - let Eio handle actual timing
       to allow other fibers to run *)
    if Parser.pending_length t.parser = 0 then
      ignore (Parser.refill t.parser ~timeout_s:0.001) ;
    (* Rate-limit refresh events *)
    if Parser.pending_length t.parser = 0 then begin
      let now = Unix.gettimeofday () in
      if now -. t.last_refresh_time >= refresh_interval then begin
        t.last_refresh_time <- now ;
        Refresh
      end
      else Idle
    end
    else
      match Parser.parse_key t.parser with
      | Some key -> key_to_event key
      | None -> Idle
  end

(** Convert event to Parser.key for draining *)
let event_to_parser_key = function
  | Key "Up" -> Some Parser.Up
  | Key "Down" -> Some Parser.Down
  | Key "Left" -> Some Parser.Left
  | Key "Right" -> Some Parser.Right
  | Key "Tab" -> Some Parser.Tab
  | Key "Delete" -> Some Parser.Delete
  | _ -> None

(* Drain consecutive navigation keys to prevent scroll lag *)
let drain_nav_keys t event =
  match event_to_parser_key event with
  | Some key -> Parser.drain_matching t.parser key
  | None -> 0

(* Drain any pending Esc keys from buffer.
   Call after modal close to prevent double-Esc navigation. *)
let drain_esc_keys t = Parser.drain_esc t.parser
