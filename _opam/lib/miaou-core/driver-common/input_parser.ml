(******************************************************************************)
(*                                                                            *)
(* SPDX-License-Identifier: MIT                                               *)
(* Copyright (c) 2025 Nomadic Labs <contact@nomadic-labs.com>                 *)
(*                                                                            *)
(******************************************************************************)

(** Terminal input parser shared between drivers.
    Based on lambda_term_driver.ml escape sequence parsing. *)

(** Parsed key event *)
type key =
  | Char of string  (** Regular character or UTF-8 grapheme *)
  | Enter
  | Tab
  | Backspace
  | Escape
  | Up
  | Down
  | Left
  | Right
  | Delete
  | Ctrl of char  (** C-a, C-b, etc. *)
  | Mouse of {row : int; col : int; release : bool}
  | Refresh  (** Synthetic refresh marker *)
  | Unknown of string  (** Unrecognized escape sequence *)

type t = {fd : Unix.file_descr; mutable pending : string}

let create fd = {fd; pending = ""}

(* ASCII keycodes *)
let esc_code = 27

let tab_code = 9

let backspace_code = 127

(** Read bytes into buffer with timeout. Returns bytes read. *)
let refill t ~timeout_s =
  try
    let r, _, _ = Unix.select [t.fd] [] [] timeout_s in
    if r = [] then 0
    else
      let buf = Bytes.create 256 in
      try
        let n = Unix.read t.fd buf 0 256 in
        if n <= 0 then 0
        else begin
          t.pending <- t.pending ^ Bytes.sub_string buf 0 n ;
          n
        end
      with Unix.Unix_error (Unix.EINTR, _, _) -> 0
  with Unix.Unix_error (Unix.EINTR, _, _) -> 0

(** Consume n bytes from pending buffer *)
let consume t n =
  let len = String.length t.pending in
  if n >= len then t.pending <- ""
  else t.pending <- String.sub t.pending n (len - n)

(** Parse a key from buffer WITHOUT consuming it.
    Returns None if buffer is empty or contains incomplete sequence.
    This is the peek-then-consume pattern from lambda-term. *)
let peek_key t =
  if String.length t.pending = 0 then None
  else
    let first = String.get t.pending 0 in
    let code = Char.code first in
    if code <> esc_code then
      (* Simple non-ESC key *)
      if first = '\000' then Some Refresh
      else if first = '\n' || first = '\r' then Some Enter
      else if code = tab_code then Some Tab
      else if code = backspace_code then Some Backspace
      else if code >= 1 && code <= 26 then
        let letter = Char.chr (code + 96) in
        Some (Ctrl letter)
      else Some (Char (String.make 1 first))
    else
      (* ESC sequence - need at least 3 chars for complete arrow keys *)
      let len = String.length t.pending in
      if len >= 3 && String.get t.pending 1 = '[' then
        let c = String.get t.pending 2 in
        match c with
        | '<' ->
            (* SGR mouse: ESC [ < ... - check for complete sequence *)
            let last = String.get t.pending (len - 1) in
            if last = 'M' || last = 'm' then
              (* Complete mouse sequence - parse it *)
              Some (Unknown "mouse_sgr")
            else None (* Incomplete *)
        | 'A' -> Some Up
        | 'B' -> Some Down
        | 'C' -> Some Right
        | 'D' -> Some Left
        | '3' ->
            (* Delete: ESC [ 3 ~ *)
            if len >= 4 && String.get t.pending 3 = '~' then Some Delete
            else if len >= 4 then Some (Unknown "3")
            else None (* Incomplete *)
        | _ -> Some (Unknown (String.make 1 c))
      else if len >= 3 && String.get t.pending 1 = 'O' then
        let c = String.get t.pending 2 in
        match c with
        | 'A' -> Some Up
        | 'B' -> Some Down
        | 'C' -> Some Right
        | 'D' -> Some Left
        | _ -> Some (Unknown (String.make 1 c))
      else if len = 1 then Some Escape
      else if len >= 2 then
        Some (Unknown (String.make 1 (String.get t.pending 1)))
      else None (* Incomplete ESC sequence *)

(** Bytes to consume for a given key type *)
let bytes_for_key = function
  | Up | Down | Left | Right -> 3 (* ESC [ A/B/C/D *)
  | Tab | Backspace | Enter -> 1
  | Char s -> String.length s
  | Ctrl _ -> 1
  | Delete -> 4 (* ESC [ 3 ~ *)
  | Escape -> 1
  | Refresh -> 1
  | Unknown _ -> 1
  | Mouse _ -> 0 (* Handled specially *)

(** Parse next key, consuming from buffer. *)
let parse_key t =
  if String.length t.pending = 0 then None
  else
    let first = String.get t.pending 0 in
    let code = Char.code first in
    if code <> esc_code then begin
      (* Simple non-ESC key - consume 1 byte *)
      consume t 1 ;
      if first = '\000' then Some Refresh
      else if first = '\n' || first = '\r' then Some Enter
      else if code = tab_code then Some Tab
      else if code = backspace_code then Some Backspace
      else if code >= 1 && code <= 26 then
        let letter = Char.chr (code + 96) in
        Some (Ctrl letter)
      else Some (Char (String.make 1 first))
    end
    else begin
      (* ESC sequence - gather more bytes if needed *)
      for _ = 1 to 5 do
        if String.length t.pending >= 3 then ()
        else ignore (refill t ~timeout_s:0.02)
      done ;
      let len = String.length t.pending in
      if len = 1 then begin
        consume t 1 ;
        Some Escape
      end
      else if len >= 3 && String.get t.pending 1 = '[' then begin
        let c = String.get t.pending 2 in
        match c with
        | '<' ->
            (* SGR mouse: ESC [ < btn;col;row (M|m) *)
            (* Wait for complete sequence *)
            let rec wait_for_terminator n =
              if n <= 0 then ()
              else
                let l = String.length t.pending in
                if l > 0 then
                  let last = String.get t.pending (l - 1) in
                  if last = 'M' || last = 'm' then ()
                  else begin
                    ignore (refill t ~timeout_s:0.02) ;
                    wait_for_terminator (n - 1)
                  end
                else begin
                  ignore (refill t ~timeout_s:0.02) ;
                  wait_for_terminator (n - 1)
                end
            in
            wait_for_terminator 20 ;
            let seq = t.pending in
            let seq_len = String.length seq in
            (* Find terminating M/m *)
            let term_idx =
              let rec find i =
                if i >= seq_len then seq_len - 1
                else if seq.[i] = 'M' || seq.[i] = 'm' then i
                else find (i + 1)
              in
              find 0
            in
            let chunk_len = min (term_idx + 1) seq_len in
            consume t chunk_len ;
            (* Parse ESC [ < btn;col;row (M|m) *)
            if chunk_len >= 6 then
              try
                let body = String.sub seq 3 (chunk_len - 4) in
                let lastc = seq.[chunk_len - 1] in
                match String.split_on_char ';' body with
                | [_btn; col; row] ->
                    let col = int_of_string col in
                    let row = int_of_string (String.trim row) in
                    Some (Mouse {row; col; release = lastc = 'm'})
                | _ -> Some Escape
              with _ -> Some Escape
            else Some Escape
        | 'M' ->
            (* X10 mouse: ESC [ M btn x y *)
            consume t 3 ;
            let rec ensure_bytes n timeout =
              if timeout <= 0 then false
              else if String.length t.pending >= n then true
              else begin
                ignore (refill t ~timeout_s:0.02) ;
                ensure_bytes n (timeout - 1)
              end
            in
            if ensure_bytes 3 20 then begin
              let _b = String.get t.pending 0 in
              let x = String.get t.pending 1 in
              let y = String.get t.pending 2 in
              consume t 3 ;
              let col = max 1 (Char.code x - 32) in
              let row = max 1 (Char.code y - 32) in
              Some (Mouse {row; col; release = true})
            end
            else Some Escape
        | 'A' ->
            consume t 3 ;
            Some Up
        | 'B' ->
            consume t 3 ;
            Some Down
        | 'C' ->
            consume t 3 ;
            Some Right
        | 'D' ->
            consume t 3 ;
            Some Left
        | '3' ->
            (* Delete: ESC [ 3 ~ *)
            if len >= 4 && String.get t.pending 3 = '~' then begin
              consume t 4 ;
              Some Delete
            end
            else begin
              consume t 3 ;
              Some (Unknown "3")
            end
        | _ ->
            consume t 3 ;
            Some (Unknown (String.make 1 c))
      end
      else if len >= 3 && String.get t.pending 1 = 'O' then begin
        let c = String.get t.pending 2 in
        consume t 3 ;
        match c with
        | 'A' -> Some Up
        | 'B' -> Some Down
        | 'C' -> Some Right
        | 'D' -> Some Left
        | _ -> Some (Unknown (String.make 1 c))
      end
      else if len >= 2 then begin
        consume t 2 ;
        Some (Unknown (String.make 1 (String.get t.pending 0)))
      end
      else begin
        consume t 1 ;
        Some Escape
      end
    end

(** Drain consecutive matching keys using peek-then-consume.
    Returns count of drained keys. *)
let drain_matching t key =
  let bytes = bytes_for_key key in
  if bytes = 0 then 0
  else
    let count = ref 0 in
    let rec drain () =
      ignore (refill t ~timeout_s:0.0) ;
      match peek_key t with
      | Some k when k = key ->
          if String.length t.pending >= bytes then begin
            consume t bytes ;
            incr count ;
            drain ()
          end
      | _ -> ()
    in
    drain () ;
    !count

(** Drain all Escape keys from buffer. Returns count drained. *)
let drain_esc t =
  let count = ref 0 in
  let rec drain () =
    ignore (refill t ~timeout_s:0.0) ;
    match peek_key t with
    | Some Escape ->
        consume t 1 ;
        incr count ;
        drain ()
    | _ -> ()
  in
  drain () ;
  !count

(** Convert key to string for PAGE.handle_key *)
let key_to_string = function
  | Char s -> s
  | Enter -> "Enter"
  | Tab -> "Tab"
  | Backspace -> "Backspace"
  | Escape -> "Esc"
  | Up -> "Up"
  | Down -> "Down"
  | Left -> "Left"
  | Right -> "Right"
  | Delete -> "Delete"
  | Ctrl c -> "C-" ^ String.make 1 c
  | Mouse {row; col; _} -> Printf.sprintf "Mouse:%d:%d" row col
  | Refresh -> "Refresh"
  | Unknown s -> s

(** Check if key is a navigation key (for draining) *)
let is_nav_key = function
  | Up | Down | Left | Right | Tab | Delete -> true
  | _ -> false

(** Get pending buffer length (for debugging) *)
let pending_length t = String.length t.pending

(** Clear pending buffer *)
let clear t = t.pending <- ""
