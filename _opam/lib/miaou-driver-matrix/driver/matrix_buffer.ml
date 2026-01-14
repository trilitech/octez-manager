(*****************************************************************************)
(*                                                                           *)
(* SPDX-License-Identifier: MIT                                              *)
(* Copyright (c) 2025 Nomadic Labs <contact@nomadic-labs.com>                *)
(*                                                                           *)
(*****************************************************************************)

[@@@warning "-69"]

type t = {
  mutable rows : int;
  mutable cols : int;
  mutable front : Matrix_cell.t array array;
  mutable back : Matrix_cell.t array array;
  mutex : Mutex.t;
  dirty : bool Atomic.t;
}

let make_grid ~rows ~cols =
  Array.init rows (fun _ -> Array.init cols (fun _ -> Matrix_cell.empty ()))

let create ~rows ~cols =
  let rows = max 1 rows in
  let cols = max 1 cols in
  {
    rows;
    cols;
    front = make_grid ~rows ~cols;
    back = make_grid ~rows ~cols;
    mutex = Mutex.create ();
    dirty = Atomic.make true;
  }

let with_lock t f =
  Mutex.lock t.mutex ;
  match f () with
  | result ->
      Mutex.unlock t.mutex ;
      result
  | exception e ->
      Mutex.unlock t.mutex ;
      raise e

let resize t ~rows ~cols =
  with_lock t (fun () ->
      let rows = max 1 rows in
      let cols = max 1 cols in
      (* On resize, create fresh empty buffers - don't copy old content.
         The UI layout changes completely on resize, so we need a full redraw.
         Leaving front buffer empty ensures diff will redraw everything. *)
      let new_front = make_grid ~rows ~cols in
      let new_back = make_grid ~rows ~cols in
      t.rows <- rows ;
      t.cols <- cols ;
      t.front <- new_front ;
      t.back <- new_back ;
      Atomic.set t.dirty true)

let rows t = t.rows

let cols t = t.cols

let size t = with_lock t (fun () -> (t.rows, t.cols))

let in_bounds t ~row ~col = row >= 0 && row < t.rows && col >= 0 && col < t.cols

let set t ~row ~col cell =
  with_lock t (fun () ->
      if in_bounds t ~row ~col then t.back.(row).(col) <- cell)

let set_from t ~row ~col cell =
  with_lock t (fun () ->
      if in_bounds t ~row ~col then begin
        t.back.(row).(col).char <- cell.Matrix_cell.char ;
        t.back.(row).(col).style <- cell.Matrix_cell.style
      end)

let get_back t ~row ~col =
  with_lock t (fun () ->
      if in_bounds t ~row ~col then t.back.(row).(col) else Matrix_cell.empty ())

let clear_back t =
  with_lock t (fun () ->
      for r = 0 to t.rows - 1 do
        for c = 0 to t.cols - 1 do
          Matrix_cell.reset t.back.(r).(c)
        done
      done)

let set_char t ~row ~col ~char ~style =
  with_lock t (fun () ->
      if in_bounds t ~row ~col then begin
        t.back.(row).(col).char <- char ;
        t.back.(row).(col).style <- style
      end)

let get_front t ~row ~col =
  if in_bounds t ~row ~col then t.front.(row).(col) else Matrix_cell.empty ()

let swap t =
  with_lock t (fun () ->
      let tmp = t.front in
      t.front <- t.back ;
      t.back <- tmp)

let cell_changed t ~row ~col =
  if in_bounds t ~row ~col then
    not (Matrix_cell.equal t.front.(row).(col) t.back.(row).(col))
  else false

let mark_all_dirty t =
  with_lock t (fun () ->
      (* Clear front buffer so all cells appear changed *)
      for r = 0 to t.rows - 1 do
        for c = 0 to t.cols - 1 do
          Matrix_cell.reset t.front.(r).(c)
        done
      done ;
      Atomic.set t.dirty true)

let mark_region_dirty t ~row_start ~row_end ~col_start ~col_end =
  with_lock t (fun () ->
      (* Clear front buffer region so those cells appear changed *)
      for r = max 0 row_start to min (t.rows - 1) row_end do
        for c = max 0 col_start to min (t.cols - 1) col_end do
          Matrix_cell.reset t.front.(r).(c)
        done
      done ;
      Atomic.set t.dirty true)

let mark_dirty t = Atomic.set t.dirty true

let is_dirty t = Atomic.get t.dirty

let clear_dirty t = Atomic.set t.dirty false

(** Batch operations record - provides unlocked accessors within with_back_buffer *)
type batch_ops = {
  clear : unit -> unit;
  set_char :
    row:int -> col:int -> char:string -> style:Matrix_cell.style -> unit;
  get : row:int -> col:int -> Matrix_cell.t;
  rows : int;
  cols : int;
}

(* Execute a function with the buffer lock held - for batch operations.
   If force_full_redraw is true, invalidates the front buffer so all cells
   appear changed - this is done atomically to avoid race conditions.
   We use invalidate (sets char to \x00) rather than reset (sets to space)
   because if the new content also has spaces, reset would compare equal
   and no change would be emitted, leaving stale content on screen. *)
let with_back_buffer ?(force_full_redraw = false) t f =
  with_lock t (fun () ->
      (* If forcing full redraw, invalidate front buffer first (while holding lock) *)
      if force_full_redraw then
        for r = 0 to t.rows - 1 do
          for c = 0 to t.cols - 1 do
            Matrix_cell.invalidate t.front.(r).(c)
          done
        done ;
      let ops =
        {
          clear =
            (fun () ->
              for r = 0 to t.rows - 1 do
                for c = 0 to t.cols - 1 do
                  Matrix_cell.reset t.back.(r).(c)
                done
              done);
          set_char =
            (fun ~row ~col ~char ~style ->
              if in_bounds t ~row ~col then begin
                t.back.(row).(col).char <- char ;
                t.back.(row).(col).style <- style
              end);
          get =
            (fun ~row ~col ->
              if in_bounds t ~row ~col then t.back.(row).(col)
              else Matrix_cell.empty ());
          rows = t.rows;
          cols = t.cols;
        }
      in
      let result = f ops in
      Atomic.set t.dirty true ;
      result)

(* Execute a read operation with the buffer lock held - for atomic diff computation *)
let with_read_lock t f = with_lock t f

(* Get front and back cells without locking - for use inside with_read_lock *)
let get_front_unlocked (t : t) ~row ~col =
  if in_bounds t ~row ~col then t.front.(row).(col) else Matrix_cell.empty ()

let get_back_unlocked (t : t) ~row ~col =
  if in_bounds t ~row ~col then t.back.(row).(col) else Matrix_cell.empty ()

let rows_unlocked (t : t) = t.rows

let cols_unlocked (t : t) = t.cols

(* Swap without locking - for use inside with_read_lock after diff *)
let swap_unlocked (t : t) =
  let tmp = t.front in
  t.front <- t.back ;
  t.back <- tmp

(* Generate SGR sequence for a style - used for dump_to_string *)
let style_to_sgr style =
  let open Matrix_cell in
  let buf = Buffer.create 32 in
  Buffer.add_string buf "\027[0" ;
  if style.bold then Buffer.add_string buf ";1" ;
  if style.dim then Buffer.add_string buf ";2" ;
  if style.underline then Buffer.add_string buf ";4" ;
  if style.reverse then Buffer.add_string buf ";7" ;
  if style.fg >= 0 && style.fg <= 255 then
    Buffer.add_string buf (Printf.sprintf ";38;5;%d" style.fg) ;
  if style.bg >= 0 && style.bg <= 255 then
    Buffer.add_string buf (Printf.sprintf ";48;5;%d" style.bg) ;
  Buffer.add_char buf 'm' ;
  Buffer.contents buf

(* Dump front buffer to string with ANSI formatting - for preserving screen on exit *)
let dump_to_string t =
  with_lock t (fun () ->
      let buf = Buffer.create (t.rows * t.cols * 2) in
      let current_style = ref Matrix_cell.default_style in
      for row = 0 to t.rows - 1 do
        for col = 0 to t.cols - 1 do
          let cell = t.front.(row).(col) in
          (* Update style if changed *)
          if not (Matrix_cell.style_equal cell.style !current_style) then begin
            if Matrix_cell.style_equal cell.style Matrix_cell.default_style then
              Buffer.add_string buf "\027[0m"
            else Buffer.add_string buf (style_to_sgr cell.style) ;
            current_style := cell.style
          end ;
          Buffer.add_string buf cell.char
        done ;
        (* Add newline after each row except the last *)
        if row < t.rows - 1 then Buffer.add_char buf '\n'
      done ;
      (* Reset style at end *)
      Buffer.add_string buf "\027[0m" ;
      Buffer.contents buf)
