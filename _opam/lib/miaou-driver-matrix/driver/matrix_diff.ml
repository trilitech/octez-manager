(******************************************************************************)
(*                                                                            *)
(* SPDX-License-Identifier: MIT                                               *)
(* Copyright (c) 2025 Nomadic Labs <contact@nomadic-labs.com>                 *)
(*                                                                            *)
(******************************************************************************)

type change =
  | MoveTo of int * int
  | SetStyle of Matrix_cell.style
  | WriteChar of string
  | WriteRun of string * int

(* Shared diff computation logic. Takes callbacks for cell access to support
   both locked and unlocked variants. Returns changes in correct order.

   Parameters:
   - get_front: get cell from front buffer at (row, col)
   - get_back: get cell from back buffer at (row, col)
   - start_row, start_col: region start (0,0 for full buffer)
   - end_row, end_col: region end (exclusive)
   - cols: total columns (for cursor wrap calculation)

   Optimizations:
   - Emits WriteRun for consecutive identical characters (e.g., "────")
   - Only emits MoveTo when cursor position changes
   - Only emits SetStyle when style changes
*)
let compute_diff ~get_front ~get_back ~start_row ~start_col ~end_row ~end_col
    ~cols =
  let changes = ref [] in

  (* Track cursor position and style. Use -1 to force MoveTo on first change. *)
  let cursor_row = ref (-1) in
  let cursor_col = ref (-1) in
  let current_style = ref Matrix_cell.default_style in

  (* Pending run of identical characters *)
  let run_char = ref "" in
  let run_count = ref 0 in

  (* Emit a change, prepending to list (we'll reverse at the end) *)
  let emit change = changes := change :: !changes in

  (* Flush any pending character run *)
  let flush_run () =
    if !run_count > 0 then begin
      if !run_count = 1 then emit (WriteChar !run_char)
      else emit (WriteRun (!run_char, !run_count)) ;
      run_count := 0 ;
      run_char := ""
    end
  in

  (* Move cursor if not already at target position *)
  let move_to row col =
    if row <> !cursor_row || col <> !cursor_col then begin
      flush_run () ;
      emit (MoveTo (row, col)) ;
      cursor_row := row ;
      cursor_col := col
    end
  in

  (* Set style if different from current *)
  let set_style style =
    if not (Matrix_cell.style_equal style !current_style) then begin
      flush_run () ;
      emit (SetStyle style) ;
      current_style := style
    end
  in

  (* Write a character - batches consecutive identical chars into runs *)
  let write_char char =
    if !run_count > 0 && char = !run_char then begin
      (* Extend current run *)
      incr run_count
    end
    else begin
      (* Start new run *)
      flush_run () ;
      run_char := char ;
      run_count := 1
    end ;
    incr cursor_col ;
    if !cursor_col >= cols then cursor_col := cols - 1
  in

  (* Scan through buffer region *)
  for row = start_row to end_row - 1 do
    for col = start_col to end_col - 1 do
      let front = get_front ~row ~col in
      let back = get_back ~row ~col in

      if not (Matrix_cell.equal front back) then begin
        move_to row col ;
        set_style back.style ;
        write_char back.char
      end
    done
  done ;

  (* Flush final run *)
  flush_run () ;

  (* Return changes in correct order *)
  List.rev !changes

(* Compute diff between front and back buffers - NOT thread-safe, use compute_atomic *)
let compute buffer =
  let rows = Matrix_buffer.rows buffer in
  let cols = Matrix_buffer.cols buffer in
  compute_diff
    ~get_front:(fun ~row ~col -> Matrix_buffer.get_front buffer ~row ~col)
    ~get_back:(fun ~row ~col -> Matrix_buffer.get_back buffer ~row ~col)
    ~start_row:0
    ~start_col:0
    ~end_row:rows
    ~end_col:cols
    ~cols

(* Compute diff atomically with buffer lock held - thread-safe for two-domain architecture *)
let compute_atomic buffer =
  Matrix_buffer.with_read_lock buffer (fun () ->
      let rows = Matrix_buffer.rows_unlocked buffer in
      let cols = Matrix_buffer.cols_unlocked buffer in
      let changes =
        compute_diff
          ~get_front:(fun ~row ~col ->
            Matrix_buffer.get_front_unlocked buffer ~row ~col)
          ~get_back:(fun ~row ~col ->
            Matrix_buffer.get_back_unlocked buffer ~row ~col)
          ~start_row:0
          ~start_col:0
          ~end_row:rows
          ~end_col:cols
          ~cols
      in
      (* Swap buffers while still holding the lock *)
      Matrix_buffer.swap_unlocked buffer ;
      changes)

(* Compute diff for a specific region *)
let compute_region buffer ~row ~col ~width ~height =
  let rows = Matrix_buffer.rows buffer in
  let cols = Matrix_buffer.cols buffer in
  let end_row = min (row + height) rows in
  let end_col = min (col + width) cols in
  compute_diff
    ~get_front:(fun ~row ~col -> Matrix_buffer.get_front buffer ~row ~col)
    ~get_back:(fun ~row ~col -> Matrix_buffer.get_back buffer ~row ~col)
    ~start_row:row
    ~start_col:col
    ~end_row
    ~end_col
    ~cols

(* Count changed cells *)
let count_changes buffer =
  let rows = Matrix_buffer.rows buffer in
  let cols = Matrix_buffer.cols buffer in
  let count = ref 0 in
  for row = 0 to rows - 1 do
    for col = 0 to cols - 1 do
      if Matrix_buffer.cell_changed buffer ~row ~col then incr count
    done
  done ;
  !count
