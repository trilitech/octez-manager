(*****************************************************************************)
(*                                                                           *)
(* SPDX-License-Identifier: MIT                                              *)
(* Copyright (c) 2025 Nomadic Labs <contact@nomadic-labs.com>                *)
(*                                                                           *)
(*****************************************************************************)

(** Diff algorithm for the Matrix driver.

    Compares front and back buffers to compute minimal changes needed
    to update the terminal display. Optimizes for:
    - Skipping unchanged cells
    - Minimizing cursor movements
    - Batching consecutive characters
    - Minimizing style changes
*)

(** A single change operation. *)
type change =
  | MoveTo of int * int  (** Move cursor to (row, col), 0-indexed *)
  | SetStyle of Matrix_cell.style  (** Change current text style *)
  | WriteChar of string  (** Write a single character *)
  | WriteRun of string * int  (** Write character repeated N times *)

(** Compute diff between front (displayed) and back (new) buffers.
    Returns list of changes to transform front into back.
    NOT thread-safe - use [compute_atomic] for two-domain architecture. *)
val compute : Matrix_buffer.t -> change list

(** Compute diff atomically with buffer lock held.
    Thread-safe for two-domain architecture. Also swaps buffers
    while holding the lock to prevent torn reads.
    Use this from the render domain. *)
val compute_atomic : Matrix_buffer.t -> change list

(** Compute diff for a specific region of the buffer. *)
val compute_region :
  Matrix_buffer.t ->
  row:int ->
  col:int ->
  width:int ->
  height:int ->
  change list

(** Count number of cells that differ between front and back. *)
val count_changes : Matrix_buffer.t -> int
