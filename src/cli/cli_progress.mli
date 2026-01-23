(******************************************************************************)
(*                                                                            *)
(* SPDX-License-Identifier: MIT                                               *)
(* Copyright (c) 2026 Nomadic Labs <contact@nomadic-labs.com>                 *)
(*                                                                            *)
(******************************************************************************)

(** Standalone progress bar rendering for CLI output.
    
    Provides multi-line progress display for tracking multiple concurrent
    or sequential downloads without requiring TUI framework dependencies.
    
    Example usage:
    {[
      let state = Cli_progress.init_display ["file1"; "file2"; "file3"] in
      let state = {state with lines_printed = Cli_progress.render_display state} in
      
      (* Update progress for file1 *)
      let state = Cli_progress.set_in_progress state 
        ~binary:"file1" ~downloaded:1000L ~total:(Some 5000L) in
      let state = {state with lines_printed = Cli_progress.render_display state} in
      
      (* Mark file1 complete, start file2 *)
      let state = Cli_progress.set_complete state ~binary:"file1" ~size:5000L in
      let state = {state with lines_printed = Cli_progress.render_display state} in
    ]} *)

(** Progress bar rendering style *)
type style = ASCII | Unicode

(** Status of a binary download *)
type binary_status =
  | Pending
  | InProgress of {downloaded : int64; total : int64 option}
  | Complete of {size : int64}

(** State of multi-binary download display *)
type display_state = {
  binaries : (string * binary_status) list;
  style : style;
  lines_printed : int;
  checksum_status : string option;
}

(** Initialize display state for a list of binaries *)
val init_display : string list -> display_state

(** Render the entire multi-line display.
    
    Uses ANSI escape sequences to move cursor up and overwrite previous output.
    Returns the number of lines printed (caller should update state.lines_printed). *)
val render_display : display_state -> int

(** Update progress for a specific binary *)
val set_in_progress :
  display_state ->
  binary:string ->
  downloaded:int64 ->
  total:int64 option ->
  display_state

(** Mark a binary as complete *)
val set_complete : display_state -> binary:string -> size:int64 -> display_state

(** Set checksum verification status message *)
val set_checksum_status : display_state -> string -> display_state

(** Clear checksum status *)
val clear_checksum_status : display_state -> display_state

(** Format file size in human-readable format *)
val format_size : int64 -> string
