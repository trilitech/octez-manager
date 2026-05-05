(******************************************************************************)
(*                                                                            *)
(* SPDX-License-Identifier: MIT                                               *)
(* Copyright (c) 2026 Nomadic Labs <contact@nomadic-labs.com>                 *)
(*                                                                            *)
(******************************************************************************)

(** Shell command execution utilities.

    All process execution functions support pluggable hooks for
    non-blocking Eio-based execution in TUI mode. When hooks are not
    set (CLI mode), blocking Unix implementations are used. *)

(** {1 Hook registration}

    The TUI calls these at startup to inject Eio-based implementations.
    Once set, all subsequent calls to [run], [run_out], etc. use the
    Eio path instead of blocking Unix I/O. *)

(** Override [run] with a non-blocking implementation. *)
val set_run_hook :
  (quiet:bool ->
  ?on_log:(string -> unit) ->
  string list ->
  (unit, [`Msg of string]) result) ->
  unit

(** Override [run_out] with a non-blocking implementation. *)
val set_run_out_hook :
  (string list -> (string, [`Msg of string]) result) -> unit

(** Override [run_out_silent] with a non-blocking implementation. *)
val set_run_out_silent_hook :
  (string list -> (string, [`Msg of string]) result) -> unit

(** Override [run_streaming] with a non-blocking implementation. *)
val set_run_streaming_hook :
  (on_log:(string -> unit) -> string list -> (unit, [`Msg of string]) result) ->
  unit

(** {1 Command execution} *)

(** Append a line to [/tmp/octez_manager_cmds.log] for debugging. *)
val append_debug_log : string -> unit

(** Shell-quote a string (POSIX single-quote style). *)
val sh_quote : string -> string

(** Join an argv list into a single shell command string. *)
val cmd_to_string : string list -> string

(** Run a command.  When [quiet] is true, stdout/stderr are captured
    rather than streamed to the terminal.
    @param on_log optional per-line callback *)
val run :
  ?quiet:bool ->
  ?on_log:(string -> unit) ->
  string list ->
  (unit, [> `Msg of string]) result

(** [run ~quiet:true]. *)
val run_silent :
  ?on_log:(string -> unit) -> string list -> (unit, [> `Msg of string]) result

(** [run ~quiet:false]. *)
val run_verbose :
  ?on_log:(string -> unit) -> string list -> (unit, [> `Msg of string]) result

(** Run command with streaming output.  Reads stdout and stderr
    concurrently using [Unix.select], ensuring output is captured as
    produced.  Use for long-running commands where real-time output is
    needed. *)
val run_streaming :
  on_log:(string -> unit) -> string list -> (unit, [> `Msg of string]) result

(** Run a command and return its stdout (trimmed). *)
val run_out : string list -> (string, [> `Msg of string]) result

(** Like {!run_out} but kills the process after [timeout] seconds.
    Returns a timeout error if the process does not complete in time. *)
val run_out_with_timeout :
  timeout:float -> string list -> (string, [> `Msg of string]) result

(** Override [run_out_with_timeout] with a non-blocking implementation. *)
val set_run_out_with_timeout_hook :
  (timeout:float -> string list -> (string, [`Msg of string]) result) -> unit

(** Like {!run_out_with_timeout} but returns the combined stdout and stderr
    on success (with stderr appended after a newline). Useful for parsing
    octez-client output where the operation hash may land on either stream. *)
val run_out_with_timeout_combined :
  timeout:float -> string list -> (string, [> `Msg of string]) result

(** Override [run_out_with_timeout_combined] with a non-blocking implementation. *)
val set_run_out_with_timeout_combined_hook :
  (timeout:float -> string list -> (string, [`Msg of string]) result) -> unit

(** Clear the {!run_out_with_timeout_combined} override. Intended for tests that
    temporarily replace command execution. *)
val reset_run_out_with_timeout_combined_hook : unit -> unit

(** Like {!run_out} but captures stderr to prevent it from leaking to
    the terminal.  Returns only stdout.  Useful for HTTP operations in
    the TUI where curl errors would corrupt the display. *)
val run_out_silent : string list -> (string, [> `Msg of string]) result

(** Run a command as another user via [su].  Falls back to direct
    execution when not root or when [user] matches the current user. *)
val run_as :
  ?quiet:bool ->
  ?on_log:(string -> unit) ->
  user:string ->
  string list ->
  (unit, [> `Msg of string]) result
