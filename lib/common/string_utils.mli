(******************************************************************************)
(*                                                                            *)
(* SPDX-License-Identifier: MIT                                               *)
(* Copyright (c) 2026 Nomadic Labs <contact@nomadic-labs.com>                 *)
(*                                                                            *)
(******************************************************************************)

(** Formatting, sleeping, editor integration, and string helpers. *)

(** Sleep for up to [seconds], checking [stop_flag] every 0.5 s.
    Returns early when [stop_flag] becomes [true], allowing background
    domains to shut down promptly. *)
val interruptible_sleep : bool Atomic.t -> float -> unit

(** Map Octez exit codes to human-readable descriptions.
    @see <https://octez.tezos.com/docs/user/exits.html> *)
val octez_exit_code_description : int -> string

(** {1 Editor Integration} *)

(** Get the user's preferred editor from environment variables.
    Tries [$VISUAL], [$EDITOR], then falls back to [sensible-editor]
    or [vi]. *)
val get_editor : unit -> string

(** Open a file in the user's preferred editor.
    Blocks until the editor exits.

    @param file_path Path to the file to edit
    @return [Ok ()] if editor exited successfully, [Error] otherwise *)
val open_in_editor : string -> (unit, [> `Msg of string]) result

(** {1 String Utilities} *)

(** Check whether [needle] is a substring of [haystack]. *)
val string_contains : needle:string -> string -> bool

(** Strip non-printable and non-ASCII bytes, keeping only printable ASCII
    (0x20-0x7E). Useful for sanitizing clipboard input from web UIs that
    inject U+00A0 NBSP, U+200B ZWSP, U+FEFF BOM and similar. *)
val strip_non_ascii : string -> string

(** {1 Timestamp Utilities} *)

(** Format the current local time as ["YYYY-MM-DD HH:MM:SS"]. *)
val now : unit -> string

(** {1 Size Formatting} *)

(** Format a byte count as a human-readable string using integer
    division (truncating).  Produces e.g. ["3 GB"], ["450 MB"],
    ["12 KB"], ["800 bytes"]. *)
val format_size : int64 -> string

(** Format a byte count as a compact human-readable string with float
    precision.  Produces e.g. ["1.2G"], ["450M"], ["12K"], ["800B"].
    Handles values up to terabytes. *)
val format_bytes : int64 -> string

(** Remove surrounding quotes from a string.
    Handles backslash-escaped double quotes, regular double quotes,
    and single quotes.  Returns the string unchanged if not quoted. *)
val unquote : string -> string

(** Format a byte count as a human-readable string with float precision
    and spaced units.  Produces e.g. ["1.5 GB"], ["100 MB"],
    ["512 bytes"].  Best for disk-space messages shown to the user. *)
val format_size_float : int64 -> string
