(******************************************************************************)
(*                                                                            *)
(* SPDX-License-Identifier: MIT                                               *)
(* Copyright (c) 2026 Nomadic Labs <contact@nomadic-labs.com>                 *)
(*                                                                            *)
(******************************************************************************)

(** Scan running processes for Octez binaries not managed by systemd *)

type process_info = {
  pid : int;
  cmdline : string;
  binary_path : string option;
  binary_realpath : string option;
      (** Resolved absolute path from /proc/PID/exe *)
  parent_pid : int option;
  user : string option;
}

(** Scan all running processes and return those running Octez binaries.
    Only processes where the executable (first token) is an Octez binary are included. *)
val scan_octez_processes : unit -> process_info list

(** Check if a PID is managed by systemd (via cgroup) *)
val is_systemd_managed : int -> bool

(** Get Octez processes that are NOT managed by systemd.
    These are candidates for "standalone" external services. *)
val get_standalone_processes : unit -> process_info list

(** {1 Testing} *)

(** Exposed for unit testing. Do not use outside tests. *)
module For_tests : sig
  val extract_binary_path : string -> string option

  val is_octez_binary : string -> bool

  val contains_substring : string -> string -> bool
end
